{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}

-- |
-- Command line utility, this is not meant to be used as a library.
-- 
-- To use as a library see the README or use this as an example of how
-- to combine the caching backend, request system, and parser/rule
-- checker.

module Main where

import           Control.Applicative
import           Control.Monad.Error
import           Control.Exception.Base    (bracket)
import           Data.Grob.Types
import           Data.Grob.Acid
import           Data.Grob.Attoparsec
import           Data.ConfigFile as C
import           Data.Acid                 (AcidState, openLocalStateFrom)
import           Data.Acid.Local           (createCheckpointAndClose)
import           Data.Acid.Advanced        (update', query')
import           Data.ByteString.Char8 as CB hiding (filter, null, any)
import           Data.Time                 (UTCTime, getCurrentTime)
import           Data.Maybe
import           Data.List                 (stripPrefix)
import           Data.List.Utils           (endswith)
import           Network.HTTP.Robots
import           Network.URI as U hiding (path)
import           System.Log.Handler.Color
import           System.Log.Logger
import           System.Console.CmdArgs
import           System.Exit
import           System.Posix.Env          (getEnv)
import           OpenSSL

rargs :: Grob
rargs = Args
    {
        argrobot    = def &= argPos 0 &= typ "ROBOT.TXT",
        argagent    = def &= argPos 1 &= typ "USERAGENT",
        argresource = def &= argPos 2 &= typ "URI",
        argnocache  = def &= explicit &= name "n" &= help "Override HTTP cache headers to not cache robots.txt",
        argallowed  = def &= explicit &= name "a" &= help "Explicitly check if allowed",
        config   = "~/.grobrc" &= typFile &= groupname "Options" &= help "Specify config file to use"
    } &=
    verbosity &=
    help    "Parser and rule checker for robots.txt's" &=
    helpArg [name "h"] &=
    summary "grob v0.1.0" &=
    noAtExpand &=
    details ["grob is a robots.txt request, parser, and rule checker library and binary.",
             "",
             "Default configuration file is in \"${HOME}/.grobrc\", use --config=[File] to specify a custom one."]

-- | @main@ begin every invocation in a `withOpenSSL` computation in
-- the event we try to request a robots.txt resource behind
-- HTTPS. Parse the cmdargs and pass into `parseConfig` to generate
-- initial application state inside of `grob/1`.
main :: IO Bool
main = withOpenSSL $ cmdArgs rargs >>= grob . parseConfig

-- | @grob@ builds and executes a given cmdarg/config session.
-- 
-- This function is responsible for resolving (if we can) the user's
-- HOME for the datadir (unless it's been set otherwise).
grob :: IO Settings -> IO Bool
grob settings = do
    sets <- settings
    home <- getEnv "HOME"
    
    debugM "Console" "Initializing AcidState"
    
    let ddir = homePath (datadir sets) home

    returnCode <- if endswith "robots.txt" (robot sets) then runWithCache (nocache sets) sets ddir else return ExitSuccess
    
    -- proper exit code termination
    exitWith returnCode

-- | @runRobot@ run with cache backend (False) or raw (True).
runWithCache :: Bool -> Settings -> String -> IO ExitCode
runWithCache True sets _ = do
    (status, (_hcache, _hexp), body)  <- open (CB.pack $ robot sets)
    return ExitSuccess
runWithCache False sets ddir =
    bracket (openLocalStateFrom ddir initialBotState)
            createCheckpointAndClose
            (middleState sets)

middleState :: Settings -> AcidState Bots -> IO ExitCode
middleState sets db = do
    curt <- liftIO getCurrentTime
    let uri = robot sets
        robotUri = pack uri
        directive = if allowed sets then "allow" else "disallow"
    qBot <- query' db (RobotById $ RobotId (sha1 robotUri))
    
    -- need to send Last-Modified so server can either tell us if it's
    -- modified or not
    
    resp <- open robotUri
    
    tree <- dbOrParse db qBot curt resp robotUri
    
    quiet <- isNormal
    
    print tree
    let f = directiveUA directive (agent sets) (resource sets) tree
        v = if allowed sets then not f else f
    
    if v
    then return (ExitFailure 1)
    else do
        formatUri quiet (fromJust $ U.parseURI uri) uri (resource sets)
        return ExitSuccess

formatUri :: Bool -> U.URI -> String -> String -> IO ()
formatUri False _ r _ = return ()
formatUri True puri _ pth  = Prelude.putStrLn $ Prelude.concat [s, "//", n, p, pth]
    where s = uriScheme puri
          a = fromJust $ uriAuthority puri
          n = uriRegName a
          p = uriPort a

-- | @filterUA@ find the user agent
filterUA :: String -> [RuleSet] -> Maybe RuleSet
filterUA ua ruleset = filt ua ruleset <|> filt "*" ruleset
    where filt u = listToMaybe . filter (\x-> unUA (userAgent x) == pack u)

-- | @directiveUA@ if no user agent then False otherwise get their
-- rule set and check against that
directiveUA :: ByteString -> String -> String -> [RuleSet] -> Bool
directiveUA dr ua path ruleset = maybe False (nany . rules) (filterUA ua ruleset)
    where nany ps = any (\(_,y) -> checkPrefix dr path y) (filter (\(x,_) -> x==dr) ps)

-- | @checkPrefix@ 
checkPrefix :: ByteString -> String -> ByteString -> Bool
checkPrefix "disallow" path res = ("/"==y) || (path == y) || isJust (stripPrefix y path)
    where y = CB.unpack res
checkPrefix "allow" path res = path == y
    where y = CB.unpack res
checkPrefix _ _ _ = True

-- | If it's a 404 we can't cache it and we should just assume it's a
-- free for all; if it's a 304 then we just want to return the parse
-- tree.
dbOrParse _ _ _ (404,_,_) _    = return []
dbOrParse _ qBot _ (304,_,_) _ = return . parseTree $ fromJust qBot
dbOrParse db qBot curt (stat, (hcache, hexp), body) uri =
    case qBot of
        Nothing -> do
            -- do initial parsing in here
            let tree = doParse body
            nBot <- update' db (NewRobot Robot {
                    robotId = RobotId $ sha1 uri,
                    url = unpack uri,
                    ttl = curt,
                    date = curt,
                    parseTree = tree
                })
            return tree
        Just p ->
            return (parseTree p)

-- | @homePath@ given a path and a path from getEnv/1, determine if we
-- want the users home directory and if so replace the leading ~ with
-- the user's HOME environment variable. If HOME is Nothing OR no
-- leading ~ in path then simply return path.
homePath :: String -> Maybe String -> String
homePath ('~':xs) home = fromMaybe "~" home ++ xs
homePath p _ = p

-- | @parseConfig@ given an cmdarg record, try to parse a config file
-- defined either by default or by the user into a settings record.
parseConfig :: Grob -> IO Settings
parseConfig argvs = do
    
    -- these are being "overridden" by the verbosity/quiet arg
    whenNormal $ updateGlobalLogger "Console" (setLevel ERROR)
    whenLoud $ updateGlobalLogger "Console" (setLevel DEBUG)
    
    -- activate color logging
    updateGlobalLogger rootLoggerName (addHandler colorHandler)
    
    debugM "Console" $ "Running Grob with " ++ config argvs
    
    home <- getEnv "HOME"
    
    -- parse the config file
    cnf <- runErrorT $ do
        cp          <- join $ liftIO $ readfile emptyCP (homePath (config argvs) home)
        datadirv    <- C.get cp "CACHING" "datadir"
        loglevelv   <- C.get cp "LOGGING" "loglevel"
        
        -- build and return a settings record
        return Settings {
                    robot    = argrobot argvs,
                    agent    = argagent argvs,
                    resource = argresource argvs,
                    nocache  = argnocache argvs,
                    allowed  = argallowed argvs,
                    datadir  = datadirv,
                    loglevel = loglevelv
                }
    handleConfig cnf

-- | @handleConfig@ log and exit with any parsing errors or return the
-- new settings record.
handleConfig :: Either (CPErrorData, String) Settings -> IO Settings
handleConfig (Left err) = do
    -- log the error and exit the program
    criticalM "Console" $ show err
    criticalM "Console" "exiting..."
    exitWith (ExitFailure 1)
handleConfig (Right conf) = do
    quiet <- isNormal
    setLL quiet conf
    
    debugM "Console" "Configuration parsed"
    debugM "Console" $ "Setting loglevel to " ++ show (loglevel conf)
    
    return conf

setLL :: Bool -> Settings -> IO ()
setLL False _ = return ()
setLL True conf = updateGlobalLogger "Console" (setLevel $ loglevel conf)
