{-# LANGUAGE CPP, TypeFamilies, OverloadedStrings, DeriveDataTypeable, FlexibleContexts,
    GeneralizedNewtypeDeriving, RecordWildCards, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Data.Grob.Types where

import           Crypto.Hash
import           Data.ByteString.Char8     (ByteString)
import           Data.SafeCopy
import           Data.Time                 (UTCTime(..))
import           Data.IxSet                ( Indexable(..), IxSet, ixFun, ixSet )
import           System.Console.CmdArgs    (Typeable, Data)
import           System.Log.Logger

type HostName = String
type LogLevel = Priority
type URI = String

sha1 :: ByteString -> String
sha1 a = show (hash a :: Digest SHA1)

-- record for cmdargs
data Grob = Args
    {
        argrobot    :: URI,
        argagent    :: String,
        argresource :: URI,
        argnocache  :: Bool,
        argallowed  :: Bool,
        config      :: URI
    } deriving (Data,Typeable,Show,Eq)

-- record for post-config parsed application settings
data Settings = Settings
    {
        robot     :: URI,
        agent     :: String,
        resource  :: URI,
        nocache   :: Bool,
        allowed   :: Bool,
        datadir   :: String,
        loglevel  :: LogLevel
    } deriving (Show, Eq)

-- | AcidState IxSet and AttoParsec types

type RuleMap = [(ByteString, ByteString)]


-- newtype for indexable id
newtype RobotId = RobotId { unRobotId :: String }
    deriving (Eq, Ord, Show, Read, Data, Typeable, SafeCopy)

-- newtype for indexable ua
newtype UserAgent = UserAgent { unUA :: ByteString }
    deriving (Eq, Ord, Show, Read, Data, Typeable, SafeCopy)

data RuleSet = RuleSet
    { userAgent :: UserAgent,
      rules     :: RuleMap }
     deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''RuleSet)

-- | record indexed by RobotId holding a parse tree
data Robot = Robot
    { robotId :: RobotId,
      url     :: URI,
      ttl     :: UTCTime,
      date    :: UTCTime,
      parseTree :: [RuleSet] }
     deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Robot)

instance Indexable Robot where
     empty = ixSet [ ixFun $ \bp -> [ robotId bp ] ]

-- | record for AcidState storing robots.txt info + parsed ruleset
data Bots = Bots
    {
        robots :: IxSet Robot
    } deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Bots)
