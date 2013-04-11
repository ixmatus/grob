{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Data.Grob.Attoparsec where 

import           Prelude hiding (takeWhile)
import           Control.Applicative hiding (many)
import           Data.Char
import           Control.Monad
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Char8 as AC
import           Data.Array.Unboxed
import           Data.ByteString as B hiding (takeWhile)
import qualified Data.ByteString.Internal as BI
import           Data.Word (Word8)
import           Data.Grob.Types

isNotEnd = not . AC.isEndOfLine

-- notInClass is not working as expected and character spans aren't
-- working as expected either but this appears to work (I hate it so
-- much though)
skipSymb = satisfy (inClass "#/[]@!$&'()*%+\",;=.~_-BCEFGHIJKLMNOPQRSTVWXYZbcefghijklmnopqrstvwxyz0-9") *> skipWhile isNotEnd

-- | Catch all character matching, basically
matchALL :: Word8 -> Bool
matchALL = inClass ":/?#[]@!$&'()*%+,;=.~a-zA-Z0-9 _-"

-- | @doParse@ Run the parser, complete the partial if the end of the stream has
-- a newline with an empty string
doParse :: ByteString -> [RuleSet]
doParse cont =
    case parse (many1 parseUABlock) cont of
        Done _ set -> set
        Partial f -> handlePartial (f B.empty)
        Fail {} -> []

-- | @handlePartial@ Handle a partial with empty string by simply
-- returning the last completion
handlePartial :: forall t a. IResult t [a] -> [a]
handlePartial (Done _ r) = r
handlePartial (Fail {})  = []

-- | @parseUABlock@ Parse a user-agent and rules block
parseUABlock = do
    ua    <- parseUACol *> takeWhile isNotEnd
    
    rulez <- many1 parseRules
    
    return RuleSet { userAgent = UserAgent ua,
                     rules = rulez }

-- | @matchUACol@ Parse the UA column and value taking into account
-- possible whitespace craziness
parseUACol = AC.skipSpace *> skipMany skipSymb *> AC.skipSpace
          *> AC.stringCI "User-Agent"
          <* AC.skipSpace
          *> AC.char8 ':'
          *> AC.skipSpace

-- | @parseRules@ Parse the directives row
parseRules = skipMany skipSymb *> ((,) <$> parseTransLower
             <*> (takeWhile1 matchALL <* many1 AC.endOfLine))

parseTransLower = do
    res <- parseDirectives <* AC.skipSpace
    return (lowercase res)

ctypeLower = listArray (0,255) (Prelude.map (BI.c2w . toLower) ['\0'..'\255']) :: UArray Word8 Word8
lowercase = B.map (\x -> ctypeLower!x)

directives = AC.stringCI "Disallow" <|> AC.stringCI "Allow"

-- | @parseDirectives@ Parse the directive column and any possibly
-- funny whitespace
parseDirectives = AC.skipSpace
                  *> directives -- <|> AC.stringCI "Crawl-delay" <|> AC.stringCI "Sitemap")
                  <* AC.skipSpace
                  <* AC.char8 ':'
