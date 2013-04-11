{-# LANGUAGE OverloadedStrings #-}

-- Primary library for fetching remote robots.txt files

module Network.HTTP.Robots where

import qualified Blaze.ByteString.Builder as Builder (fromByteString, toByteString)
import           Data.Maybe
import           Data.ByteString.Char8
import           Data.Monoid (mempty, mappend)
import           Network.Http.Client
import qualified System.IO.Streams as Streams
import           System.IO.Streams (InputStream)

type RobotResponse = (StatusCode, (ByteString, ByteString), ByteString)

-- | @open@ Open the given robots.txt URL returning the response
-- headers and body and following any 301, 302, or 303's
open :: ByteString -> IO RobotResponse
open url = get url robotHandler

-- | @robotHandler@ Handle a response. Concactenate the body and
-- return the Response status and headers along with it.
robotHandler :: Response -> InputStream ByteString -> IO RobotResponse
robotHandler p i1 = do
    i2 <- Streams.map Builder.fromByteString i1
    x <- Streams.fold mappend mempty i2
    return (s, (hdr "Cache-Control", hdr "Expires"), Builder.toByteString x)
    where
        s = getStatusCode p
        
        -- easy header getter
        hdr x = fromMaybe "" $ getHeader p x
