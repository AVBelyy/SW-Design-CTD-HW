{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Lib (Hour(..), getTweetsHours, Twitterish(..), RealTwitter(..), MockedTwitter(..)) where

import           Control.Monad          (guard, replicateM)
import           Control.Monad.Random   (getRandomR)
import           Data.Aeson.Types       (FromJSON)
import           Data.Default.Class     (Default)
import           Data.Map               (fromList, (!))
import           Data.Text              (pack)
import           Data.Time.Clock        (UTCTime (..), utctDayTime)
import           Data.Time.LocalTime    (TimeOfDay (..), timeOfDayToTime, timeToTimeOfDay,
                                         todHour)
import           Web.Authenticate.OAuth (Credential)
import           Web.Twitter.Conduit    hiding (Response)
import           Web.Twitter.Types      (SearchResult (..), Status (..))

-- Real Twitter auth data

tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey = "Q4Pu2F7r2Nxf8xxNSezKWmBZT"
    , oauthConsumerSecret = "0FuXUhVBnLBhPe47nQspJecSID6p8Q8IIpuQGkIR8yRqz39Vxr"
    }

cred :: Credential
cred = Credential [ ("oauth_token", "24385828-qivW02fJi4TzqKxQELyfYeJ4kO0YRcmr5OjEYGBp6")
                  , ("oauth_token_secret", "Mj3x8ETWjuVREeiPLTloNkSD6lgKjaoLgVXyni5IX3SPy")]

twInfo :: TWInfo
twInfo = def { twToken = def { twOAuth = tokens, twCredential = cred }
             , twProxy = Nothing }

searchUrl = "https://api.twitter.com/1.1/search/tweets.json"

-- End of auth data


-- Auxiliary data structures

data Hour = Hour Int deriving Show

type Response = SearchResult [Status]

instance Default Status where
    def =  Status {
            statusContributors = Nothing,
            statusCoordinates = Nothing,
            statusCreatedAt = undefined,
            statusCurrentUserRetweet = Nothing,
            statusEntities = Nothing,
            statusExtendedEntities = Nothing,
            statusFavoriteCount = 0,
            statusFavorited = Nothing,
            statusFilterLevel = Nothing,
            statusId = 0,
            statusInReplyToScreenName = Nothing,
            statusInReplyToStatusId = Nothing,
            statusInReplyToUserId = Nothing,
            statusLang = Nothing,
            statusPlace = Nothing,
            statusPossiblySensitive = Nothing,
            statusScopes = Nothing,
            statusQuotedStatusId = Nothing,
            statusQuotedStatus = Nothing,
            statusRetweetCount = 0,
            statusRetweeted = Nothing,
            statusRetweetedStatus = Nothing,
            statusSource = "",
            statusText = "",
            statusTruncated = False,
            statusUser = undefined,
            statusWithheldCopyright = Nothing,
            statusWithheldInCountries = Nothing,
            statusWithheldScope = Nothing
        }

-- End of data structures


-- "Mockups" mainly are defined here

data RealTwitter = RealTwitter
data MockedTwitter = MockedTwitter

class Twitterish a where
    callAPI :: a -> APIRequest apiName Response -> IO Response

instance Twitterish RealTwitter where
    callAPI _ query = do
        mgr <- newManager tlsManagerSettings
        call twInfo mgr query

instance Twitterish MockedTwitter where
    callAPI _ APIRequestGet{..} | _url == searchUrl =
        return $ SearchResult (setTweetHour <$> [0..n-1]) undefined
            where n = min (fromIntegral count) 24
                  PVInteger count = fromList _params ! "count"
    callAPI _ _ = fail "not implemented here"

-- End of "mockups"


-- Helper functions

setTweetHour :: Int -> Status
setTweetHour h = def { statusCreatedAt = UTCTime undefined $ timeOfDayToTime $ TimeOfDay h 0 0 }

getTweetHour :: Status -> Int
getTweetHour = todHour . timeToTimeOfDay . utctDayTime . statusCreatedAt

searchByDate :: String -> APIRequest SearchTweets (SearchResult [Status])
searchByDate q = APIRequestGet "https://api.twitter.com/1.1/search/tweets.json"
                 [ ("q", PVString (pack q)), ("until", PVString "2016-10-28")
                 , ("count", PVInteger 100)]

-- End functions


getTweetsHours :: Twitterish t => t -> String -> Hour -> IO [Int]
getTweetsHours t q (Hour n) = do
    -- Trivial asserts
    guard (n > 0 && n <= 24)
    guard (not $ null q)

    res <- callAPI t (searchByDate q)
    let hours = getTweetHour <$> searchResultStatuses res
    return $ (\i -> length $ filter (== i) hours) <$> [0..n-1]
