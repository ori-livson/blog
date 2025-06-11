module GitHub (loadCommentsFromIssue, Comment (..), User (..), issuesApiUrl) where

import Config (issuesApiUrl)
import Data.Aeson (FromJSON (parseJSON), decode, withObject, (.:))
import Data.Aeson.Types (Parser)
-- import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (pack)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import LucidUtils (HTML, markdownToLucid)
import Network.HTTP.Conduit (parseRequest)
import Network.HTTP.Simple
  ( getResponseBody,
    httpLBS,
    setRequestHeaders,
  )

-- Define a data type to represent each object in the list
data PreComment = PreComment
  { htmlUrl_ :: String,
    user_ :: User,
    createdAt_ :: UTCTime,
    body_ :: String
  }
  deriving (Show)

data Comment = Comment
  { htmlUrl :: String,
    user :: User,
    createdAt :: UTCTime,
    body :: HTML
  }
  deriving (Show)

resolveComment :: PreComment -> IO Comment
resolveComment PreComment {htmlUrl_, user_, createdAt_, body_} = do
  resolvedBody <- markdownToLucid . pack $ body_
  return
    Comment
      { htmlUrl = htmlUrl_,
        user = user_,
        createdAt = createdAt_,
        body = resolvedBody
      }

-- Define a data type for the 'user' field
data User = User
  { login :: String,
    userHtmlUrl :: String,
    avatarUrl :: String
  }
  deriving (Show)

-- Define FromJSON instances for the User and Issue types
instance FromJSON User where
  parseJSON = withObject "User" $ \v ->
    User
      <$> v
        .: "login"
      <*> v
        .: "html_url"
      <*> v
        .: "avatar_url"

instance FromJSON PreComment where
  parseJSON = withObject "PreComment" $ \v ->
    PreComment
      <$> v
        .: "html_url"
      <*> v
        .: "user"
      <*> (parseUTCTime =<< v .: "created_at")
      <*> v
        .: "body"

parseUTCTime :: String -> Parser UTCTime
parseUTCTime timeStr = case parseTimeM True defaultTimeLocale "%FT%XZ" timeStr of
  Just time -> return time
  Nothing -> fail "Invalid time format"

{-- To Debug Add:

import qualified Data.ByteString.Lazy.Char8 as BL
-- putStrLn $ BL.unpack responseBody
 --}

fetchAndParseJSON :: String -> IO (Maybe [Comment])
fetchAndParseJSON url = do
  request <- parseRequest url
  let headers = [("Accept", "application/json"), ("User-Agent", "ori-livson"), ("X-Github-Media-Type:", "github.v3")]
  let requestWithHeader = setRequestHeaders headers request
  response <- httpLBS requestWithHeader
  let responseBody = getResponseBody response
  -- BL.putStrLn responseBody
  let maybeParsedData = decode responseBody :: Maybe [PreComment]
  mapM (traverse resolveComment) maybeParsedData

loadCommentsFromIssue :: Int -> IO [Comment]
loadCommentsFromIssue issueId = do
  let apiUrl = issuesApiUrl ++ show issueId ++ "/comments"
  maybeIssues <- fetchAndParseJSON apiUrl
  case maybeIssues of
    Just issues -> return issues
    Nothing -> error "Failed retrieve comments"