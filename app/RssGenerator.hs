module RssGenerator (rss) where

import Config (selfAuthor, selfDescription, selfTitle, selfUrl)
import Data.Maybe (fromJust, fromMaybe)
import Data.Time (Day, UTCTime (..))
import Network.URI (parseURI)
import Templates (Post (..), Posts, sortPosts)
import Text.RSS
  ( Item,
    ItemElem (Author, Category, Description, Guid, Link, PubDate, Title),
    RSS (RSS),
    rssToXML,
    showXML,
  )

rss :: Posts -> String
rss posts =
  showXML . rssToXML $
    RSS
      selfTitle -- Channel title
      (fromJust (parseURI selfUrl)) -- Channel link (will crash if invalid)
      selfDescription -- Channel description
      [] -- Channel extensions
      (map item $ sortPosts posts) -- Channel items

item :: (String, Post) -> Item
item (resource, post) =
  [ Title $ title post,
    Link (fromJust . parseURI $ itemUri),
    Guid True itemUri,
    Description (fromMaybe "" $ subtitle post),
    Author selfAuthor,
    Category Nothing (head $ tags post), -- all posts should have at least 1 tag so this should crash if not
    PubDate (dayToUTCTime $ date post)
  ]
  where
    itemUri = selfUrl ++ "/posts/" ++ resource

dayToUTCTime :: Day -> UTCTime
dayToUTCTime day = UTCTime {utctDay = day, utctDayTime = 0}