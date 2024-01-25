{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Templates (homeHtml, tagMatchHtml, contactHtml, postHtml, commentToHtml, allTagsHtml) where

import Data.List (nub, sort, sortBy)
import Data.Map (toList)
import qualified Data.Map as M
import qualified Data.Map as Map
import Data.Text (Text, pack)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GitHub
  ( Comment (Comment, body, createdAt, user),
    User (User, login, userHtmlUrl),
    issuesUrl,
  )
import Lucid
import Lucid.Svg (d_, fill_, fill_rule_, path_, stroke_, viewBox_)
import Page (LucidHtml, Page (..), Pages)

---------------------------------------------------------------------------------------------------
-- /Home
---------------------------------------------------------------------------------------------------

homeHtml :: Pages -> LucidHtml
homeHtml pages = do
  standardHead "Ori Livson's Blog"
  standardBody $ do
    standardBanner
    postEntries pages

---------------------------------------------------------------------------------------------------
-- /Tags
---------------------------------------------------------------------------------------------------

allTagsHtml :: Pages -> LucidHtml
allTagsHtml pages = do
  standardHead "All Tags"
  standardBody $ do
    standardBanner
    standardTitle "All Tags" Nothing
    ul_ $ mapM_ (\s -> li_ $ url ("/tags/" ++ s) s) distinctTags
  where
    distinctTags = sort . nub . concatMap tags $ pages

tagMatchHtml :: Pages -> String -> LucidHtml
tagMatchHtml pages value = do
  standardHead value
  standardBody $ do
    standardBanner
    standardTitle value Nothing
    postEntries $ Map.filter matchesTag pages
  where
    matchesTag Page {tags} = value `elem` tags

---------------------------------------------------------------------------------------------------
-- /Posts
---------------------------------------------------------------------------------------------------

postHtml :: Pages -> String -> LucidHtml
postHtml pages endpoint = do
  case M.lookup endpoint pages of
    Just page -> assemblePost page
    _ -> notFound

assemblePost :: Page -> LucidHtml
assemblePost Page {title, subtitle, tags, body, footnotes, comments, issueId} = do
  standardHead title
  standardBody $ do
    standardBanner
    standardTitle title subtitle
    tagsBar tags
    mapM_ renderSection body
    footnotesSection footnotes
    commentSection issueId comments

tagsBar :: [String] -> LucidHtml
tagsBar values = ul_ [class_ "tags"] $ do
  mapM_ (\s -> li_ $ url ("/tags/" ++ s) s) values

renderSection :: (Maybe String, LucidHtml) -> LucidHtml
renderSection (heading, content) = do
  case heading of
    Just s -> sectionH $ toHtml s
    Nothing -> mempty
  content

footnotesSection :: [LucidHtml] -> LucidHtml
footnotesSection values = div_ [class_ "footnotes"] $ do
  sectionH "Footnotes"
  hr_ []
  div_ [class_ "footnote-definitions"] $ do
    mapM_ (uncurry footnote) (zip [1 ..] values)

footnote :: Int -> LucidHtml -> LucidHtml
footnote idx html = do
  div_ [id_ footnoteId, class_ "footnote-definition"] $ do
    -- sup_ [class_ "dummy-a"] footnoteId
    div_ [class_ "footnote-body"] html
  where
    footnoteId = pack $ "footnote-" ++ show idx

commentSection :: Int -> [LucidHtml] -> LucidHtml
commentSection issueId comments = do
  div_ $ do
    sectionH "Comments"
    p_ [class_ "tagline"] $ em_ "Comments are a static snapshot of a GitHub Issue. Please leave a comment and after reviewing it, I'll rebuild the site with it."
    form_ [action_ . pack $ issuesUrl ++ show issueId] $ do
      button_ [formtarget_ "_blank", class_ "button-primary"] "Post a Comment"
    sequence_ comments

commentToHtml :: Comment -> LucidHtml
commentToHtml Comment {user, createdAt, body} = do
  let User {login, userHtmlUrl} = user
  let dateStr = formatTime defaultTimeLocale "%Y.%m.%d %H:%M:%S (UTC)" createdAt
  div_ [class_ "comment"] $ do
    p_ [class_ "comment-info"] $ do
      url userHtmlUrl login
      em_ [class_ "post-time"] $ toHtml (" posted at " ++ dateStr)
    hr_ [class_ "comment-divider"]
    body

notFound :: LucidHtml
notFound = do
  standardHead "Not Found"
  standardBody $ do
    standardBanner
    standardTitle "Blog not found" Nothing

---------------------------------------------------------------------------------------------------
-- /Contact
---------------------------------------------------------------------------------------------------

contactHtml :: LucidHtml
contactHtml = do
  standardHead "Contact"
  standardBody $ do
    standardBanner
    standardTitle "Contact" Nothing

---------------------------------------------------------------------------------------------------
-- Shared Templates
---------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------
-- <Head>
--------------------------------------------------------------------------

standardHead :: String -> LucidHtml
standardHead t = doctypehtml_ $ do
  head_ $ do
    title_ $ toHtml t
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=UTF-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    faviconLink
    css
    codeBlockCSS
    script_ [src_ "/static/theme-switcher.js"] emptyText

css :: LucidHtml
css = do
  link_
    [ id_ "theme",
      rel_ "stylesheet",
      href_ (pack $ "/static/" ++ defaultTheme ++ "-theme.css"),
      type_ "text/css"
    ]
  link_ [rel_ "stylesheet", href_ "/static/style.css", type_ "text/css"]
  link_ [rel_ "stylesheet", href_ "/static/skeleton.css", type_ "text/css"]

codeBlockCSS :: LucidHtml
codeBlockCSS = do
  link_ [rel_ "stylesheet", href_ "/static/highlight.default.min.css"]
  link_
    [ id_ "code-block-theme",
      rel_ "stylesheet",
      href_ (pack $ "/static/" ++ defaultTheme ++ ".min.css")
    ]
  script_ [src_ "/static/highlight.min.js"] emptyText
  script_ "hljs.highlightAll();"

faviconLink :: LucidHtml
faviconLink =
  link_
    [ rel_ "icon",
      href_ "static/favicon.ico",
      type_ "image/x-icon"
    ]

--------------------------------------------------------------------------
-- <Body>
--------------------------------------------------------------------------

navBar :: LucidHtml
navBar = do
  div_ [class_ "navbar-spacer"] ""
  nav_ [class_ "navbar"] $ do
    div_ [class_ "container"] $ do
      ul_ [class_ "navbar-list"] $ do
        navLink "/" "home"
        navLink "/tags" "tags"
        navLink "/contact" "contact"
        div_ [class_ "expander"] ""
        li_ [class_ "navbar-item"] $ do
          themeButton

navLink :: String -> String -> LucidHtml
navLink path text = do
  li_ [class_ "navbar-item"] $ do
    a_ [class_ "navbar-link", href_ . pack $ path] $ toHtml text

themeButton :: LucidHtml
themeButton = button_
  [id_ "button-dark-mode"]
  $ do
    svg_
      [ xmlns_ "http://www.w3.org/2000/svg",
        fill_ "currentColor",
        stroke_ "none",
        width_ "30",
        height_ "30",
        class_ "inline-block",
        viewBox_ "0 0 20 20",
        id_ "light-mode-icon"
      ]
      $ do
        path_ [fill_rule_ "evenodd", d_ "M10 2a1 1 0 011 1v1a1 1 0 11-2 0V3a1 1 0 011-1zm4 8a4 4 0 11-8 0 4 4 0 018 0zm-.464 4.95l.707.707a1 1 0 001.414-1.414l-.707-.707a1 1 0 00-1.414 1.414zm2.12-10.607a1 1 0 010 1.414l-.706.707a1 1 0 11-1.414-1.414l.707-.707a1 1 0 011.414 0zM17 11a1 1 0 100-2h-1a1 1 0 100 2h1zm-7 4a1 1 0 011 1v1a1 1 0 11-2 0v-1a1 1 0 011-1zM5.05 6.464A1 1 0 106.465 5.05l-.708-.707a1 1 0 00-1.414 1.414l.707.707zm1.414 8.486l-.707.707a1 1 0 01-1.414-1.414l.707-.707a1 1 0 011.414 1.414zM4 11a1 1 0 100-2H3a1 1 0 000 2h1z"]

    svg_
      [ xmlns_ "http://www.w3.org/2000/svg",
        fill_ "currentColor",
        stroke_ "none",
        width_ "30",
        height_ "30",
        class_ "inline-block",
        viewBox_ "0 0 20 20",
        id_ "dark-mode-icon"
      ]
      $ do
        path_ [d_ "M17.293 13.293A8 8 0 016.707 2.707a8.001 8.001 0 1010.586 10.586z"]

standardBanner :: LucidHtml
standardBanner = header_ [class_ "banner"] $ do
  a_ [href_ "/"] $ bannerH "Ori Livson's Blog"
  p_ "Something / Something Else / And That Too"

standardTitle :: String -> Maybe String -> LucidHtml
standardTitle title subtitle = do
  sectionH $ toHtml title
  case subtitle of
    Just s -> taglineH $ toHtml s
    Nothing -> mempty

standardBody :: LucidHtml -> LucidHtml
standardBody h = body_ [class_ "container has-docked-nav"] $ do
  navBar
  div_ [class_ "content"] h

-- Home and Tag Body

postEntries :: Pages -> LucidHtml
postEntries specs = ul_ [class_ "posts"] $ do
  mapM_ (uncurry postEntry) (sortPages specs)

postEntry :: String -> Page -> LucidHtml
postEntry endpoint Page {title, date} = do
  let dateStr = formatTime defaultTimeLocale "%Y.%m.%d" date
  li_ [class_ "post"] $ a_ [href_ (pack $ "/posts/" ++ endpoint)] $ do
    span_ [class_ "post-time"] $ toHtml dateStr
    span_ $ toHtml title

sortPages :: Pages -> [(String, Page)]
sortPages pages = sortBy comparePages $ toList pages
  where
    comparePages p1 p2 = compare (pageDate p1) (pageDate p2)
    pageDate (_, Page {date}) = date

---------------------------------------------------------------------------------------------------
-- Config
---------------------------------------------------------------------------------------------------

defaultTheme :: String
defaultTheme = "dark"

bannerH = h3_ []

sectionH = h4_ []

taglineH = h5_ [class_ "tagline"]

---------------------------------------------------------------------------------------------------
-- General Helpers
---------------------------------------------------------------------------------------------------

url :: String -> String -> LucidHtml
url path label = a_ [href_ (pack path)] $ toHtml label

emptyText :: Text
emptyText = pack ""
