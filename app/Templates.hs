module Templates (homeHtml, tagsHtml, contactHtml, postHtml) where

import Data.List (sortBy)
import Data.Map (toList)
import qualified Data.Map as M
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Lucid
import Lucid.Svg (d_, fill_, fill_rule_, path_, stroke_, viewBox_)
import Page (LucidHtml, Page (Page, body, date, footnotes, subtitle, tags, title), Pages)

defaultTheme :: String
defaultTheme = "dark"

----------------------------------------------------------------------------------------------------
-- /Home
----------------------------------------------------------------------------------------------------

homeHtml :: Pages -> LucidHtml
homeHtml pages = do
  siteHead "Ori's Doo-Das"
  blogBody $ do
    mainHeading "Ori Livson's" $ Just "Doo-Das"
    postEntries pages

----------------------------------------------------------------------------------------------------
-- /Tags
----------------------------------------------------------------------------------------------------

tagsHtml :: Pages -> String -> LucidHtml
tagsHtml pages value = do
  siteHead value
  blogBody $ do
    mainHeading value Nothing
    postEntries $ Map.filter matchesTag pages
  where
    matchesTag Page {tags} = value `elem` tags

----------------------------------------------------------------------------------------------------
-- /Posts
----------------------------------------------------------------------------------------------------

postHtml :: Pages -> String -> LucidHtml
postHtml pages endpoint = do
  case M.lookup endpoint pages of
    Just spec -> assemblePost spec
    _ -> notFound

assemblePost :: Page -> LucidHtml
assemblePost Page {title, subtitle, tags, body, footnotes} = do
  siteHead title
  blogBody $ do
    mainHeading title subtitle
    tagsBar tags
    sequence_ body
    footnotesSection footnotes

tagsBar :: [String] -> LucidHtml
tagsBar values = ul_ [class_ "tags"] $ do
  mapM_ (\s -> li_ $ url ("/tags/" ++ s) s) values

url :: String -> String -> LucidHtml
url path label = a_ [href_ (pack path)] $ toHtml label

footnotesSection :: [LucidHtml] -> LucidHtml
footnotesSection values = div_ [class_ "footnotes"] $ do
  h2_ "Footnotes"
  hr_ [class_ "footnotes-separatator"]
  div_ [class_ "footnote-definitions"] $ do
    mapM_ (uncurry footnote) (zip [1 ..] values)

footnote :: Int -> LucidHtml -> LucidHtml
footnote idx html = do
  div_ [id_ footnoteId, class_ "footnote-definition"] $ do
    -- sup_ [class_ "dummy-a"] footnoteId
    div_ [class_ "footnote-body"] html
  where
    footnoteId = pack $ "footnote-" ++ show idx

notFound :: LucidHtml
notFound = do
  siteHead "Not Found"
  blogBody $ do
    mainHeading "Blog not found" Nothing

----------------------------------------------------------------------------------------------------
-- /Contact
----------------------------------------------------------------------------------------------------

contactHtml :: LucidHtml
contactHtml = do
  siteHead "Contact"
  blogBody $ do
    mainHeading "Contact" Nothing

----------------------------------------------------------------------------------------------------
-- Shared Templates
----------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------
-- <Head>
---------------------------------------------------------------------------

siteHead :: String -> LucidHtml
siteHead t = doctypehtml_ $ do
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

emptyText :: Text
emptyText = pack ""

---------------------------------------------------------------------------
-- <Body>
---------------------------------------------------------------------------

navBar :: LucidHtml
navBar = do
  div_ [class_ "navbar-spacer"] ""
  nav_ [class_ "navbar"] $ do
    div_ [class_ "container"] $ do
      ul_ [class_ "navbar-list"] $ do
        li_ [class_ "navbar-item"] $ do
          a_ [class_ "navbar-link", href_ "/"] "home"
        li_ [class_ "navbar-item"] $ do
          a_ [class_ "navbar-link", href_ "/contact"] "contact"
        li_ [class_ "navbar-item"] $ do
          themeButton

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

mainHeading :: String -> Maybe String -> LucidHtml
mainHeading title subtitle = h1_ [class_ "title"] $ do
  toHtml title
  br_ []
  span_ [class_ "subtitle"] $ toHtml (fromMaybe "" subtitle)

-- Blog Body

blogBody :: LucidHtml -> LucidHtml
blogBody h = body_ [class_ "container has-docked-nav"] $ do
  navBar
  div_ [class_ "content"] h

-- Home and Tag Body

postEntries :: Pages -> LucidHtml
postEntries specs = ul_ [class_ "posts"] $ do
  mapM_ (uncurry postEntry) (sortPages specs)

postEntry :: String -> Page -> LucidHtml
postEntry endpoint Page {title, date} = do
  li_ [class_ "post"] $ a_ [href_ (pack $ "/posts/" ++ endpoint)] $ do
    time_ [datetime_ (pack formattedDate)] $ toHtml formattedDate
    span_ $ toHtml title
  where
    formattedDate = formatTime defaultTimeLocale "%Y.%m.%d" date

sortPages :: Pages -> [(String, Page)]
sortPages pages = sortBy comparePages $ toList pages
  where
    comparePages p1 p2 = compare (pageDate p1) (pageDate p2)
    pageDate (_, Page {date}) = date
