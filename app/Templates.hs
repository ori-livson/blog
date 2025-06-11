{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Templates
  ( aboutHtml,
    generateComments,
    homeHtml,
    tagMatchHtml,
    contactHtml,
    postsHtml,
    assemblePost,
    upcomingHtml,
    makeFigure,
    publicationsHtml,
    commentToHtml,
    allTagsHtml,
    container,
    youtube,
    titleH,
    Post (..),
    Posts,
    Blog (..),
    SiteConfig (..),
  )
where

import Config (bannerSubtitle, bannerTitle, defaultTheme, issuesUrl)
import Control.Monad (when)
import Data.List (nub, sort, sortBy)
import Data.Map (Map, toList)
import qualified Data.Map as M
import qualified Data.Map as Map
import Data.Text (Text, pack)
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GitHub
  ( Comment (Comment, body, createdAt, user),
    User (User, login, userHtmlUrl),
    loadCommentsFromIssue,
  )
import Lucid
import Lucid.Base (makeAttribute)
import Lucid.Svg (d_, fill_, fill_rule_, path_, rect_, stroke_, viewBox_, x_, y_)
import qualified Lucid.Svg as Svg
import LucidUtils (LucidHtml)

data Blog = Blog
  { home :: LucidHtml,
    about :: LucidHtml,
    upcoming :: LucidHtml,
    contact :: LucidHtml,
    publications :: LucidHtml,
    posts :: Posts
  }

---------------------------------------------------------------------------------------------------
-- /Home
---------------------------------------------------------------------------------------------------

homeHtml :: LucidHtml -> Posts -> LucidHtml
homeHtml intro posts = do
  standardHead $ plainSite bannerTitle
  standardBody True $ do
    homeBanner
    div_ [centerText] intro
    titleH "Posts"
    postEntries posts

homeBanner :: LucidHtml
homeBanner = bannerHead $ do
  bannerHeading
  p_ $ toHtml bannerSubtitle

---------------------------------------------------------------------------------------------------
-- /Posts
---------------------------------------------------------------------------------------------------

data Post = Post
  { title :: String,
    subtitle :: Maybe String,
    date :: Day,
    tags :: [String],
    body :: [(Maybe String, LucidHtml)],
    footnotes :: [LucidHtml],
    comments :: Maybe [LucidHtml],
    issueId :: Int,
    staticPaths :: [FilePath],
    siteConfig :: SiteConfig
  }

type Posts = Map String Post

postsHtml :: Posts -> LucidHtml
postsHtml posts = do
  standardHead $ plainSite "Posts"
  standardBody True $ do
    standardBanner
    titleH "Posts"
    postEntries posts

postEntries :: Posts -> LucidHtml
postEntries specs = ul_ [class_ "posts"] $ do
  mapM_ (uncurry postEntry) (sortPosts specs)

postEntry :: String -> Post -> LucidHtml
postEntry endpoint Post {title, date, subtitle} = do
  let dateStr = formatTime defaultTimeLocale "%Y.%m.%d" date
  li_ [class_ "post"] $
    div_ $ do
      div_ $
        span_ $
          toHtml dateStr
      div_ $ do
        a_ [href_ (pack $ "/posts/" ++ endpoint), class_ "post-title"] $ toHtml title
        case subtitle of
          Just s -> div_ [singleStyle "font-style" "italic"] $ toHtml s
          Nothing -> mempty

sortPosts :: Posts -> [(String, Post)]
sortPosts posts = sortBy (flip comparePosts) (toList posts)
  where
    comparePosts p1 p2 = compare (postDate p1) (postDate p2)
    postDate (_, Post {date}) = date

---------------------------------------------------------------------------------------------------
-- /About
---------------------------------------------------------------------------------------------------

aboutHtml :: LucidHtml -> LucidHtml
aboutHtml contents = do
  standardHead $ plainSite "About"
  standardBody True $ do
    standardBanner
    standardTitle "About" Nothing
    contents

---------------------------------------------------------------------------------------------------
-- /Upcoming Posts
---------------------------------------------------------------------------------------------------

upcomingHtml :: LucidHtml -> LucidHtml
upcomingHtml contents = do
  standardHead $ plainSite "Upcoming Posts"
  standardBody True $ do
    standardBanner
    standardTitle "Upcoming Posts" Nothing
    -- standardBody True centers everything below with flex-box
    -- wrapping contents with div_ keeps contents left aligned
    -- but the whole block centered
    contents

---------------------------------------------------------------------------------------------------
-- /Contact
---------------------------------------------------------------------------------------------------

contactHtml :: LucidHtml -> LucidHtml
contactHtml contents = do
  standardHead $ plainSite "Contact"
  standardBody True $ do
    standardBanner
    standardTitle "Contact" Nothing
    contents

---------------------------------------------------------------------------------------------------
-- /Publications
---------------------------------------------------------------------------------------------------

publicationsHtml :: LucidHtml -> LucidHtml
publicationsHtml contents = do
  standardHead $ plainSite "Publications"
  standardBody True $ do
    standardBanner
    standardTitle "Publications" Nothing
    contents

---------------------------------------------------------------------------------------------------
-- /Tags
---------------------------------------------------------------------------------------------------

allTagsHtml :: Posts -> LucidHtml
allTagsHtml posts = do
  standardHead $ plainSite "All Tags"
  standardBody False $ do
    standardBanner
    standardTitle "All Tags" Nothing
    ul_ [class_ "all-tags"] $ mapM_ (\s -> li_ [class_ "all-tags"] $ url ("/tags/" ++ s) s) distinctTags
  where
    distinctTags = sort . nub . concatMap tags $ posts

tagMatchHtml :: Posts -> String -> LucidHtml
tagMatchHtml posts value = do
  standardHead $ plainSite value
  standardBody True $ do
    standardBanner
    standardTitle ("Tag: \"" ++ value ++ "\"") Nothing
    postEntries $ Map.filter matchesTag posts
  where
    matchesTag Post {tags} = value `elem` tags

---------------------------------------------------------------------------------------------------
-- /Posts/x
---------------------------------------------------------------------------------------------------

assemblePost :: Post -> LucidHtml
assemblePost Post {title, subtitle, tags, body, footnotes, comments, issueId, siteConfig} = do
  standardHead siteConfig
  standardBody True $ do
    standardBanner
    postTitle title subtitle
    tagsBar tags
    mapM_ renderSection body
    commentSection issueId comments
    footnotesSection footnotes

postTitle :: String -> Maybe String -> LucidHtml
postTitle title subtitle = do
  postTitleH $ toHtml title
  case subtitle of
    Just s -> subtitleH $ toHtml s
    Nothing -> mempty

tagsBar :: [String] -> LucidHtml
tagsBar values = ul_ [class_ "tags"] $ do
  mapM_ (\s -> li_ $ url ("/tags/" ++ s) s) values

renderSection :: (Maybe String, LucidHtml) -> LucidHtml
renderSection (heading, content) = do
  case heading of
    Just s -> sectionH $ toHtmlRaw s
    Nothing -> mempty
  content

footnotesSection :: [LucidHtml] -> LucidHtml
footnotesSection [] = mempty
footnotesSection values = div_ [class_ "footnotes"] $ do
  sectionH "Footnotes and References"
  hr_ []
  div_ [class_ "footnote-definitions"] $ do
    mapM_ (uncurry footnote) (zip [1 ..] values)

footnote :: Int -> LucidHtml -> LucidHtml
footnote idx html = do
  div_ [id_ footnoteId, class_ "footnote-definition"] $ do
    sup_ [class_ "dummy-a"] (toHtml $ show idx)
    div_ [class_ "footnote-body"] html
  where
    footnoteId = pack $ "footnote-" ++ show idx

commentSection :: Int -> Maybe [LucidHtml] -> LucidHtml
commentSection _ Nothing = mempty
commentSection issueId (Just comments) = do
  div_ [class_ "comments"] $ do
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

---------------------------------------------------------------------------------------------------
-- Shared Templates
---------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------
-- <Head>
--------------------------------------------------------------------------

data SiteConfig = SiteConfig
  { siteTitle :: String,
    hasCodeBlocks :: Bool,
    hasMathBlocks :: Bool
  }

plainSite :: String -> SiteConfig
plainSite t =
  SiteConfig
    { siteTitle = t,
      hasCodeBlocks = False,
      hasMathBlocks = False
    }

standardHead :: SiteConfig -> LucidHtml
standardHead SiteConfig {siteTitle, hasCodeBlocks, hasMathBlocks} = doctypehtml_ $ do
  head_ $ do
    title_ $ toHtml siteTitle
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=UTF-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    meta_ [charset_ "UTF-8"]
    when hasMathBlocks $ script_ [src_ "/static/math-jax.js"] emptyText
    faviconLink
    css
    when hasCodeBlocks codeBlockCSS
    script_ [src_ "/static/theme-switcher.js"] emptyText
    script_ [src_ "/static/main.js"] emptyText

css :: LucidHtml
css = do
  link_
    [ id_ "link-theme-css-vars",
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
    [ id_ "link-code-block-theme",
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

--------------------------------------
-- Nav Bar
--------------------------------------

navBar :: LucidHtml
navBar = do
  nav_ [class_ "navbar"] $ do
    div_ [class_ "container"] $ do
      ul_ [class_ "navbar-list"] $ do
        li_ [id_ "sandwich-li", class_ "navbar-item"] $ sandwichButton
        navLink "/" "home"
        navLink "/about" "about"
        navLink "/publications" "publications"
        navLink "/posts" "posts"
        navLink "/tags" "tags"
        navLink "/upcoming" "upcoming"
        navLink "/contact" "contact"
        div_ [class_ "expander"] ""
        li_ [id_ "theme-li", class_ "navbar-item"] $ do
          themeButton
          clearThemeButton

navLink :: String -> String -> LucidHtml
navLink path text = do
  li_ [class_ "navbar-item nav-toggle"] $ do
    a_ [class_ "navbar-link", href_ . pack $ path] $ toHtml text

themeButton :: LucidHtml
themeButton = button_
  [id_ "button-toggle-dark-mode"]
  $ do
    svg_
      (svgIcon "sun-icon")
      $ do
        -- Light theme symbol (sun)
        path_ [fill_rule_ "evenodd", d_ "M10 2a1 1 0 011 1v1a1 1 0 11-2 0V3a1 1 0 011-1zm4 8a4 4 0 11-8 0 4 4 0 018 0zm-.464 4.95l.707.707a1 1 0 001.414-1.414l-.707-.707a1 1 0 00-1.414 1.414zm2.12-10.607a1 1 0 010 1.414l-.706.707a1 1 0 11-1.414-1.414l.707-.707a1 1 0 011.414 0zM17 11a1 1 0 100-2h-1a1 1 0 100 2h1zm-7 4a1 1 0 011 1v1a1 1 0 11-2 0v-1a1 1 0 011-1zM5.05 6.464A1 1 0 106.465 5.05l-.708-.707a1 1 0 00-1.414 1.414l.707.707zm1.414 8.486l-.707.707a1 1 0 01-1.414-1.414l.707-.707a1 1 0 011.414 1.414zM4 11a1 1 0 100-2H3a1 1 0 000 2h1z"]

    svg_
      (svgIcon "moon-icon")
      $ do
        -- Dark theme symbol (crescent moon)
        path_ [d_ "M17.293 13.293A8 8 0 016.707 2.707a8.001 8.001 0 1010.586 10.586z"]

sandwichButton :: LucidHtml
sandwichButton = button_ [id_ "sandwich-button"] $ do
  svg_ (svgIcon "sandwich-icon") $ do
    rect_ [x_ "2", y_ "5", Svg.width_ "16", height_ "1"]
    rect_ [x_ "2", y_ "10", Svg.width_ "16", height_ "1"]
    rect_ [x_ "2", y_ "15", Svg.width_ "16", height_ "1"]

svgIcon :: Text -> [Attribute]
svgIcon svgId =
  [ xmlns_ "http://www.w3.org/2000/svg",
    fill_ "currentColor",
    stroke_ "none",
    Svg.width_ "30",
    height_ "30",
    class_ "icon-button",
    viewBox_ "0 0 20 20",
    id_ svgId
  ]

clearThemeButton :: LucidHtml
clearThemeButton =
  button_
    [id_ "button-revert-to-os-theme-preference", class_ "button-primary"]
    "OS Theme"

--------------------------------------
-- Rest of Body
--------------------------------------

bannerHead :: LucidHtml -> LucidHtml
bannerHead = header_ [class_ "banner"]

bannerHeading :: LucidHtml
bannerHeading = a_ [href_ "/"] $ bannerH (toHtml bannerTitle)

standardBanner :: LucidHtml
standardBanner = bannerHead $ do
  bannerHeading

standardTitle :: String -> Maybe String -> LucidHtml
standardTitle title subtitle = do
  titleH $ toHtml title
  case subtitle of
    Just s -> subtitleH $ toHtml s
    Nothing -> mempty

standardBody :: Bool -> LucidHtml -> LucidHtml
standardBody centered content = body_ [class_ "container has-docked-nav"] $ do
  navBar
  div_
    [ class_ $
        if centered
          then "content centered"
          else "content"
    ]
    content

--------------------------------------
-- Comments
--------------------------------------

-- Bool arg is noComments
generateComments :: Bool -> Int -> IO (Maybe [LucidHtml])
generateComments True _ = return Nothing
generateComments False issueId = Just . map commentToHtml <$> loadCommentsFromIssue issueId

---------------------------------------------------------------------------------------------------
-- General Elements
---------------------------------------------------------------------------------------------------

url :: String -> String -> LucidHtml
url path label = a_ [href_ (pack path)] $ toHtml label

emptyText :: Text
emptyText = pack ""

container :: LucidHtml -> LucidHtml
container = div_

bannerH :: LucidHtml -> LucidHtml
bannerH = h1_ [centerText]

titleH :: LucidHtml -> LucidHtml
titleH = h2_ [centerText]

postTitleH :: LucidHtml -> LucidHtml
postTitleH = h3_ [centerText]

sectionH :: LucidHtml -> LucidHtml
sectionH = h3_ [centerText]

subtitleH :: LucidHtml -> LucidHtml
subtitleH = h5_ [centerText, singleStyle "font-style" "italic"]

centerText :: Attribute
centerText = singleStyle "text-align" "center"

youtube :: String -> LucidHtml
youtube src =
  iframe_
    [ Lucid.width_ "560",
      height_ "315",
      src_ $ pack src,
      title_ "YouTube video player",
      makeAttribute "frameborder" "0",
      makeAttribute "allow" "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share",
      makeAttribute "referrerpolicy" "strict-origin-when-cross-origin",
      makeAttribute "allowfullscreen" "allowfullscreen"
    ]
    mempty

makeFigure :: Text -> Text -> LucidHtml -> LucidHtml
makeFigure width caption content =
  figure_ [styleFromMap $ Map.fromList [("width", width), ("height", "auto")]] $ do
    content
    figcaption_ (toHtmlRaw caption)

styleFromMap :: Map Text Text -> Attribute
styleFromMap m = style_ $ mconcat $ map stylePart (M.toList m)

singleStyle :: Text -> Text -> Attribute
singleStyle k v = styleFromMap $ Map.singleton k v

stylePart :: (Text, Text) -> Text
stylePart (k, v) = k <> ": " <> v <> "; "