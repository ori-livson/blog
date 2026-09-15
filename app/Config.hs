module Config
  ( issuesUrl,
    issuesApiUrl,
    bannerSubtitle,
    bannerTitle,
    defaultTheme,
    selfAuthor,
    selfDescription,
    selfTitle,
    selfUrl,
    staticSrc,
    targetDir,
  )
where

issuesUrl :: String
issuesUrl = "https://github.com/ori-livson/blog/issues/"

issuesApiUrl :: String
issuesApiUrl = "https://api.github.com/repos/ori-livson/blog/issues/"

defaultTheme :: String
defaultTheme = "dark"

bannerTitle :: String
bannerTitle = "Ori Livson"

bannerSubtitle :: String
bannerSubtitle = "PhD Candidate / Software Engineer"

staticSrc :: String
staticSrc = "static"

targetDir :: String
targetDir = "html"

selfUrl :: String
selfUrl = "https://ori-livson.com"

selfAuthor :: String
selfAuthor = "Ori Livson"

selfTitle :: String
selfTitle = "Ori Livson's Blog"

selfDescription :: String
selfDescription = "My blog about my math research and musings on software engineering."