module Config (issuesUrl, issuesApiUrl, bannerSubtitle, bannerTitle, defaultTheme, staticSrc, targetDir) where

issuesUrl :: String
issuesUrl = "https://github.com/ori-livson/blog/issues/"

issuesApiUrl :: String
issuesApiUrl = "https://api.github.com/repos/ori-livson/blog/issues/"

defaultTheme :: String
defaultTheme = "dark"

bannerTitle :: String
bannerTitle = "Ori Livson"

bannerSubtitle :: String
bannerSubtitle = "PhD Candidate / Software Engineer / PhD Teaching Fellow"

staticSrc :: String
staticSrc = "static"

targetDir :: String
targetDir = "html"