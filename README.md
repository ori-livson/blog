# Ori Livson's Blog

Feel free to fork this project and create your own:
    - `markdown` folder
    - `app/PageSpecs.hs` implementing:

``` haskell
module PageSpecs (loadBlog) where

import Templates (Blog(..))

loadBlog :: Bool -> Bool -> IO Blog
loadBlog devMode noComments = ...
```

    - `app/Config.hs` implementing:

``` haskell
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
bannerSubtitle = "Mathematics PhD Candidate / Software & Data Engineer"

staticSrc :: String
staticSrc = "static"

targetDir :: String
targetDir = "html"
```

## Generate files in html dir

```
cabal run -fforce-recomp
```

Add `-fdev` to include an example post.

Add `-fno-comments` to disable the GitHub API code (used to pull comments)

Add `-fforce-recomp` because we want to rebuild if we change markdown files for instance


## Run Simple Local Server

```
python3 -m http.server 9000 -d html
```

## Add new modules

To add a new module `app/X.hs` in root folder Y run `./add_module Y X`