module PageSpecs
  ( loadBlog,
  )
where

import qualified Data.Map as Map
-- youtube,

import Data.Text (pack)
import Data.Time.Calendar (fromGregorian)
import LucidUtils (HTML, loadPath, loadPathsOrdered)
import System.FilePath ((</>))
import Templates
  ( Blog (..),
    Post (..),
    Posts,
    SiteConfig (..),
    container,
    generateComments,
    makeFigure,
    makeImg,
  )

loadBlog :: Bool -> Bool -> IO Blog
loadBlog devMode noComments = do
  home <- loadHome
  about <- loadAbout
  upcoming <- loadUpcomingPlans
  posts <- loadPosts devMode noComments
  contact <- loadContact
  publications <- loadPublications
  teaching <- loadTeaching
  return Blog {home, about, upcoming, contact, publications, teaching, posts}

loadAbout :: IO HTML
loadAbout = container <$> loadPath "content/about.md"

loadUpcomingPlans :: IO HTML
loadUpcomingPlans = container <$> loadPath "content/upcoming.md"

loadContact :: IO HTML
loadContact = container <$> loadPath "content/contact.md"

loadHome :: IO HTML
loadHome = container <$> loadPath "content/home.md"

loadPublications :: IO HTML
loadPublications = container <$> loadPath "content/publications.md"

loadTeaching :: IO HTML
loadTeaching = container <$> loadPath "content/teaching.md"

loadPosts :: Bool -> Bool -> IO Posts
loadPosts dev noComments = do
  arrowAusPost <- Map.singleton "is-there-a-right-way-to-vote" <$> loadArrowAus noComments
  staticSite1 <- Map.singleton "how-this-website-was-made" <$> loadHowThisSiteWasMade noComments
  pythonHTMX <- Map.singleton "simple-htbuilder-htmx-fastapi-combo" <$> loadPythonHTMX noComments
  haskellHTMX <- Map.singleton "lucid-htmx-servant-combo" <$> loadHaskellHTMX noComments
  let mainPosts = [arrowAusPost, staticSite1, pythonHTMX, haskellHTMX]

  examplePost <-
    if dev
      then Map.singleton "example" <$> loadExamplePost noComments
      else return Map.empty

  return $ mconcat $ mainPosts ++ [examplePost]

---------------------------------------------------------------------------------------------------
-- Is there a right way to vote? Arrow and the 2025 Australian Elections
---------------------------------------------------------------------------------------------------

loadArrowAus :: Bool -> IO Post
loadArrowAus noComments = do
  let postTitle = "Is there a right way to vote?"
  let subtitle = Just "Arrow's Impossibility Theorem and the 2025 Australian Federal Election."

  intro <- load "0-intro.md"
  methodMap <-
    makeFigure
      "60%"
      "Figure 1: Types of Voting Methods used for Lower House / Unicameral elections</br>(see the original <a href='https://commons.wikimedia.org/wiki/File:Electoral_systems_map.svg'>Legend</a>)"
      <$> load "voting-methods.svg"

  ausA <- load "1-aus.md"
  irvExample <-
    makeFigure
      "95%"
      "Figure 2: An example of preference flow in Instant Run-Off Voting. Candidate D is eliminated in round 1 and its 2 ballots are passed to their next ranked preference (B and A, respectively)."
      <$> load "example-irv.svg"
  ausB <- load "1b-aus.md"

  ausC <- load "1c-aus.md"
  melbourne <-
    makeFigure
      "95%"
      "Figure 3: Preference flows first to final preference flows in the Melbourne electorate"
      <$> load "melbourne.svg"

  iia <- load "2a-iia.md"
  arrow <- load "2b-arrow.md"
  condorcet <- load "2c-condorcet.md"
  condorcetExample <-
    makeFigure
      "95%"
      "Figure 4: an election with 3 voters that produces a Condorcet Paradox under pairwise majority voting."
      <$> load "condorcet-paradox.svg"

  condorcet2 <- load "2d-condorcet.md"
  discussion <- load "3-discussion.md"
  discussionb <- load "3b-discussion.md"

  addendum <- load "4-addendum.md"

  let body =
        [ (Nothing, intro),
          (Nothing, methodMap),
          (Just "The 2025 Australian Federal Election", ausA),
          (Nothing, irvExample),
          (Nothing, ausB),
          (Just "How 3<sup>rd</sup>-Place Flips Electorates", ausC),
          (Nothing, melbourne),
          (Just "The Indenpendence of Irrelevant Alternatives Axiom", iia),
          (Just "Arrow's Impossibility Theorem", arrow),
          (Just "The Why: Condorcet Paradoxes", condorcet),
          (Nothing, condorcetExample),
          (Nothing, condorcet2),
          (Just "Arrow's Impossibility Theorem and Australia: a Bug or a Feature?", discussion),
          (Just "Is Voting Doomed?", discussionb),
          (Just "Addendum", addendum)
        ]

  footnotes <- loadPathsOrdered $ rootDir </> "footnotes"
  let issueId = 2
  comments <- generateComments noComments issueId

  return
    Post
      { title = postTitle,
        subtitle = subtitle,
        date = fromGregorian 2025 05 12,
        tags =
          [ "Mathematics",
            "Social Choice-Theory"
          ],
        body = body,
        footnotes = footnotes,
        comments = comments,
        issueId = issueId,
        staticPaths = [],
        siteConfig =
          SiteConfig
            { siteTitle = postTitle,
              hasCodeBlocks = False,
              hasMathBlocks = False
            }
      }
  where
    rootDir = "content/posts/arrow-aus"
    bodyDir = rootDir </> "body"
    load x = loadPath $ bodyDir </> x

---------------------------------------------------------------------------------------------------
-- How this website was made
---------------------------------------------------------------------------------------------------

loadHowThisSiteWasMade :: Bool -> IO Post
loadHowThisSiteWasMade noComments = do
  let postTitle = "How this website was made"
  let subtitle = Just "Clean HTML templating in Haskell and tricks for webdev on the cheap."

  intro <- load "0-intro.md"
  why <- load "1-the-requirements.md"
  functionalTemplating <- load "2-functional-templating.md"
  minimisingFrameworks <- load "3-minimising-frameworks.md"
  commentsTrick <- load "4-comments.md"
  themeing <- load "5-dark-light-mode.md"
  looks <- load "6-looks.md"
  codeAndMath <- load "7-code-blocks-and-math.md"
  hosting <- load "8-hosting.md"

  let body =
        [ (Nothing, intro),
          (Just "The Requirements", why),
          (Just "Functional Templating", functionalTemplating),
          (Just "Minimising Frameworks", minimisingFrameworks),
          (Just "Comments", commentsTrick),
          (Just "Themes", themeing),
          (Just "Looks", looks),
          (Just "Markdown Authoring", codeAndMath),
          (Just "Deployment and Hosting", hosting)
        ]

  footnotes <- loadPathsOrdered $ rootDir </> "footnotes"
  let issueId = 3
  comments <- generateComments noComments issueId

  return
    Post
      { title = postTitle,
        subtitle = subtitle,
        date = fromGregorian 2026 06 04,
        tags =
          [ "Software Engineering",
            "Web Development",
            "Haskell"
          ],
        body = body,
        footnotes = footnotes,
        comments = comments,
        issueId = issueId,
        staticPaths = [],
        siteConfig =
          SiteConfig
            { siteTitle = postTitle,
              hasCodeBlocks = True,
              hasMathBlocks = False
            }
      }
  where
    rootDir = "content/posts/how-this-website-was-made"
    bodyDir = rootDir </> "body"
    load x = loadPath $ bodyDir </> x

---------------------------------------------------------------------------------------------------
-- HTMX with Python's HTBuilder & FastAPI
---------------------------------------------------------------------------------------------------

loadPythonHTMX :: Bool -> IO Post
loadPythonHTMX noComments = do
  let postTitle = "HTMX with Python's HTBuilder & FastAPI"
  let subtitle = Just "Interactive websites all in one server generating HTML snippets."

  intro <- load "0-intro.md"
  htmx <- load "1-what-is-htmx.md"
  htbuilder <- load "2-what-is-htbuilder.md"
  fastapi <- load "3-fastapi.md"
  demoIntro <- load "4-demo-intro.md"
  preview <- load "5-preview.html"
  demoIntro2 <- load "6-demo-intro-2.md"
  sessionState <- load "7-session-state.md"
  multiupdates <- load "8-updating-mutliple-elements.md"
  conclusion <- load "9-conclusion.md"
  tricks <- load "10-htbuilder-tricks.md"
  css <- load "11-css.md"
  extras <- load "12-extras.md"

  let body =
        [ (Just "Introduction", intro),
          (Just "What is HTMX?", htmx),
          (Just "Generating HTML with HTBuilder?", htbuilder),
          (Just "FastAPI", fastapi),
          (Just "A First Project", demoIntro),
          (Nothing, preview),
          (Nothing, demoIntro2),
          (Just "Session State", sessionState),
          (Just "Updating Multiple HTMX Targets", multiupdates),
          (Just "Conclusion", conclusion),
          (Just "HT Builder Tricks", tricks),
          (Just "CSS", css),
          (Just "HTMX Extras", extras)
        ]

  footnotes <- loadPathsOrdered $ rootDir </> "footnotes"
  let issueId = 4
  comments <- generateComments noComments issueId

  return
    Post
      { title = postTitle,
        subtitle = subtitle,
        date = fromGregorian 2026 06 12,
        tags =
          [ "Software Engineering",
            "Web Development",
            "Python"
          ],
        body = body,
        footnotes = footnotes,
        comments = comments,
        issueId = issueId,
        staticPaths = [],
        siteConfig =
          SiteConfig
            { siteTitle = postTitle,
              hasCodeBlocks = True,
              hasMathBlocks = False
            }
      }
  where
    rootDir = "content/posts/simple-htbuilder-htmx-fastapi-combo"
    bodyDir = rootDir </> "body"
    load x = loadPath $ bodyDir </> x

---------------------------------------------------------------------------------------------------
-- HTMX with Python's HTBuilder & FastAPI
---------------------------------------------------------------------------------------------------

loadHaskellHTMX :: Bool -> IO Post
loadHaskellHTMX noComments = do
  let postTitle = "HTMX with Haskell's Lucid & Servant"
  let subtitle = Just "A \"Vomit Draft Editor\" made with functional HTML generation."

  intro <- load "0-intro.md"
  vomitDraft <- load "1-what-is-a-vomit-draft-editor.md"
  preview <- load "2-preview.html"
  vomitDraft2 <- load "3-what-is-a-vomit-draft-editor-pt2.md"
  solution <- load "4-the-solution.md"

  let diagramFname = "htmx-vomit-draft-diagram.png"
  let diagram =
        makeFigure
          "80%"
          "Figure 1: HTML & HX Posts; arrow direction points at what outerHTML will be replaced by the response."
          (makeImg . pack $ "static" </> diagramFname)
  -- important the above "static" doesn't start with a / so it's the static dir relative to the index.html

  solutionPt2 <- load "4.5-the-solution.md"
  theStack <- load "5-point-of-the-stack.md"
  lucid <- load "6-lucid.md"
  servant <- load "7-servant.md"
  whatsMising <- load "8-whats-missing.md"
  conclusion <- load "9-conclusion.md"

  let body =
        [ (Just "Introduction", intro),
          (Just "What is a Vomit Draft Editor?", vomitDraft),
          (Nothing, preview),
          (Nothing, vomitDraft2),
          (Just "The HTMX Solution", solution),
          (Nothing, diagram),
          (Nothing, solutionPt2),
          (Just "What is the point of this stack?", theStack),
          (Just "Generating the HTML with Lucid", lucid),
          (Just "The Servant API", servant),
          (Just "What's Missing?", whatsMising),
          (Just "Conclusion", conclusion)
        ]

  footnotes <- loadPathsOrdered $ rootDir </> "footnotes"
  let issueId = 5
  comments <- generateComments noComments issueId

  return
    Post
      { title = postTitle,
        subtitle = subtitle,
        date = fromGregorian 2026 06 16,
        tags =
          [ "Software Engineering",
            "Web Development",
            "Haskell"
          ],
        body = body,
        footnotes = footnotes,
        comments = comments,
        issueId = issueId,
        staticPaths = [bodyDir </> diagramFname],
        siteConfig =
          SiteConfig
            { siteTitle = postTitle,
              hasCodeBlocks = True,
              hasMathBlocks = False
            }
      }
  where
    rootDir = "content/posts/lucid-htmx-servant-combo"
    bodyDir = rootDir </> "body"
    load x = loadPath $ bodyDir </> x

---------------------------------------------------------------------------------------------------
-- Example Post
---------------------------------------------------------------------------------------------------

loadExamplePost :: Bool -> IO Post
loadExamplePost noComments = do
  let postTitle = "Just enough CSS for a blog (Test)"
  let subtitle = Just "This is copied from Niklas Fasching's blog post https://niklasfasching.de/posts/just-enough-css/ for testing only"

  intro <- load "intro.md"
  s1 <- mconcat <$> loadPathsOrdered (bodyDir </> "section1")
  s2 <- mconcat <$> loadPathsOrdered (bodyDir </> "section2")

  let body =
        [ (Nothing, intro),
          (Just "It doesn't have to be this way", s1),
          (Just "Creating my own minimal stylesheet", s2)
        ]

  footnotes <- loadPathsOrdered $ rootDir </> "footnotes"
  let issueId = 1
  comments <- generateComments noComments issueId

  return
    Post
      { title = postTitle,
        subtitle = subtitle,
        date = fromGregorian 2023 10 09,
        tags = ["CSS", "Minimalism"],
        body = body,
        footnotes = footnotes,
        comments = comments,
        issueId = issueId,
        staticPaths = [],
        siteConfig =
          SiteConfig
            { siteTitle = postTitle,
              hasCodeBlocks = True,
              hasMathBlocks = False
            }
      }
  where
    rootDir = "content/posts/example"
    bodyDir = rootDir </> "body"
    load x = loadPath $ rootDir </> "body" </> x
