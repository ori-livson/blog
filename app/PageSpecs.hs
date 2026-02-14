module PageSpecs
  ( loadBlog,
  )
where

import qualified Data.Map as Map
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
    -- youtube,
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
loadContact = do
  container <$> loadPath "content/contact.md"

loadHome :: IO HTML
loadHome = do
  container <$> loadPath "content/home.md"

loadPublications :: IO HTML
loadPublications = do
  container <$> loadPath "content/publications.md"

loadTeaching :: IO HTML
loadTeaching = do
  container <$> loadPath "content/teaching.md"

loadPosts :: Bool -> Bool -> IO Posts
loadPosts dev noComments = do
  arrowAusPost <- Map.singleton "is-there-a-right-way-to-vote" <$> loadArrowAus noComments
  let mainPosts = [arrowAusPost]

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
  intro <- load "0-intro.md"
  methodMap <- (makeFigure "60%" "Figure 1: Types of Voting Methods used for Lower House / Unicameral elections</br>(see the original <a href='https://commons.wikimedia.org/wiki/File:Electoral_systems_map.svg'>Legend</a>)") <$> (load "voting-methods.svg")

  ausA <- load "1-aus.md"
  irvExample <- (makeFigure "95%" "Figure 2: An example of preference flow in Instant Run-Off Voting. Candidate D is eliminated in round 1 and its 2 ballots are passed to their next ranked preference (B and A, respectively).") <$> (load "example-irv.svg")
  ausB <- load "1b-aus.md"

  ausC <- load "1c-aus.md"
  melbourne <- (makeFigure "95%" "Figure 3: Preference flows first to final preference flows in the Melbourne electorate") <$> (load "melbourne.svg")

  iia <- load "2a-iia.md"
  arrow <- load "2b-arrow.md"
  condorcet <- load "2c-condorcet.md"
  condorcetExample <- (makeFigure "95%" "Figure 4: an election with 3 voters that produces a Condorcet Paradox under pairwise majority voting.") <$> load "condorcet-paradox.svg"
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
  let postTitle = "Is there a right way to vote?"

  return
    Post
      { title = postTitle,
        subtitle = Just "Arrow's Impossibility Theorem and the 2025 Australian Federal Election",
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
    load = \x -> loadPath $ bodyDir </> x

---------------------------------------------------------------------------------------------------
-- Example Post
---------------------------------------------------------------------------------------------------

loadExamplePost :: Bool -> IO Post
loadExamplePost noComments = do
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
  let postTitle = "Just enough CSS for a blog (Test)"

  return
    Post
      { title = postTitle,
        subtitle = Just "This is copied from Niklas Fasching's blog post https://niklasfasching.de/posts/just-enough-css/ for testing only",
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
    load = \x -> loadPath $ rootDir </> "body" </> x
