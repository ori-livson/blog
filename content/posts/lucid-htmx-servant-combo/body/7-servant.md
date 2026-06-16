The core architecture I went with is splitting the program into two modules `Ui.hs`, `Main.hs`. The former contains all the code for the HTML generation and a model of the client state. The latter, simply contains the code for spinning up the API Server, defining its endpoints, and converting between HTTP requests/responses, and inputs/outs of functions in `Ui.hs`.

Interestingly, we can automate the conversion between Haskell record and HTTP request bodies as follows.

```haskell
import Lucid (Html)
import Web.FormUrlEncoded (FromForm)    -- http-api-data package

newtype Model
  = Model {boxes :: [Text]}             -- Text requires less type-conversion than String for filling in arguments in Lucid
  deriving (Generic)                    -- needed for FromForm to work

instance FromForm Model                 -- unfortunate coupling between Main and UI but this has to be defined with the type.
```

Then, with the help of the [dani-servant-lucid2 package](https://hackage.haskell.org/package/dani-servant-lucid2) we convert Lucid's HTML type to an HTTP HTML Response.
```haskell
import Servant
import qualified Servant.API.ContentTypes.Lucid as SL
import Ui
import qualified Ui as U

type ModelRequestBody = ReqBody '[FormUrlEncoded] Model

type GetHtmlResponse = Get '[SL.HTML] U.HTML

type PostHtmlResponse = Post '[SL.HTML] U.HTML
```

Then, we finally define our API type as follows:

```haskell
type API =
  "static" :> Raw
    :<|> GetHtmlResponse
    :<|> "reset-timer" :> ModelRequestBody :> PostHtmlResponse
    :<|> Capture "remainingTime" Int :> "tick" :> ModelRequestBody :> PostHtmlResponse
    :<|> "close-block" :> ModelRequestBody :> PostHtmlResponse
    :<|> "finish" :> ModelRequestBody :> PostHtmlResponse
    :<|> "times-up" :> ModelRequestBody :> PostHtmlResponse
```

To implement - say - the `POST /{remainingTime}/tick` endpoint, we need to provide a function of type `Int -> Model -> Handler U.HTML`, where `Handler` is the Monad that handles conversion from/to HTTP request/responses.

In `UI.hs` the implementation of rendering new HTML from `remainingTime` and the `<textarea>` values (i.e., client state) is a function of the form `Int -> Model -> U.HTML`, so all we need for our implementation is:
```haskell
renderResetTimer :: Model -> Handler U.HTML
renderResetTimer model =
    return $ renderTimer defaultTimeLimit model
```
(For more information on `return` and Monads in general, I recommend [this tutorial in pictures](https://www.adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)).

When, the endpoint only has 1 argument, like `POST /finish`, if we have a `Ui.hs` function `renderFinishedContainer :: Model -> HTML` we simply implement the endpoint with `return . renderFinishedContainer :: Model -> Handler HTML`. So the full implementation is as follows.
```haskell
server :: Server API
server =
  serveDirectoryWebApp "static/"
    :<|> return renderHomepage
    :<|> renderResetTimer
    :<|> renderDecrementedTimer
    :<|> return . renderClosedAndNewBlock
    :<|> return . renderFinishedContainer
    :<|> return . renderTimesUpContainer
  where
    renderResetTimer :: Model -> Handler U.HTML
    renderResetTimer model =
      return $ renderTimer defaultTimeLimit model

    renderDecrementedTimer :: Int -> Model -> Handler U.HTML
    renderDecrementedTimer timeRemaining model =
      return $ renderTimer (timeRemaining - 1) model
```
The rest of `Main.hs` is standard and can be found [here](https://github.com/ori-livson/haskell-vomit-draft-editor/blob/master/app/Main.hs), which I largely drew from this [starter code](https://github.com/haskell-servant/servant-lucid/blob/master/example/Main.hs), from the original [servant-lucid package](https://hackage.haskell.org/package/servant-lucid) for [Lucid v1](https://hackage.haskell.org/package/lucid) rather than the current [Lucid v2](https://hackage.haskell.org/package/lucid2). See [Footnote 6](#footnote-6) for more info on Lucid v1.