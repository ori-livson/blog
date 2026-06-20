Ernest Marcinko's [htmx-serverless](https://github.com/ernestmarcinko/htmx-serverless) project allows us to add an attribute `hx-ext="serverless"` to any HTMX powered element so that any time it would fire an HTTP request, a JavaScript function can be run in its place. For example, the following HTML:
```html
<p
    id="timer"
    hx-post="/tick"
    hx-trigger="every 1000ms"
    hx-target="#timer"
    hx-swap="outerHTML"
    hx-ext="serverless"
    hx-vals="js:{timeRemaining: 5}"
>
    Remaining: 5
</p>
```
Can be handled by the following:
```javascript
htmxServerless.handlers.set('/tick', function(text, params, xhr){
    ...
});
```
Which catches any `POST /tick` call and injects the corresponding `FormData` content (that HTMX generates) into the `params` argument. Extra, named form-data can be injected via the `hx-vals` attribute, e.g., `hx-vals="js:{timeRemaining: 5}"` adds `timeRemaining=5` to the JS FormData object.

Now, to get this handler to call our `.wasm` for the correct HTML response, and importantly such that we can generate these handles, we serve all `hx-` requests with a single `.wasm` function `dispatch: String -> String` with:

- **Input:** endpoints `/path?query_params`, where `query_params` is just the url-encoded form data, e.g., our form with `hx-vals="js:{timeRemaining: 3}"` and 2 text-boxes, both with name "boxes" and text "a" and "b", respectively yields input `/tick?timeRemainingboxes=3&boxes=a&boxes=b"`

- **Output:** whatever HTML-string we would respond to the `hx-` request with.

This allows us to generate handlers programmatically like so:

```javascript
export function genHandler(endpoint) {
  /*Takes an endpoint like POST   /tick
    with form urlencoded:         boxes=a&boxes=b&timeRemaining=3
    and creates a new endpoint:   /tick?boxes=a&boxes=b&timeRemaining=3
    then calls the wasm:          dispatch(/tick?boxes=a&boxes=b&timeRemaining=3)
  */
  return function (text, params, xhr) {
    const qs = new URLSearchParams(params).toString();
    const payload = endpoint + "?" + qs;
    const html = callWithString(inst.exports.dispatch, payload);
    return html;
  };
}
```

Where our WebAssembly instance `inst` and `callWithString` are part of a module [wasm-dispatcher.js](https://github.com/ori-livson/haskell-htmx-to-wasm/blob/master/site/static/wasm-dispatcher.js), and we generate handlers in the HTML with:

```html
<script type="module">
    import { genHandler } from "./static/wasm-dispatcher.js";
    htmxServerless.handlers.set("/tick", genHandler("/tick"));
    htmxServerless.handlers.set("/reset-timer", genHandler("/reset-timer"));
    htmxServerless.handlers.set("/finish", genHandler("/finish"));
    htmxServerless.handlers.set("/close-block", genHandler("/close-block"));
    htmxServerless.handlers.set("/times-up", genHandler("/times-up"));
</script>
```
Which in turn is generated in our [Ui.hs](https://github.com/ori-livson/haskell-htmx-to-wasm/blob/master/app/Ui.hs)
 in a straightforward manner by:

```haskell
handlers ["/tick", "/reset-timer", "/finish", "/close-block", "/times-up"]

handlers :: [Text] -> HTML
handlers routes =
  script_ [type_ "module"] $
    "import { genHandler } from \"./static/wasm-dispatcher.js\";\n"
      <> foldMap registerHandler routes

registerHandler :: Text -> Text
registerHandler route =
  "htmxServerless.handlers.set(\"" <> route <> "\", genHandler(\"" <> route <> "\"));"
```

Furthermore, the dispatch calls are redirected in our `.wasm` to our HTML producing functions in `Ui.hs` by the code block below. Importantly, the `.wash` API we've created involves a similar volume of code to an equivalent [Servant API](https://github.com/ori-livson/haskell-vomit-draft-editor/blob/master/app/Main.hs). Albeit without the sophisticated, automated parsing and validation Servant provides.

```haskell
type QueryMap = Map Text [Text]

handler :: String -> IO String
handler input = do
  putStrLn ("Got input: " ++ input)

  -- split /tick?boxes=a&boxes=b&timeRemaining=3 into (/tick, boxes=a&boxes=b&timeRemaining=3)
  let (path, _) =
        case break (== '?') input of
          (p, '?' : q) -> (p, q)
          (p, _) -> (p, "")

  let qMap = parseQuery $ T.pack input
  let model = buildModel qMap
  let timeRemaining = lookupInt "timeRemaining" qMap

  let html =
        case path of
          "/" ->
            renderHomepage
          "/reset-timer" ->
            renderTimer defaultTimeLimit model
          "/close-block" ->
            renderClosedAndNewBlock model
          "/finish" ->
            renderFinishedContainer model
          "/times-up" ->
            renderTimesUpContainer model
          "/tick" -> do
            case timeRemaining of
              Just n -> renderTimer (n - 1) model
              Nothing -> error "/tick timeRemaining missing!"
          _ -> error $ "Unknown route: " ++ path

  return . LT.unpack . renderText $ html

parseQuery :: T.Text -> QueryMap
--- < 10 line function
```

Thus, **we write < 100 lines of JS utilities once** (the `.wasm` reader and JS function to call our the `.wasm`'s `dispatch` function, and the `genHandler` function), and then every time we need a new `hx-` endpoint, we implement the handler (i.e., HTML generation) in our Haskell API, and simply add an entry to the corresponding block:
```haskell
handlers ["/tick", "/reset-timer", "/finish", "/close-block", "/times-up"]

handlers :: [Text] -> HTML
...
```