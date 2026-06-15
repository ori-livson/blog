Next, we'll survey the Lucid HTML generation of the above endpoints, noting some nice features of Lucid. Then, we'll outline how Servant has to be setup, discuss some limitations and then conclude.

To begin, we need to implement our initial homepage request, i.e., our `GET /`, endpoint, which taking is simply.

```haskell
type HTML = Html () -- shorthand for redability

renderHomepage :: HTML
renderHomepage = do
  pageHead
  pageBody
```

Where we try to carve out the essential DOM structure in `pageBody` (see Figure 1), i.e., relegating bulky HTML (e.g., `<head>`), attributes and innerText to other functions or where clauses.

```haskell
pageBody :: HTML
pageBody =
  div_ $ do
    h1_ "Vomit Draft Editor"
    p_ instructions
    form_
      [ hxPost_ "/finish",
        hxTarget_ . toIdSelector $ containerId,
        hxSwap_ "outerHTML"
      ]
      $ div_ [idAttr containerId]
      $ do
        writableBlock
        submitButton

instructions :: Text
instructions =
  pack
    ( "Once you start typing you can only stop typing for "
        ++ show defaultTimeLimit
        ++ " seconds before your text is locked in!"
    )

submitButton :: HTML
submitButton = button_ [type_ "submit"] "Finalise"
```

We already see our first HTMX endpoint setup in `POST /finish`, the rest are part of `writableBlock :: HTML`. See [Footnote 3](#footnote-3) for more information on HTMX Attribute bindings in Lucid, and see [Footnote 4](#footnote-4) for more info on the helper functions like `idAttr` and `toIdSelector`.

We can also create *base elements* / *element builders*, e.g.,

```haskell
baseTextArea :: [Attributes] -> HTML -> HTML
baseTextArea attrs =
  textarea_ $
    name_ "boxes" : cols_ "100" : attrs

-- Used in both of:

readOnlyBlock :: Int -> Text -> HTML
readOnlyBlock numRows box =
  baseTextArea
    [ rows_ . toText $ numRows,
      readonly_ ""
    ]
    $ toHtml box

writableBlock :: HTML
writableBlock =
  div_ [idAttr activeBlockId] $ do
    baseTextArea
      [ rows_ . toText $ defaultRowsPerBlock,
        hxPost_ "/reset-timer",
        hxTrigger_ onInputEdit,
        hxTarget_ . toIdSelector $ timerId,
        hxSwap_ "outerHTML"
      ]
      mempty -- no innerHTML
    renderTimer defaultTimeLimit initModel
  where
    onInputEdit =
      T.intercalate
        ", "
        [ "input changed delay:300ms",
          "paste changed delay:300ms",
          "cut changed delay:300ms"
        ]
```

See [Footnote 5](#footnote-5) for more examples, and [Ui.hs](https://github.com/ori-livson/haskell-vomit-draft-editor/blob/master/app/Ui.hs) for all the other endpoint implementations.