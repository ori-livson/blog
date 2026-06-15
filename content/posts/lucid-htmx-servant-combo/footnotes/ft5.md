In Lucid v1, elements could be *partially applied* in the following sense.

```haskell
baseTextArea :: HTML -> HTML
baseTextArea =
  textarea_
    [ name_ "segments",
      cols_ "100"
    ]
```

And used like so:

```haskell
readOnlyBlock :: Int -> Text -> HTML
readOnlyBlock numRows box =
  with
    baseTextArea
    [ rows_ . toText $ numRows,
      readonly_ ""
    ]
    $ toHtml box
```

Lucid v2 uses a *builder* style, e.g.,

```haskell
baseTextArea :: [Attributes] -> HTML -> HTML
baseTextArea attrs =
  textarea_ $
    name_ "boxes" : cols_ "100" : attrs
```

Usable without the `with` function, i.e.,

```haskell
readOnlyBlock :: Int -> Text -> HTML
readOnlyBlock numRows box =
  baseTextArea
    [ rows_ . toText $ numRows,
      readonly_ ""
    ]
    $ toHtml box
```

However, generalising takes a bit more work, but leads to more readable composition, e.g.,
```haskell
-- More intuitive
type Element = HTML -> HTML

withId :: Element -> String -> Element
withId element idStr = with element [id_ . pack $ idStr]

v1 :: HTML
v1 = withId div_ "someId" "innerText"

-- Less readable
v1MoreAttrs :: HTML
v1MoreAttrs = with (withId div_ "someId") [class_ "more attrs"] "innerText"
```

vs

```haskell
-- Trickier
withId :: ([Attributes] -> t) -> String -> ([Attributes] -> t)
withId element idStr attrs = element $ (id_ . pack $ idStr) : attrs

v2 :: HTML
v2 = withId div_ "someId" [] "innerText"

-- More readable
v2MoreAttrs :: HTML
v2MoreAttrs = withId div_ "someId" [class_ "more attrs"] "innerText"
```

And also some other features for free, e.g.,

```haskell
v1 :: HTML
v1 =
  ul_ $ do
     mapM_ (li_ . toHtml . show) ([1, 2, 3] :: [Int])
```
vs

```haskell
v2 :: HTML
v2 = ul_ $ mapM_ (li_ . toHtml . show) [1, 2, 3]
```

See this [changelog](https://github.com/chrisdone/lucid/blob/master/lucid2/CHANGELOG.md#new-in-lucid2) and [creator's blog post](https://chrisdone.com/posts/lucid2/) for more details.
