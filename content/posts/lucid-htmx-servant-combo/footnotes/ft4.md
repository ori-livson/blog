Even with the `OverloadedStrings` extension set, one will likely find themselves managing conversions between the following types:

- HTML (i.e., `type HTML = Html ()` from `Lucid`): generally used with `toHTML` to set `innerText`.
- `Text` (i.e., from `Data.Text`): generally used to set attribute values.
- `String`: generally the outcome of building up endpoints, e.g., `"/" ++ show timeRemaining ++ "/tick"`.

And if you're a bit too obsessive about readability like me, you'll painstakingly try to avoid writing expressions like: 
```haskell
div_ [id_ . pack $ containerId]`
-- or
div_ [hxTarget_ . pack $ "#" ++ containerId]
```

So I've created helpers like:

```bash
idAttr :: String -> Attributes
idAttr = id_ . pack

-- to be able to write: div_ [idAttr containerId]`

toIdSelector :: String -> Text
toIdSelector idStr = pack $ "#" ++ idStr

-- to be able to write: div_ [hxTarget_ . toIdSelector $ containerId]`
```