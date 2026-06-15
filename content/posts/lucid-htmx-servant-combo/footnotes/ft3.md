To get HTMX bindings for Lucid, there's [this package](https://hackage.haskell.org/package/lucid-htmx-0.1.0.7), but it clashed with my dependencies. However, making your own attributes in Lucid is very easy, here are the ones I made for this project:

```haskell
hxPost_ :: Text -> Attribute
hxPost_ = makeAttribute "hx-post"

hxTrigger_ :: Text -> Attribute
hxTrigger_ = makeAttribute "hx-trigger"

hxTarget_ :: Text -> Attribute
hxTarget_ = makeAttribute "hx-target"

hxSwap_ :: Text -> Attribute
hxSwap_ = makeAttribute "hx-swap"
```