Personally, I think the frustrating moments web development come from when a website is supposed to be doing something (e.g., firing off an HTTP request, responding to an event, re-rendering something) but nothing happens, not even an error.

In the implementation of this post, there are still 2 such failure points I struggled with.

1. (More so) generating a `hx-target` to an element that doesn't exist in the DOM.
2. (Less so) generating a `hx-get` or `hx-post` to an endpoint that doesn't exist in the API.

The first may be solvable with clever unit test on rendered HTML, e.g., for every `hx-target=#element` check that `id=element` exists somewhere.

The second, could be solved if there was a way to generate the API endpoints from the `hx-target`, e.g., being able to define functions like:
```haskell
genEndpoint :: String -> API
genEndpoint x = (SpecialApiPath x) :> ModelRequestBody :> PostHtmlResponse
```

Finally, a version of these experiments for CSS (i.e., some alternative or way to generate it that is type safe, readable and without silent errors) would be incredible. The closest thing I've found is [elm-ui](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/) (with [this talk](https://www.youtube.com/watch?v=Ie-gqwSHQr0)) as an alternative to CSS for [elm](https://elm-lang.org/). It actually allows you to center a div with `centerX` and `centerY` attributes!