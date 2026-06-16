Putting aside Haskell for a moment, the two main ideas I'm playing with here are:

1. Programmatically generating HTML with code that really *looks like* the HTML it outputs. In other words, generating HTML with the full logic of a programming language (conditionals, loops, composition, nesting, abstraction) but not at the cost of the readability of HTML. This is something I believe HTML templating languages such as jinja and JSX often fall short at.

2. HTMX as a means to avoiding writing front-end code dedicated to: event listeners, managing HTTP requests, serialising and deserialising request and response objects, state management, re-rendering elements, etc. In other words, that sort of code often adds a lot of code volume, fragmentation, complexity and failure points.

I go into these two ideas in more detail in my [earlier post](/posts/simple-htbuilder-htmx-fastapi-combo) about a similar stack in Python.

As for the Haskell implementation:

- **Lucid** is simply the best implementation I've seen in terms of idea 1. In particular, HTML nesting is mirrored by the indented-do notation, e.g.,
```html
<div id="container">
  <ul class="greetings">
    <li>hello</li>
    <li>hi</li>
    <li>whattup</li>
  </ul>
</div>
```
Can be rendered by:

```haskell
import Lucid

greetings :: Html ()
greetings = 
  div_ [id_ "container"] $ do
    ul_ [class_ "greetings"] $ do
      li_ "hello"
      li_ "hi"
      li_ "whattup"
```

In fact, the combination of the do-notation and absence of bracketing enabled by Haskell's ML-style syntax can lead to HTML generating functions that are more readable than the HTML itself. This is furthered enhanced by Haskell's unparalleled type safety and features for composition (e.g., partial function application).

- **Servant** is a very interesting framework for developing REST APIs. In particular, its "API as a type" concept adds very thorough type safety to API endpoint paths, query parameters, request bodies, response types, etc. It is especially powerful when those HTTP types link to type-safe query generation (e.g., via the package [postgres-typed](https://github.com/dylex/postgresql-typed#complete-example)).

However, compilation errors with Servant can be quite hard to troubleshoot, so if you want a simpler API framework, I hear [scotty](https://hackage.haskell.org/package/scotty) is good.