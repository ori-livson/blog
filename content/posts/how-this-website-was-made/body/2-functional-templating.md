#### *Towards HTML Templating that actually looks like HTML*

Many webdev development frameworks allow you to generate HTML using **HTML templating**. This works by letting users write the HTML they can, while leaving special *placeholders* to be filled-in by variables. HTML templating also typically includes *wrappers* around HTML that cause it to be rendered according to if-else logic or per iterations of a loop.

However, mixing HTML with programmatic logic (i.e., variables, if-else logic and loops) often leads to templates that lack readability, let alone even resembling the HTML it generates. Examples include:

**React JSX (JavaScript)**
```jsx
function TodoList() {
  const todos = ['finish doc', 'submit pr', 'nag dan to review'];
  return (
    <ul>
      {todos.map((message) => <Item key={message} message={message} />)}
    </ul>
  );
}

function Item(props) {
  return <li>{props.message}</li>;
}
```

**Hugo Templating (Go)**
```html
{{ if isset .Params "title" }}
 <title>{{ .Params.title }}</title>
{{ else }}
 <title>{{ .Site.title }}</title>
{{ end }}
```

An alternative I'm interested is: **functionally generating HTML** and laying out those functions according to the visual logic of the HTML it represents.

Before looking at a Haskell implementation in this website, let's motivate this idea with the below example using Python's [htbuilder](https://github.com/tvst/htbuilder) package.

```python
from htbuilder import div, li, ul

dom1 = div(id="container")(
    ul(_class="greetings")(
        li("hello"),
        li("hi"),
        li("whattup"),
    )
)

dom2 = div(id="container")(
    ul(_class="greetings")(
        li(greeting)
        for greeting in [
            "hello",
            "hi",
            "whattup",
        ]
    )
)
```

Printing either of `dom1` or `dom2` produces the following HTML (both indented on-save using the popular formatters: [prettier](https://prettier.io/) for HTML and [black](https://pypi.org/project/black/) for python).
```html
<div id="container">
  <ul class="greetings">
    <li>hello</li>
    <li>hi</li>
    <li>whattup</li>
  </ul>
</div>
```
Importantly, the source Python and resulting HTML have matching visual logic, i.e., you can mentally write out the HTML as your eyes follow the nesting of `dom1` or `dom2`'s definitions. Furthermore, htbuilder works great when paired with [fastapi](https://fastapi.tiangolo.com/features/#just-modern-python) for the server, and [htmx](https://htmx.org/) for interactivity and dynamically generating content. 

**Edit:** see this [newer post](/posts/simple-htbuilder-htmx-fastapi-combo/) for more details.

In this website, I used Haskell's [Lucid](https://hackage.haskell.org/package/lucid2#readme) package (see [Footnote 2](#footnote-2) for tutorial info). The indentation of Haskell's `do-blocks` mirrors the resulting HTML nesting, and seamlessly composes with other HTML returning functions. Below is how the nav-bar for this site is written.
```haskell
navBar :: HTML
navBar = do
  nav_ [class_ "navbar"] $ do
    div_ [class_ "container"] $ do
      ul_ [class_ "navbar-list"] $ do
        li_ [id_ "sandwich-li", class_ "navbar-item"] $ sandwichButton
        navLink "/" "home"
        navLink "/about" "about"
        navLink "/publications" "publications"
        navLink "/posts" "posts"
        navLink "/tags" "tags"
        navLink "/upcoming" "upcoming"
        navLink "/contact" "contact"
        div_ [class_ "expander"] ""
        li_ [id_ "theme-li", class_ "navbar-item"] $ do
          themeButton
          revertToOSThemeButton

navLink :: Text -> Text -> HTML
navLink path text = do
  li_ [class_ "navbar-item nav-toggle"] $ do
    a_ [class_ "navbar-link", href_ path] $ toHtml text

sandwichButton :: HTML
...

themeButton :: HTML
...

revertToOSThemeButton :: HTML
...
```

Note, the repeated `navLink` elements could have been generated using a `forM_` loop, but I don't think that would improve readability in this case.

**Edit:** see this [even newer post](/posts/lucid-htmx-servant-combo) that goes into much more detail about Lucid (as well as HTMX), and the perks of using functional programming languages in web development besides the usual cliché of *"if it compiles, it works"*.

I also highly, recommend looking into [elm](https://elm-lang.org/) for it's very interesting approach to developing single-page-apps (i.e., not this website), and its user-friendliness. Plus, it also features [elm-ui](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/) for replacing CSS (talk [here](https://www.youtube.com/watch?v=Ie-gqwSHQr0)), and may soon be getting a DB language Acadia, made by elm's creator Evan Czaplicki.