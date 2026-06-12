[Htbuilder](https://github.com/tvst/htbuilder) is a python package for generating HTML by effectively exposing a function for every HTML tag. HTML attributes can be passed as arguments to those functions, and critically, so can child-HTML elements.

For example, the following htbuilder expression:

```python
from htbuilder import div, li, ul

div(id="container")(
    ul(_class="greetings")(
        li("hello"),
        li("hi"),
        li("whattup"),
    )
)
```
Generates the following HTML:
```html
<div id="container">
  <ul class="greetings">
    <li>hello</li>
    <li>hi</li>
    <li>whattup</li>
  </ul>
</div>
```
(I.e., if we print or cast the expression to `str`).

This first example, satisfies the following key tenant of good HTML generation:

>**Tenant 1:** The function arguments indent to match the nesting of the HTML; that way, the source Python and resulting HTML have matching **visual logic**. In other words, you can mentally write out the HTML as your eyes follow the nesting of the code. 

The way I reliably get this indenting is by setting up the Python [black](https://pypi.org/project/black/) formatter to format on-save (the reference HTML formatting was produced using the [prettier](https://prettier.io/) formatter).

However, the functionally generating HTML really shines when rendering HTML based on **programmatic logic** (i.e., variables, if-else logic and loops). For example, we can rewrite the above example as follows.

```python
div(id="container")(
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

This second example, satisfies another key tenant of good HTML generation:

>**Tenant 2:** Any mixing of programmatic logic with HTML should not make any surrounding HTML less readable, and the final product should still have a matching visual logic to the HTML it generates.

This property is only somewhat satisfied in templated approaches such as JSX (React), Jinja (Python), Hugo (Go) due to programmatic parts cluttering the solutions with brackets and other special symbols (see the examples in [Footnote 2](#footnote-2)).

Moreover, unlike many templated solutions, the above expressions can be assigned to variables, or the return value of functions. This allows us to express the high level design of our websites by composing high level functions. For example, in the full website example we're about to overview, the homepage is rendered from a top-level function like so.

```python
from htbuilder import HtmlTag, form, h3, head, html, p, script, ...

def homepage(session: SessionState) -> HtmlTag:
    return html(
        head(script(src="/static/htmx.min.js")),
        body(
            h3("HTMX Demo"),
            form(
                name_contains_input(session),
                bulk_select_button(session),
            ),
            main_table(session),
            submit_button(session),
            p(id="result"),
        ),
    )
  ```

Note, When HTML elements or attributes are python keywords (e.g., `input` and `class`), they are prefixed with an underscore (e.g., `_input` and `_class`). Likewise, underscores replace hyphens when they would be invalid in Python syntax, e.g., `_input(hx_get="/table/filter")` generates `<input hx-get="/table/filter"></input>.


Finally, adding in HTMX is just a matter of adding more arguments (i.e., HTML attributes), e.g., `hx_get="/table/filter"`.