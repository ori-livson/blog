Htbuilder reduces a lot of the friction for writing inline CSS styles, which can be useful in situations where things don't have to be pretty (e.g., internal websites). However, once you want the design of parent HTML elements to determine the styles of their children, one probably should resort to working with a proper external `.css` file. In other words, htbuilder code tends to be become clunky / poorly designed when you try to inject styles into functions responsible for rendering child elements.

For example, say we have a function `render_paragraph(text)` that generates a `<p>` tag used in several parts of the webpage, but when that paragraph is a child of a special `<div>`, we want the paragraph to have `margin-left: 20px`.

I wish we could do something like:
```python
from htbuilder import styles

base_paragraph = render_paragraph(text)
special_paragraph = base_paragraph(style=styles(margin_left="20px"))
```

But this will just remove all other styles that `base_paragraph` may have. Likewise, we don't want to create a duplicate `render_special_paragraph(text)` function that adds this style. So that really just leaves patterns like:
```python
def parent():
    return div(
        "Parent",
        base_paragraph(
            extra_styles=dict(margin_left="20px"),
        ),
    )


def base_paragraph(extra_styles: dict = None):
    p_style = dict(color="red")
    p_style |= extra_styles or {}
    return p("World", style=styles(color="red"))
```

And this can get a bit cumbersome, especially since one has to be careful as to when they actually chose to render the `dict` as an inline style.