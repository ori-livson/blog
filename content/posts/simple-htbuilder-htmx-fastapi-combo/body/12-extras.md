Here are some HTMX + Htbuilder additions that you might find yourself needing for similar, real-world projects.

**Loading Spinners**

Simply add the following style globally:
```css
.htmx-indicator {
    opacity: 0;
    transition: opacity 500ms ease-in;
    z-index: -9999;
}
.htmx-request .htmx-indicator {
    opacity: 1;
    z-index: 9999;
}
.htmx-request.htmx-indicator {
    opacity: 1;
    z-index: 9999;
}
```

And include element like so in your webpage:

```python
from htbuilder import HtmlTag, div, img, styles

SPINNER_ID = "spinner"

def spinner() -> HtmlTag:
    overlay_style = styles(
        position="fixed",
        top=0,
        left=0,
        width=percent(100),
        height=percent(100),
        display="flex",
        justify_content="center",
        align_items="center",
        background=rgba(0, 0, 0, 0.1),
    )

    spinner_style = styles(
        width=px(200),
        height=px(200),
    )
    return div(id=SPINNER_ID, style=overlay_style, _class="htmx-indicator")(
        img(id=SPINNER_ID, src="/static/spinner.gif", style=spinner_style),
    )
```

It can then be used in HTMX enabled elements by adding the attribute `hx_indicator=f"#{SPINNER_ID}"`

**Use with HTTPS**

If your server runs with HTTPS enabled, you need the following `<meta>` tag in your `<head>`:
```python
from htbuilder import meta

meta(http_equiv="Content-Security-Policy", content="upgrade-insecure-requests")
```

**Nice Dropdowns with select2**

While I love vanilla, HTML, CSS and JS when possible, select inputs (AKA dropdowns) are very lacking (i.e., one typically type to filter and multiselect functionality). For this, I've found the JS library [select2](https://select2.org/getting-started/basic-usage/) works well to close this gap, however, it needs a bit of help to work with HTMX, namely, serve the [select2 css and js files](https://select2.org/getting-started/installation/) and add the following script.

```javascript
function refreshSelect2() {
    // Initialise Select2 drop downs
    $('.select2-element').select2();

    // Ensure Select2 triggers HTMX events
    $('select').on('select2:select', function (e) {
        $(this).closest('select').get(0).dispatchEvent(new Event('change'));
    });
}

$(document).ready(function() {
    refreshSelect2();
});

// Reinitialize Select2 after the content has been updated by HTMX
document.addEventListener('htmx:afterSettle', function (event) {
    refreshSelect2();
});
```

Then you can create compatible dropdowns with something like:
```python
from dataclasses import dataclass, field
from htbuilder import select

@dataclass
class Select:
    name: str
    selected: str | list[str] | None = None  # list for multi_select
    options: list[str] = field(default_factory=list)
    mutli_select: bool = False
    endpoint: str
    target_id: str

def select_element(s: Select):
    options = []
    for val in s.options:
        option_attrs = {"value": val}

        is_selected = (
            s.selected is not None
            and (val in s.selected and s.mutli_select)
            or (val == s.selected and not s.mutli_select)
        )

        if is_selected:
            option_attrs["selected"] = "selected"
        options.append(option(**option_attrs)(val))

    select_attrs = {
        "_class": "select2-element",
        "name": s.name,
    }
    if s.mutli_select:
        select_attrs["multiple"] = "multiple"  # the pressence of just attr key is all we need
    elif s.on_change:
        select_attrs |= dict(
            data_hx_get=s.endpoint,
            data_hx_trigger="change",
            data_hx_target=f"#{s.target},
        )

    return select(**select_attrs)(*options)
```