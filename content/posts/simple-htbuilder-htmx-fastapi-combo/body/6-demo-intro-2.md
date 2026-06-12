The key requirements are:

1. Typing in the text input should instantly filter the table to rows whose "name" contains that text anywhere (case insensitively).
2. Clicking select all, should check all rows in the table, and when all rows are checked, the button turns into a "Deselect all" button.
3. Clicking the arrow symbol of any column sorts the table by that column (up arrow ascending) and reverses the ordering if clicked again; the arrow also turns red to indicate how the table is sorted.
4. Clicking submit prints the names of all the selected rows (excluding all filtered-out rows).

Implementing these requirements alone is straightforward, but one has to be careful when combining them. For example, requirement 1 requires us to first render elements like so:
```python
from htbuilder import input_, table, thead, tbody, trow, th, td, tr

TABLE_ID = "table"

def name_contains_input() -> HtmlTag:
    return input_(
        type="text",
        name="name_contains",
        placeholder=placeholder,
        hx_get="/table/filter",
        hx_trigger="input changed delay:300ms",
        hx_target=f"#{TABLE_ID}",
        hx_swap="outerHTML"
    )

def main_table(rows: list) -> HtmlTag:
    ...
    heading_row: HtmlTag = thead(tr(...))
    rows: HtmlTag = tbody(make_row(row) for row in rows)
    return table(
        heading_row,
        rows,
        id=TABLE_ID,
    )
```

and an API endpoint like so:
```python
from fastapi import FastAPI
from fastapi.responses import HTMLResponse

app = FastAPI()

@app.get("/table/filter")
def filter_names(name_contains: str, session_id: str) -> HTMLResponse:
    rows = [row for row in get_rows() if name_contains.lower() in row.name.lower()]
    return HTMLResponse(content=str(main_table(rows)))
```

However, if we then implement requirement 2 or 3, we presumably make another `hx-get` on `main_table` such that we don't forget about our name filter. A naive solution is loading up our endpoints with query parameters, but this becomes cumbersome quickly. Instead, we will use **session state** retrievable from a single `session_id` attached to every request.