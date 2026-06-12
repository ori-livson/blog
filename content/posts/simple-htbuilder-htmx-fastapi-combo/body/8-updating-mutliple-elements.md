Recall the following part of requirement 3: "when all rows are checked, the button turns into a "Deselect all" button". There are 2 events that should cause this outcome: the checking of a final unchecked checkbox, and the clicking of the "Select all" button. In the latter case, two portions of the HTML need to be updated: the whole table, and the button itself.

Because there is so little outside the table, one could just `hx-target` the `<body>`, but it's worth looking at another option: **hx-swap-oob**, which stands for</br>**hx swap out of bounds**.

The way this works, is that we mark certain elements as *'out of bounds swappable'*, and if a matching element (e.g., by element id) is returned by the server it automatically swaps out that first element. 

For example, say we render the buttons as follows:
```python
def bulk_select_button(session: SessionState) -> HtmlTag:
    all_selected = all(row.selected for row in session.rows)
    button_text = "Deselect All" if all_selected else "Select All"
    href = f"/table/{'deselect' if all_selected else 'select'}/all"
    return button(
        button_text,
        id=SELECT_ALL_ID,
        hx_post=add_session(href, session),
        hx_target=f"#{TABLE_ID}",
        hx_swap_oob="true",
    )
```

If all rows are selected, it reads "Deselect All" and hx-posts to `/table/deselect/all`, and importantly becomes `hx-swap-oob` enabled. If we then deselect a row manually --- triggering the `/table/{row_id}/toggle-select` endpoint --- attaching a copy of the "Select All" button to the response automatically causes the desired swap.

Attaching simply means string concatenation like so:

```python
def oob_response(dom: HtmlTag, oob: HtmlTag) -> HTMLResponse:
    return HTMLResponse(content=str(dom) + str(oob))
```

So, the toggle-select endpoint can work like so:
```python
@app.post("/table/row/{row_id}/toggle-select")
def select(row_id: str, session_id: str) -> HTMLResponse:
    session = SESSIONS[session_id]
    if row := get_row(session=session, row_id=row_id):
        row.selected = not row.selected
        return oob_response(
            dom=some_target(session),
            oob=bulk_select_button(session),
        )

    raise HTTPException(
        status_code=404,
        detail=f"Row ID {row_id} not found in session",
    )
```

There are yet more options for targeting multiple HTML elements, discussed by the creator of HTMX in [this short post](https://htmx.org/examples/update-other-content/).