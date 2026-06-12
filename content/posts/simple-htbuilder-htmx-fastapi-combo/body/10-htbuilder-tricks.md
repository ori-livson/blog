HTBuilder has a number of additional interesting ways to initialise HTML elements by way of overwriting the `__getitem__` method (i.e., `obj[idx]` syntax) and `__call__` method (i.e., `obj(arg)` syntax) of python objects.

Firstly, if one has a variable `element: HtmlTag` that already has children and HTML attributes, one can call `element` again to add more elements and attributes, for example, if we have:
```python
def generate_checkbox(row: Row, session: SessionState) -> HtmlTag:
    attrs = dict(
        type="checkbox",
        name="selected",
        value=row.id,
        hx_post=add_session(f"/table/row/{row.id}/select", session),
    )
    # need an absence of the checked attribute to render an unchecked checkbox
    if row.selected:
        attrs["checked"] = "checked"  # any string works

    return input_(**attrs)
```

We can give it a `id` attribute afterwards by calling:

```python
base_checkbox = generate_checkbox(row, session)
special_checkbox = base_checkbox(id="special_id")
```

**Note:** adding attributes to a second call will overwrite existing, matching attributes in the `HtmlTag`.


Secondly, if we have a parent element `parent: HtmlTag` and list of child elements `[element1, element2, element3]: list[HtmlTag]`, we can initialise the parent via the expression `parent[element1, element2, element3]` rather than the more cumbersome `parent(*[element1, element2, element3])`.