The basic feature of HTMX is giving HTML elements special attributes that:

- Lets them send HTTP requests on any standard [web event](https://developer.mozilla.org/en-US/docs/Web/API/Element#events), e.g., clicks, key-presses, mouse hovers, etc.

- Specifies where to put the server response --- in particular, if the server responds with an HTML snippet, you can switch out a corresponding part of the page with that snippet.

A prototypical example of HTMX is as follows.

```html
<input
  type="text"
  name="name_contains"
  placeholder="Type to filter rows by name..."
  hx-get="/table/filter"
  hx-trigger="input changed delay:300ms"
  hx-target="#table"
  hx-swap="outerHTML"
/>
```

What does it do? It makes an ordinary text input like so:
<input
  type="text"
  name="name_contains"
  placeholder="Type to filter rows by name..."
  style="width: 30ch; margin-left: 12px; margin-right: 12px;"
/>
and filters a table to only contain records whose "name" contains the contents of the input. Specifically, HTMX drives this process with the follwoing attributes:

1. (`hx-trigger="input changed"`) Every time you edit its contents of the input to `X`, it -
2. (`hx-get="/table/filter"`) fires an HTTP request `GET /table/filter?name_contains=X` then -
3. (`hx-swap="outerHTML"`) takes the HTTP response, and uses it to replace the entire HTML element that matches `id=table` (i.e., the table's outerHTML is swapped out). 

Moreover:

4. (`hx-trigger="...delay:300ms"`) Means requests are fired only after 300ms without an input change, this stops the website from overloading the server or creating jittery changes on those responses.

Typically, this functionality would have to be split-up over the following 4 pieces of code, disconnected from the HTML `<input>` tag.

1. Some JavaScript event handler would have to listen for changes to the input.
2. Some JavaScript code would have to send the HTTP request, wait for, and capture the response.
3. Some webpage state would have to be changed to trigger the replacement of the HTML surrounding `#table`.
4. Some other state would also have to keep track of how long it has been since the `<input>` tag has been changed.

Yes, a modern JavaScript framework like React could keep part 1 close to the `<input>` tag by bundling them together in a component. The logic of parts 2-3 may also be part of the component (although there would likely be a module providing the functions to do the HTTP request). Likewise, the state needed to drive parts 3 and 4 could be part of the component too. However, driving the refresh of the component according to the state change of 3 requires one to understand the often complicated lifecycle system of the framework. I've certainly worked on projects where setting up an `<input>` like the above could require editing 4 or 5 files, just on the frontend.

So, HTMX can sidestep a lot of the work and failure points, but there are two natural questions:
- What if a single client-side change requires many HTML elements to be changed in response?
- Are we really going to send HTML between the backend and frontend? Surely, passing JSON back and forth is more efficient.

HTMX has a number of fine-grained solutions for the first question that we will discuss, although just replacing the closest parent of everything that needs to be changed often works fine. Secondly, by sending HTML snippets (AKA partials), there really isn't *that much* text to pass through, it just *looks wrong*. That said, HTML compresses well with standard text compression algorithms, so one can optimise things by adding a gzip or brotli middleware to your server.