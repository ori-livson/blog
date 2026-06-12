The final piece of this puzzle is an API server for listening to the HTMX requests and serving the response HTML that we render with htbuilder.

Here, nothing special is required, I just prefer [FastAPI](https://fastapi.tiangolo.com/tutorial/first-steps/) because it's relatively lightweight and doesn't force you into mountains of mandatory directories and config files.

One just needs to figure out what kind of HTTP requests HTMX generates (e.g., whether the data you're expecting will be in query parameters, form data, etc.).