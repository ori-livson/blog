Our general procedure for keeping track of client state is to define classes:

```python
@dataclass
class Row:
    name: str
    selected: bool  # checkbox column


@dataclass
class SessionState:
    id: str  # referenced by ?session_id=x in allrequests
    name_contains: str  # Requirement 1
    rows: list[Row]
    sort_by: tuple[str, bool]  # Requirement 3 (attr of Row, ascending?)

    def sort(self):
        sort_col, sort_ascending = self.sort_by
        self.rows = sorted(
            self.rows,
            key=lambda r: asdict(r)[sort_col],
            reverse=not sort_ascending,
        )
```

And having the API assign each user a `session_id` used to access their session state, e.g., via a flow like:

```python
from uuid import uuid4

from fastapi import FastAPI, HTTPException
from fastapi.responses import HTMLResponse, RedirectResponse

SESSIONS: dict[str, SessionState] = {}

@app.get("/")
def render_homepage(session_id: str | None = None) -> HTMLResponse:
    if not session_id:
        # If first time visiting, assign a session_id and attach it to their URL
        session_id = generate_session_id()
        return RedirectResponse(
            url=f"/?session_id={session_id}",
        )

    if session_id not in SESSIONS:
        raise HTTPException(
            status_code=404,
            detail=f"Session ID {session_id} not found.",
        )

    return response(
        dom=homepage(session=SESSIONS[session_id]),
    )

def generate_session_id() -> str:
    result = str(uuid4())
    SESSIONS[result] = initialise_session(session_id=result)
    return result
```

Then, our HTMX powered endpoints update the session before generating response HTML, e.g.,

```python
@app.get("/table/filter")
def filter_names(name_contains: str, session_id: str) -> HTMLResponse:
    # Render a table with the updated name filter
    session = SESSIONS[session_id]
    session.name_contains = name_contains
    return response(
        dom=main_table(session=session),
    )
```

**Note:** if the session state contains sensitive data, one should not use query parameters to manage session ids, but rather something like authorization headers, or signed, encrypted tokens in secure cookies.