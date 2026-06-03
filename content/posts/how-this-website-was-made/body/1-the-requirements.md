
**For the reader** it's a website that:

- Is snappy, loading from just a **few kilobytes per page**.
- Is **responsive for mobile** and window resizing.
- Has **responsive dark / light themes**.
- Has **syntax highlighting of code blocks and math**.
- Supports comments with images, code, maths, etc.

**For the developer** (me) it's a website that:

- Supports authoring with Markdown, images, HTML snippets, etc.
- **Costs ~$1 USD per month to host** on AWS <sup>[1](#footnote-1)</sup>.
- **Can be deployed in seconds** by uploading a **<1Mb** zip file with a simple script.

The outcome is a website made with a recipe that mixes old and new; specifically it:

- Uses my own [Haskell program](https://github.com/ori-livson/blog) to generate the HTML.
- Has very little JavaScript save for ~100 lines for the light/dark theme toggling, a [Highlight JS](https://highlightjs.org/) script for code highlighting and [MathJax](https://www.mathjax.org/) for LateX formatting.
- Is framework free, it's just good old-fashioned HTML and vanilla CSS and JavaScript in the backseat.