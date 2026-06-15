The implementation involves using Haskell's [Pandoc](https://pandoc.org/) package to convert Markdown to [Lucid](https://hackage.haskell.org/package/lucid2#readme) HTML. A useful, unintended consequence is that writing raw HTML into markdown files just functions as normal HTML in the corresponding converted HTML. For example, converting:
```markdown
# Hello

Some text

<svg>...</svg>

- Item 1
- Item 2
- Item 3
```
Produces:
```html
<h1>Hello</h1>
<p>Some text</>
<svg>...</svg>
<li>
  <ul>Item 1</ul>
  <ul>Item 2</ul>
  <ul>Item 3</ul>
</li>
```

See [LucidUtils.hs](https://github.com/ori-livson/blog/blob/master/app/LucidUtils.hs).

I haven't had great success converting .tex files to HTML via MathJax - especially when latex imports and new commands are used. What I've found works best is simply pasting my latex into [Up Math](https://upmath.me/), which converts it into Markdown or HTML. It even handles things like [Tikzcd](https://ctan.math.washington.edu/tex-archive/graphics/pgf/contrib/tikz-cd/tikz-cd-doc.pdf) well.