[HTMX](https://htmx.org/) is a JavaScript library that lets you create interactive websites entirely out of a single backend server that generates HTML snippets. Critically, this can mean avoiding a lot of the complexity surrounding passing data back and forth between a frontend client and a backend server, let alone setting up a frontend client.

I've personally been using it often at my job to setup internal, data entry and management websites with Python, and I have really been enjoying it. However, it took a few tries to get the solution to scale, i.e., I had to figure out some design patterns to prevent other sorts of complexities popping up and bogging the development of those projects.   

So, this post is tutorial on that, and will hopefully save you some of the troubles I encountered. To explain this, we'll go over a simple example project I've developed, available in this [public repo](https://github.com/ori-livson/simple-htbuilder-htmx-fastapi-combo).

Moreover, this post is also about encouraging another approach I really enjoy - **functional HTML generation**. In other words, Python + HTMX users are often steered towards using HTML templating languages such as [jinja](https://jinja.palletsprojects.com/en/stable/templates/#synopsis) and [mako](https://www.makotemplates.org/), and I've always found HTML templating very lacking in the following two senses:

1. These templates often become cluttered, unreadable, and don't look like the HTML they actually generate.
2. They often don't have great support for composing templates like one can with pure functions or components in modern JS frameworks.

What I'm interested in, is simply generating HTML with functions, and when it's done right (with a good code formatter), the functions can be automatically indented to mirror the HTML you expect on the screen. I'be written a bit about that in an [earlier post](/posts/how-this-website-was-made/) about how this website was made using Haskell, and I plan to write a version of this post for Haskell in the future. Note, my first introduction to this was through [elm](https://elm-lang.org/), which I highly recommend looking into too.<sup>[1](#footnote-1)</sup>