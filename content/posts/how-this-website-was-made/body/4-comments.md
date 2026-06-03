When it came to having comments on my website, the following questions naturally arose:

- Do commenters need accounts?
- Should comments be moderated?
- How do I store the comments?
- Should comments allow formatting, code blocks and images?

My head starting spinning straight away, would I need a server? Should I host a separate comment server (e.g., [commentario](https://comentario.app/en/))? What would that server cost? Would it be better to pay for a managed service like [Disqus](https://disqus.com/)? This all felt like I was wildly veering away from my goals with this experiment.

Then I stumbled on [Rachel Smith's blog](https://rachsmith.com/static-blog-comments/), which asked: why not just take comments via a form, manually commit them to the website's repo, and redeploy? Is it really such blasphemy as a user to not get your comment posted instantly, or as a dev to hard-code content? Considering personal blogs barely get any comments, would it not a huge waste of money or servers just for a website to be "proper"?

>**The idea of gaining so much by just taking a way a bit of functionality blew my mind... We really lack an opposite to the phrase "throwing the baby out with the bathwater".**

Afterwards, I found [Ryan HBM's blog](https://www.richyhbm.co.uk/posts/using-github-issues-as-comments-for-a-static-site/), which found an ingenious way to host comments in Rachel's style. Simply use the GitHub issues of your repository as the form the submitting comments, and build the website by loading those comments using the GitHub API. The benefit here is GitHub handles the tough work of formatting your comments, images, GIFs, etc., providing moderation, storage, accounts, and more.

My Haskell code for reading GitHub comments can be found [here](https://github.com/ori-livson/blog/blob/master/app/GitHub.hs).