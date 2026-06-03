#### *Does anyone else feel like they suck at CSS?*

For the look and feel of my site, I drew a lot from [Niklas Fasching's blog](https://niklasfasching.de/posts/just-enough-css/).
In particular, it introduced me to [Skeleton CSS](http://getskeleton.com/), a **400 line CSS file** that defines nice starting points for spacing, typography, etc. Some nice additions I made to it are:

- CSS Variables:
  ```css
  :root {
    --bg-color: #fafafa;
    --bg-secondary-color: #f8f8f8;
    --bg-navbar-color: #fff;
    --text-color: #000;
    --text-secondary-color: #555;
    --primary-color: #1eaedb;
    --primary-button: #33c3f0;
    --border-color: #e1e1e1;
    --strong-color: #0fa0ce;
  }
  ```
  used throughout my .css files like so:
  ```css
  body {
    background-color: var(--bg-color);
  }
  ```

- Converting most sizes and spacing to font-based measures, i.e., `rem` and `em`.