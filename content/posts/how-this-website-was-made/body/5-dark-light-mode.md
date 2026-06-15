For themes, I found this great [blog post by Codemzy](https://www.codemzy.com/blog/dark-mode-to-static-site), that in 18 lines of JavaScript manages a user's theme preference via a toggle-able "dark" class in the root `<html>` as well as the browser's local storage for persistence.

What's nice about this is that it's a totally isolated feature in an isolated JavaScript file `theme-switcher.js`, which we can orchestrate via a `window.onload = function () {...}` callback in a `main.js` included after `theme-switcher.js`.

Again, yes, it's a bit blasphemous having this hard-coding, special file ordering and the CSS needing to know about the special "dark" class, etc. But the simplicity is just worth it; the hard-coding and coupling always ends up somewhere.

I extended Codemzy's approach in the following way:

- I use the presence of the "dark" class of the `<html>` value, to toggle the visibility of a sun / moon button icon (to indicate the next click switching to the light / dark theme respectively)
- These button clicks also toggles a .css file include between `light-theme.css` and `dark-theme.css`, which are just short lists of CSS variables (see the [Look and Feel section](#section-7)).
- I add an extra "OS Theme" button which clears any user preference, reverting the page to the OS preferred theme.
- Finally, I poll the user / OS preferences every 0.5 seconds to apply any updates.

You can view the JavaScript [here](https://github.com/ori-livson/blog/blob/master/static/theme-switcher.js).

**Note:** the files `light-theme.css` and `dark-theme.css` primarily contain CSS variables, (e.g., `--text-color: #FFF;` for the dark theme), a nifty trick there is adding theme specific CSS rules to these files, e.g.,
```css
svg,
img {
  filter: invert(1) hue-rotate(180deg);
}
```
for inverting the colours of `<image>` and `<svg>` elements in dark mode. 