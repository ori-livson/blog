/** Inspired by:
 * https://www.codemzy.com/blog/dark-mode-to-static-site */

/**
 * Elements:
 * 1. Toggle Theme Button
 * 2. Revert to OS Theme Button (hidden by default)
 *
 * 3. OS Theme
 * 4. Browser Local Storage
 *
 * NB: The theme is dictated by "dark" or "light" class given to the root <html> tag
 *
 * Strategy:
 * Use the theme from 4. otherwise use 3.
 * If the user clicks 1. then we set 4. and add "overriden" class to <html> and reveal 2.
 * If the user clicks 2. then we clear 4. and refresh.
 */

const dark = "dark";
const light = "light";
let lastPreference = null;

toggleTheme(isNetPreferenceDark(), false);

function initThemeSwitcher() {
  /** Add handler for night / dark mode icon */
  document
    .getElementById("button-dark-mode")
    .addEventListener("click", function () {
      toggleTheme(!isNetPreferenceDark(), true);
    });
  /** Add handler for revert to os theme icon */
  document
    .getElementById("revert-dark-mode")
    .addEventListener("click", function () {
      localStorage.removeItem("theme");
      toggleTheme(isNetPreferenceDark(), false);
    });
  /** Run watcher in the background every 1s */
  setInterval(watcher, 1000);
}

function watcher() {
  toggleTheme(isNetPreferenceDark(), false);
}

function isNetPreferenceDark() {
  const buttonPreference = localStorage.getItem("theme");
  if (buttonPreference !== null) {
    return buttonPreference === dark;
  }
  const osPreference = window.matchMedia("(prefers-color-scheme: dark)").matches
    ? dark
    : light;
  return osPreference === dark;
}

function toggleTheme(toDark, removeButtonPreference) {
  if (toDark) {
    document.documentElement.classList.add(dark);
  } else {
    document.documentElement.classList.remove(dark);
  }

  if (removeButtonPreference) {
    localStorage.setItem("theme", toDark ? dark : light);
  }

  if (localStorage.getItem("theme") === null) {
    document.documentElement.classList.remove("non-os-preference");
  } else {
    document.documentElement.classList.add("non-os-preference");
  }

  if (toDark === lastPreference) {
    return;
  }

  lastPreference = toDark;
  updateCSS(toDark);
}

/** Switch href for link tags to css like
 * light.min.css <-> dark.min.css
 * light-theme.css <-> dark-theme.css
 * etc.
 * See: app/Templates.hs
 */

function updateCSS(toDark) {
  updateCSSHref("theme", toDark);
  updateCSSHref("code-block-theme", toDark);
}

function updateCSSHref(id, toDark) {
  const currentTheme = document.getElementById(id).getAttribute("href");
  const newTheme = toDark
    ? currentTheme.replace(light, dark)
    : currentTheme.replace(dark, light);

  document.getElementById(id).setAttribute("href", newTheme);
}
