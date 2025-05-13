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

toggleTheme(netThemePreference(), false);

function initThemeSwitcher() {
  document
    .getElementById("button-dark-mode")
    .addEventListener("click", function () {
      toggleTheme(!netThemePreference(), true);
    });
  document
    .getElementById("revert-dark-mode")
    .addEventListener("click", function () {
      localStorage.removeItem("theme");
      toggleTheme(netThemePreference(), false);
    });

  setInterval(syncTheme, 1000);
}

function syncTheme() {
  console.log("Watcher!");
  toggleTheme(netThemePreference(), false);
}

function netThemePreference() {
  const storagePreference = localStorage.getItem("theme");
  if (storagePreference !== null) {
    return storagePreference === dark;
  }
  const osPreference = window.matchMedia("(prefers-color-scheme: dark)").matches
    ? dark
    : light;
  return osPreference === dark;
}

function toggleTheme(toDark, overriding) {
  if (toDark) {
    document.documentElement.classList.add(dark);
  } else {
    document.documentElement.classList.remove(dark);
  }

  if (overriding) {
    localStorage.setItem("theme", toDark ? dark : light);
  }

  if (localStorage.getItem("theme") === null) {
    document.documentElement.classList.remove("overridden");
  } else {
    document.documentElement.classList.add("overridden");
  }

  toggleAllCSS(toDark);
}

/** Switch href for link tags to css like
 * light.min.css <-> dark.min.css
 * light-theme.css <-> dark-theme.css
 * etc.
 * See: app/Templates.hs
 */

function toggleAllCSS(toDark) {
  toggleCSSHref("theme", toDark);
  toggleCSSHref("code-block-theme", toDark);
}

function toggleCSSHref(id, toDark) {
  const currentTheme = document.getElementById(id).getAttribute("href");
  const newTheme = toDark
    ? currentTheme.replace(light, dark)
    : currentTheme.replace(dark, light);

  document.getElementById(id).setAttribute("href", newTheme);
}
