/** Inspired by:
 * https://www.codemzy.com/blog/dark-mode-to-static-site */

// If the user's preference isn't dark, but they set Dark Mode, we will store that in local storage. So we know to give them darkness next time they load another page.
const darkPreferred = window.matchMedia("(prefers-color-scheme: dark)").matches;

const dark = "dark";
const light = "light";

if (
  localStorage.getItem("theme") === dark ||
  (!("theme" in localStorage) && darkPreferred === dark)
) {
  toggleToDark(true);
}

/**
 * #button-dark-mode uses .dark class on html
 */
window.onload = function () {
  document
    .getElementById("button-dark-mode")
    .addEventListener("click", function () {
      toggleToDark(!document.documentElement.classList.contains(dark));
    });
};

function toggleToDark(setToDark) {
  if (setToDark) {
    document.documentElement.classList.add(dark);
  } else {
    document.documentElement.classList.remove(dark);
  }

  if (darkPreferred) {
    localStorage.setItem("theme", setToDark ? dark : light);
  } else {
    localStorage.removeItem("theme");
  }

  toggleAllCSS();
}

/** Switch href for link tags to css like
 * light.min.css <-> dark.min.css
 * light-theme.css <-> dark-theme.css
 * etc.
 * See: app/Templates.hs
 */

function toggleAllCSS() {
  toggleCSSHref("theme");
  toggleCSSHref("code-block-theme");
}

function toggleCSSHref(id) {
  var currentTheme = document.getElementById(id).getAttribute("href");
  var newTheme = currentTheme.includes("light")
    ? currentTheme.replace("light", "dark")
    : currentTheme.replace("dark", "light");
  document.getElementById(id).setAttribute("href", newTheme);
}
