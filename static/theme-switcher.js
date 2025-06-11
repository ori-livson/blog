/** Inspired by:
 * https://www.codemzy.com/blog/dark-mode-to-static-site */

/**
 * Elements:
 * 1. Toggle Theme Button(s) (light / dark icon; the opposite to the current theme is shown)
 * 2. Revert to OS Theme Button (shown when website theme differs from OS theme)
 * 3. OS Theme
 * 4. Browser Local Storage (Optional item key: "user-theme-preference"; possible values: "dark" | "light")
 *
 * Note: The active site theme is dictated the pressence of the "dark" class on the <html> tag
 *
 * Strategy:
 * Use the theme from 4. otherwise use 3.
 * If the user clicks 1. then we set 4., which reveals 2.
 * If the user clicks 2. then we clear 4. and refresh.
 */

const dark = "dark";
const light = "light";
let lastPreference = null;

toggleTheme({ toDark: currentPreferenceIsDark() });

function initThemeSwitcher() {
  /** Add handler for light / dark mode icon */
  document
    .getElementById("button-toggle-dark-mode")
    .addEventListener("click", function () {
      /** Flip the theme and set User Preference  */
      const toDark = !currentPreferenceIsDark();
      localStorage.setItem("user-theme-preference", toDark ? dark : light);
      toggleTheme({ toDark: !currentPreferenceIsDark() });
    });
  /** Add handler for revert to os theme icon */
  document
    .getElementById("button-revert-to-os-theme-preference")
    .addEventListener("click", function () {
      /** Remove user preference and update theme per OS Preference */
      localStorage.removeItem("user-theme-preference");
      toggleTheme({ toDark: currentPreferenceIsDark() });
    });
  /** Run watcher in the background to update them every 1s */
  setInterval(watcher, 1000);
}

function watcher() {
  toggleTheme({ toDark: currentPreferenceIsDark() });
}

function currentPreferenceIsDark() {
  const userPreference = localStorage.getItem("user-theme-preference");
  if (userPreference !== null) {
    return userPreference === dark;
  }

  /** Default to a check of the OS preference being dark */
  return window.matchMedia("(prefers-color-scheme: dark)").matches;
}

function toggleTheme({ toDark }) {
  /** Dark theme activated by adding "dark" class to <html> */
  if (toDark) {
    document.documentElement.classList.add("dark");
  } else {
    document.documentElement.classList.remove("dark");
  }

  /** We use localStorage to persistently track whether the use has set a preference
   *  In which case we reveal a button to allow the user to revert to the OS theme.
   */
  if (localStorage.getItem("user-theme-preference") === null) {
    document.documentElement.classList.remove("user-theme-preference");
  } else {
    document.documentElement.classList.add("user-theme-preference");
  }

  if (toDark === lastPreference) {
    return;
  }

  lastPreference = toDark;
  updateCSS({ toDark: toDark });
}

/** Switch href for link tags to css like
 * light.min.css <-> dark.min.css
 * light-theme.css <-> dark-theme.css
 * etc.
 * See: app/Templates.hs
 */

function updateCSS({ toDark }) {
  updateCSSHref({ linkId: "link-theme-css-vars", toDark: toDark });
  updateCSSHref({ linkId: "link-code-block-theme", toDark: toDark });
}

function updateCSSHref({ linkId, toDark }) {
  const link = document.getElementById(linkId);
  if (link == null) {
    return;
  }
  const currentTheme = link.getAttribute("href");
  const newThemeHref = toDark
    ? currentTheme.replace("light", "dark")
    : currentTheme.replace("dark", "light");

  link.setAttribute("href", newThemeHref);
}
