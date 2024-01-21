/** https://www.codemzy.com/blog/dark-mode-to-static-site */

function setTheme(dark, preference) {
  if (dark) {
    preference !== "dark"
      ? localStorage.setItem("theme", "dark")
      : localStorage.removeItem("theme");
    document.documentElement.classList.add("dark");
    updateThemeCSS();
  } else if (!dark) {
    preference !== "light"
      ? localStorage.setItem("theme", "light")
      : localStorage.removeItem("theme");
    document.documentElement.classList.remove("dark");
    updateThemeCSS();
  }
}

const preference = window.matchMedia("(prefers-color-scheme: dark)").matches
  ? "dark"
  : "light";
if (
  localStorage.getItem("theme") === "dark" ||
  (!("theme" in localStorage) && preference === "dark")
) {
  setTheme(true, preference);
}
window.onload = function () {
  document
    .getElementById("button-dark-mode")
    .addEventListener("click", updateTheme);
};

function updateTheme() {
  setTheme(!document.documentElement.classList.contains("dark"), preference);
}

function updateThemeCSS() {
  toggleCSS("theme");
  toggleCSS("code-block-theme");
}

function toggleCSS(id) {
  var currentTheme = document.getElementById(id).getAttribute("href");
  var newTheme = currentTheme.includes("light")
    ? currentTheme.replace("light", "dark")
    : currentTheme.replace("dark", "light");
  document.getElementById(id).setAttribute("href", newTheme);
}
