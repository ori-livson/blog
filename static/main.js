window.onload = function () {
  initThemeSwitcher();

  document
    .getElementById("sandwich-button")
    .addEventListener("click", function () {
      document.querySelectorAll(".nav-toggle").forEach((item) => {
        item.classList.toggle("show");
      });
      document.querySelectorAll("#theme-li").forEach((item) => {
        item.classList.toggle("show");
      });
    });
};
