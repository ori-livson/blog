const rssLink = document.getElementById("rss-link");
const rssToast = document.getElementById("rss-toast");

const rssUrl = rssLink.href;

rssLink.addEventListener("click", async (event) => {
  event.preventDefault();

  try {
    await navigator.clipboard.writeText(rssUrl);
    showRssToast(
      `RSS feed URL ${rssUrl} copied!\nPaste it into your favourite RSS reader.`,
    );
  } catch (error) {
    showRssToast("Could not copy the RSS URL.");
    console.error("Clipboard error:", error);
  }
});

function showRssToast(message) {
  rssToast.textContent = message;
  rssToast.classList.add("show");

  clearTimeout(rssToast.timeout);
  rssToast.timeout = setTimeout(() => {
    rssToast.classList.remove("show");
  }, 4000);
}
