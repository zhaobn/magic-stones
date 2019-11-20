
const consentBtn = document.getElementById("consent-btn");
const consentText = document.getElementById("consent-info");

consentText.addEventListener("scroll", () => checkScrollHeight(consentText, consentBtn), false);
consentBtn.onclick = () => location.href = "reminder.html"

function checkScrollHeight(text, btn){
  ((text.scrollTop + text.offsetHeight) >= text.scrollHeight) ? btn.disabled = false : null;
}
