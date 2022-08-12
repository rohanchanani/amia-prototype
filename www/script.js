
const updateCheckboxes = (allChange=false) => {
  console.log(allChange);
  let pattern = document.getElementById("Search").value;
  let regExp = new RegExp(pattern, "i");
  for (let checkbox of document.getElementById("Options").children[1].children) {
    let innerBox = checkbox.children[0].children[0];
    if (innerBox.checked || (innerBox.value.match(regExp) && pattern != "")) {
      checkbox.removeAttribute("style");
    } else {
      checkbox.setAttribute("style", "display: none;");
    }
  }
}
document.getElementById("Search").addEventListener("keyup", updateCheckboxes);
document.getElementById("Options").addEventListener("click", updateCheckboxes);
document.getElementById("Options").addEventListener("change", updateCheckboxes);
document.getElementById("Dimension").addEventListener("change", updateCheckboxes);
document.getElementById("GroupedActualGraph").addEventListener("change", updateCheckboxes);