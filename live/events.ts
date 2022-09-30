import { Messages } from "./Messages";


export function listenEvents(messages:Messages) {

  // Handle Click Events via bubbling
  document.addEventListener("click", function(e) {
    let el = e.target as HTMLElement

    // Find the nearest source that has a click handler
    var source:HTMLElement = el.closest("[data-on-click]");

    // console.log("Click", source)

    if (source?.dataset.onClick) {
      messages.sendAction(source.dataset.onClick)
    }
  })

  // These work on inputs, so they don't need to check for 
  document.addEventListener("input", function(e) {
    let el = e.target as HTMLInputElement
    if (el.dataset.onInput) {
      let val = JSON.stringify(el.value)
      messages.sendActionVal(el.dataset.onInput, val)
    }
  })

  document.addEventListener("keypress", function(e) {
    let el = e.target as HTMLInputElement
    if (e.code == "Enter" && el.dataset.onEnter) {
      console.log("ENTER!", el.dataset.onEnter)
      messages.sendAction(el.dataset.onEnter)
    }
  })

}