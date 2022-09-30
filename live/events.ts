import { Messages } from "./Messages";
import { toAction } from "./types"

// EVENTS: All via event bubbling up to document
// then when the DOM is changed, they still work

export function listenEvents(messages:Messages) {

  // Handle Click Events via bubbling
  document.addEventListener("click", function(e) {
    let el = e.target as HTMLElement

    // Find the nearest source that has a click handler
    var source:HTMLElement = el.closest("[data-on-click]");

    if (source?.dataset.onClick) {
      messages.sendAction(source.dataset.onClick)
    }
  })

  // These work on inputs, so they don't need to check for 
  document.addEventListener("input", function(e) {
    let el = e.target as HTMLInputElement
    if (el.dataset.onInput) {
      let act = el.dataset.onInput
      messages.sendActionVal(toAction(act), el.value)
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