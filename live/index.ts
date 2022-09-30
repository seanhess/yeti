import { hydrate, patch, render, DOMNode } from 'million';
import { fromDomNodeToVNode, fromStringToDomNode } from 'million/utils';
import { listenEvents } from './events';
import { WEBSOCKET_ADDRESS, Messages } from './Messages'
import { DELIMITER, INIT_PAGE, INIT_STATE, State } from './types';



console.log("VERSION 1", INIT_PAGE, INIT_STATE)

var currentState:State = INIT_STATE
var rootElement:DOMNode

const messages = new Messages(DELIMITER)
messages.onUpdate(update)
messages.onClose(() => {
  // reconnect on close
  setTimeout(() => messages.connect(INIT_PAGE, currentState), 1000)
})
messages.connect(INIT_PAGE, currentState)

listenEvents(messages)


function update(newState:State, params:string, html:string) {

  // This is stripping tab characters in the data attributes, can't use tab as a delimiter
  let dom = fromStringToDomNode(html)
  let vnode = fromDomNodeToVNode(dom)

  // TODO replace million.js with new version.
  // This works, but it REALLY doesn't like the unclosed input tags from lucid
  rootElement = patch(rootElement, vnode)
  currentState = newState

  // wait, is this pushing in a circle?
  updateHistory(newState, params, html)
}


function updateHistory(newState:State, params:string, html:string) {
  if (("?" + params) != location.search) {
    console.log("New History", params, location.search)
    let url = location.origin + location.pathname + "?" + params
    history.pushState([newState, params, html], "", url)
  }
}

// History events
window.addEventListener("popstate", function(e) {
  let [newState, params, html] = e.state
  update(newState, params, html)
})

window.addEventListener("load", function() {
  console.log("docload")
  rootElement = document.getElementById("yeti-root-content")

  let firstChild = rootElement.firstChild as DOMNode
  let initContent = fromDomNodeToVNode(firstChild)
  rootElement = patch(rootElement, initContent)
})

// EVENTS: All via event bubbling up to document
// then when the DOM is changed, they still work

