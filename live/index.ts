import { hydrate, patch, render, DOMNode, m, VNode, Flags } from 'million';
import { fromDomNodeToVNode, fromStringToDomNode } from 'million/utils';
import { listenEvents } from './events';
import { WEBSOCKET_ADDRESS, Messages } from './Messages'
import { INIT_PAGE, INIT_STATE, State } from './types';
import { fromVDOM, VDOM } from './vdom'


const CONTENT_ID = "yeti-root-content"

console.log("VERSION 2", INIT_PAGE, INIT_STATE)

var currentState:State = INIT_STATE
var rootElement:DOMNode

const messages = new Messages()
messages.onUpdate(update)
messages.onClose(() => {
  // reconnect on close
  setTimeout(() => messages.connect(INIT_PAGE, currentState), 1000)
})
messages.connect(INIT_PAGE, currentState)

listenEvents(messages)


function update(newState:State, params:string, vdom:VDOM) {

  // This is stripping tab characters in the data attributes, can't use tab as a delimiter
  // let dom = fromStringToDomNode(html)
  // let vnode = fromDomNodeToVNode(dom)
  // rootElement = patch(rootElement, vnode)
  let newRoot = m("div", {"id": CONTENT_ID}, fromVDOM(vdom))
  rootElement = patch(rootElement, newRoot)

  currentState = newState

  // wait, is this pushing in a circle?
  updateHistory(newState, params, vdom)
}


function updateHistory(newState:State, params:string, vdom:VDOM) {
  if (("?" + params) != location.search) {
    console.log("New History", params, location.search)
    let url = location.origin + location.pathname + "?" + params
    history.pushState([newState, params, vdom], "", url)
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

  // hydreate the content
  let initContent = fromDomNodeToVNode(rootElement)
  console.log('INIT', initContent)
  rootElement = patch(rootElement, initContent)
})



