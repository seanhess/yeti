import { hydrate, patch, render, DOMNode } from 'million';
import { fromDomNodeToVNode, fromStringToDomNode } from 'million/utils';

declare var yetiInit: {
    state: string;
    page: string;
    delimiter: string;
  }

var currentState:string;

console.log("VERSION 1")


const PORT = 3031
const HOST = location.hostname
const PATH = location.pathname

var socket:WebSocket;
var rootElement:DOMNode

console.log("Connecting: ", HOST, PORT)
open()

function open() {
  console.log("Opening...")
  socket = new WebSocket('ws://' + HOST + ':' + PORT)

  // Connection opened
  socket.addEventListener('open', (event) => {
      console.log("Open, register: ", yetiInit.state)

      // 1. send our initial state to register
      // TODO better page encoding
      currentState = yetiInit.state
      socketSend([JSON.stringify(yetiInit.page), currentState]);
  });

  // Listen for messages
  socket.addEventListener('message', (event) => {
      let [newState, params, html] = event.data.split("\n")

      update(newState, params, html)

      let url = location.origin + location.pathname + "?" + params
      history.pushState([currentState, params, html], "", url)
  });

  socket.addEventListener('close', (e) => {
    console.log("Closed")
    setTimeout(() => open(), 1000)
  });

  socket.addEventListener('error', (e) => {
    console.log("Error", e)
  });
}

function update(newState:string, params:string, html:string) {
  // console.log("MESSAGE", html)
  // // This is stripping tab characters in the data attributes
  let dom = fromStringToDomNode(html)
  let vnode = fromDomNodeToVNode(dom)

  // This works, but it REALLY doesn't like the unclosed input tags from lucid
  rootElement = patch(rootElement, vnode)
  currentState = newState
}

window.addEventListener("popstate", function(e) {
  let [newState, params, html] = e.state
  update(newState, params, html)
})

function socketSend(lines:string[]) {
  socket.send(lines.join("\n"))
}

window.addEventListener("load", function() {
  console.log("docload")
  rootElement = document.getElementById("yeti-root-content")

  let firstChild = rootElement.firstChild as DOMNode
  let initContent = fromDomNodeToVNode(firstChild)
  rootElement = patch(rootElement, initContent)
})


// EVENTS: All via event bubbling up to document
// then when the DOM is changed, they still work


// Handle Click Events via bubbling
document.addEventListener("click", function(e) {
  let el = e.target as HTMLElement

  // Find the nearest source that has a click handler
  var source:HTMLElement = el.closest("[data-on-click]");

  // console.log("Click", source)

  if (source?.dataset.onClick) {
    socket.send(source.dataset.onClick)
  }
})


// These work on inputs, so they don't need to check for 
document.addEventListener("input", function(e) {
  let el = e.target as HTMLInputElement
  if (el.dataset.onInput) {
    let val = JSON.stringify(el.value)
    socket.send([el.dataset.onInput, val].join(yetiInit.delimiter))
  }
})

document.addEventListener("keypress", function(e) {
  let el = e.target as HTMLInputElement
  if (e.code == "Enter" && el.dataset.onEnter) {
    console.log("ENTER!", el.dataset.onEnter)
    socket.send(el.dataset.onEnter)
  }
})
