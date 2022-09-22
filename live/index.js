import { hydrate, patch, render } from 'million';
import { fromDomNodeToVNode, fromStringToDomNode } from 'million/utils';

console.log("VERSION 1")

const PORT = 9160
const HOST = location.hostname
const PATH = location.pathname
console.log("Connecting: ", HOST, PORT)

const socket = new WebSocket('ws://' + HOST + ':' + PORT)

var rootElement

// Connection opened
socket.addEventListener('open', (event) => {
    console.log("Open")

    // 1. send our initial state to register
    socket.send(juniperState);
});

// Listen for messages
socket.addEventListener('message', (event) => {
    let dom = fromStringToDomNode(event.data)
    let vnode = fromDomNodeToVNode(dom)

    // This works, but it REALLY doesn't like the unclosed input tags from lucid
    rootElement = patch(rootElement, vnode)
});

socket.addEventListener('close', (e) => {
  console.log("Closed")
});

socket.addEventListener('error', (e) => {
  console.log("Error", e)
});

window.addEventListener("load", function() {
  console.log("State:", juniperState)
  rootElement = document.getElementById("juniper-root-content")

  // 0. Hydrate content so we don't lose focus, etc
  let initContent = fromDomNodeToVNode(rootElement.children[0])
  rootElement = patch(rootElement, initContent)
})


// Handle Click Events, Easy!
document.addEventListener("click", function(e) {
  let click = e.target.dataset.onClick
  if (click) {
    socket.send(click)
  }
})
