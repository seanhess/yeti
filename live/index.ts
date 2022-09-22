import { hydrate, patch, render, DOMNode } from 'million';
import { fromDomNodeToVNode, fromStringToDomNode } from 'million/utils';

declare const JUNIPER_STATE:string;

console.log("VERSION 1")


const PORT = 9160
const HOST = location.hostname
const PATH = location.pathname
console.log("Connecting: ", HOST, PORT)

var socket:WebSocket = new WebSocket('ws://' + HOST + ':' + PORT)

var rootElement:DOMNode

// Connection opened
socket.addEventListener('open', (event) => {
    console.log("Open")

    // 1. send our initial state to register
    socket.send(JUNIPER_STATE);
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
  console.log("State:", JUNIPER_STATE)
  rootElement = document.getElementById("juniper-root-content")

  let firstChild = rootElement.firstChild as DOMNode
  let initContent = fromDomNodeToVNode(firstChild)
  rootElement = patch(rootElement, initContent)
})


// Handle Click Events via bubbling, Easy!
document.addEventListener("click", function(e) {
  let el = e.target as HTMLElement
  if (el.dataset.onClick) {
    socket.send(el.dataset.onClick)
  }
})

document.addEventListener("input", function(e) {
  let el = e.target as HTMLInputElement
  if (el.dataset.onInput) {
    let val = JSON.stringify(el.value)
    socket.send(el.dataset.onInput + "\t" + val)
  }
})
