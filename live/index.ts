import { hydrate, patch, render, DOMNode } from 'million';
import { fromDomNodeToVNode, fromStringToDomNode } from 'million/utils';
import { SocketAddress } from 'net';

declare var juniperState:string;

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
      console.log("Open, register: ", juniperState)

      // 1. send our initial state to register
      socketSend([JSON.stringify("Counter"), juniperState]);
  });

  // Listen for messages
  socket.addEventListener('message', (event) => {
      let [newState, html] = event.data.split("\n")
      console.log(newState)

      let dom = fromStringToDomNode(html)
      let vnode = fromDomNodeToVNode(dom)

      // This works, but it REALLY doesn't like the unclosed input tags from lucid
      rootElement = patch(rootElement, vnode)
      juniperState = newState
  });

  socket.addEventListener('close', (e) => {
    console.log("Closed")
    setTimeout(() => open(), 1000)
  });

  socket.addEventListener('error', (e) => {
    console.log("Error", e)
  });
}

function socketSend(lines:string[]) {
  socket.send(lines.join("\n"))
}

window.addEventListener("load", function() {
  console.log("State:", juniperState)
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
