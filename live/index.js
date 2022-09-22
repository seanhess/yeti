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
    // we are connecting to our path
    // we need to establish a protocol
    console.log("Open")
    socket.send(juniperState);
});

// Listen for messages
socket.addEventListener('message', (event) => {
    console.log('Message from server ', event.data);

    let dom = fromStringToDomNode(event.data)
    let vnode = fromDomNodeToVNode(dom)

    // This works, but it REALLY doesn't like the unclosed input tags from lucid
    rootElement = patch(rootElement, vnode)
    console.log(rootElement)
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

  // Hydrate it! (hydrate doesn't work, ironically)
  let initContent = fromDomNodeToVNode(rootElement.children[0])
  rootElement = patch(rootElement, initContent)
})

document.addEventListener("click", function(e) {
  if (e.target.type == "submit") {
    socket.send("Increment")
  }
})
