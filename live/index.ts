import { hydrate, patch, render, DOMNode, m, VNode, Flags } from 'million';
import { fromDomNodeToVNode, fromStringToDomNode } from 'million/utils';
import { listenEvents } from './events';
import { WEBSOCKET_ADDRESS, Messages } from './Messages'
import { INIT_PAGE, INIT_STATE, State, Classes } from './types';
import { fromVDOM, VDOM } from './vdom'


const CONTENT_ID = "yeti-root-content"

console.log("VERSION 2", INIT_PAGE, INIT_STATE)
console.log("stylesheets", document.styleSheets)

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


function update(newState:State, params:string, vdom:VDOM, classes:Classes) {

  // This is stripping tab characters in the data attributes, can't use tab as a delimiter
  // let dom = fromStringToDomNode(html)
  // let vnode = fromDomNodeToVNode(dom)
  // rootElement = patch(rootElement, vnode)
  let newRoot = m("div", {"id": CONTENT_ID}, fromVDOM(vdom))
  rootElement = patch(rootElement, newRoot)

  currentState = newState

  // Update stylesheet
  let defs = classDefinitions(classes).join("\n")
  console.log("DEFS", defs)
  styleSheet.replaceSync(defs)

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
  let [newState, params, html, stylesheet] = e.state
  update(newState, params, html, stylesheet)
})

window.addEventListener("load", function() {
  console.log("docload")
  rootElement = document.getElementById("yeti-root-content")

  // hydreate the content
  let initContent = fromDomNodeToVNode(rootElement)
  console.log('INIT', initContent)
  rootElement = patch(rootElement, initContent)
})




// DYNAMIC STYLES ////////////////////


const styleSheet = new CSSStyleSheet();

styleSheet.replaceSync('body { background: red; }');

// sheet.insertRule("* { color: blue; }");

// Apply the stylesheet to a document
document.adoptedStyleSheets = [styleSheet];


function classDefinitions(classes:Classes):string[] {
  var defs = []
  for (var name in classes ) {
    defs.push(classDefinition(name, classes[name]))
  }
  return defs
}

function classDefinition(name:string, properties:[prop:string]):string {
  var props = []
  for (var propName in properties) {
    props.push(propName + ":" + properties[propName])
  }
  return "." + name + " " + "{" + props.join("; ") + "}"
}
