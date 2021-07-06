import {
    init,
    classModule,
    propsModule,
    styleModule,
    eventListenersModule,
    h,
} from "snabbdom";

import html from 'snabby'

// TODO as a workaround, after load, call the server once more to rehydrate the html
// then you're up and running normally

// or do the same with elm, perhaps?
// yeah, because I have to update it anyway, and then I don't have to work in javascript

const patch = init([
// Init patch function with chosen modules
classModule, // makes it easy to toggle classes
propsModule, // for setting properties on DOM elements
styleModule, // handles styling on elements with support for animations
eventListenersModule, // attaches event listeners
]);

var root = document.getElementById("container");

var test = '<div id="container"><input type="text" id="field" name="field" key="field"/><p>Hello2</p></div>'
console.log("TEST", test)
console.log("TEMP", test.template())
let initialHtml = html([test])
console.log("TEST", initialHtml)

setTimeout(function() {
    html.update(root, test)
}, 1000)

// const vnode = h("div#container.two.classes", { on: { click: someFn } }, [
//     h("span", { style: { fontWeight: "bold" } }, "This is bold"),
//     " and this is just normal text",
//     h("a", { props: { href: "/foo" } }, "I'll take you places!"),
// ]);
// // Patch into empty DOM element – this modifies the DOM as a side effect
// patch(container, vnode);

// const newVnode = h(
//     "div#container.two.classes",
//     { on: { click: anotherEventHandler } },
//     [
//         h(
//         "span",
//         { style: { fontWeight: "normal", fontStyle: "italic" } },
//         "This is now italic type"
//         ),
//         " and this is still just normal text",
//         h("a", { props: { href: "/bar" } }, "I'll take you places!"),
//     ]
// );
// // Second `patch` invocation

// setTimeout(function() {
//     patch(vnode, newVnode); // Snabbdom efficiently updates the old view to the new state
// }, 1000)

function someFn() {

}

function anotherEventHandler() {

}