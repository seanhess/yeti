var h = require('virtual-dom/h');
var diff = require('virtual-dom/diff');
var patch = require('virtual-dom/patch');
var createElement = require('virtual-dom/create-element');
var parser = require('vdom-parser')

console.log("Loaded")

// 1: Create a function that declares what the DOM should look like
function render(text)  {
    console.log("render")
    return h('div', { id: "root" }, [
        h('input', { type: "text", id: "field", name: "field" }, []),
        h('p', {}, [text])
    ]);
}

function renderString(n) {
    var out = '<div id="root">'
    out += '<p>here is a new message</p>'
    for (var i = 0; i < n; i++) {
        out += '<p>Num: '+i+'</p>'
    }
    out += '<input type="text" id="field" name="field" key="field"/>'
    return parser(out)
}

// 2: Initialise the document
// var count = 0;      // We need some app data. Here we just store a count.

// var tree = render("hello");               // We need an initial tree
// var rootNode = createElement(tree);     // Create an initial root DOM node ...
// document.body.appendChild(rootNode);    // ... and it should be in the document
// var rootNode = document.getElementById("root")
var rootNode = document.getElementById("root")
var tree = parser(rootNode)
// console.log("ROOT", rootNode)
console.log("TREE", tree)


var newTree = renderString(1)
console.log("NEW TREE", newTree)
var patches = diff(tree, newTree)
console.log("P1", patches)
// rootNode = patch(rootNode, patches)


// // 3: Wire up the update logic
// setInterval(function () {
//       count++;
//       var newTree = render(count);
//       var patches = diff(tree, newTree);
//       rootNode = patch(rootNode, patches);
//       tree = newTree;
// }, 1000);


var count = 0
function update() {
    count++
    var newTree = renderString(count)
    console.log("NEW TREE", newTree)
    var patches = diff(tree, newTree)
    console.log("PATCHEZ", patches)
    rootNode = patch(rootNode, patches)
    tree = newTree

    if (count < 4) {
        setTimeout(update, 1000)
    }
}

setTimeout(update, 1000)