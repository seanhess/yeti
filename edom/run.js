const JUNIPER_UPDATE_DOM = "updateDOM"

window.addEventListener("load", function () {
  var root = document.getElementById('yeti-root-content')

  var app = Elm.Main.init({
    node: document.getElementById('yeti-root-content'),

    // yetiState should be embedded: let yetiState = {}
    flags: [document.title, root.innerHTML, yetiState]
  });



  // fire an event of that name
  app.ports.sendEvent.subscribe(function(name) {
    if (name == JUNIPER_UPDATE_DOM) {
      document.dispatchEvent(new Event(name))
    }
  });
})


const Yeti = {}

Yeti.registerComponent = function(name, f) {

  let selector = "." + name
  document.addEventListener("updateDOM", function() {

    const doms = document.querySelectorAll(selector)
    for (dom of doms) {

      // compare data input as strings
      if (dom.dataset.input !== dom.oldInput) {
        let inp = JSON.parse(dom.dataset.input)
        f.call(dom, inp)
      }

      dom.oldInput = dom.dataset.input
    }
  })
}


// Javascript responsible for communicating with server
// Elm just takes info in over a port
// Updates

// // Create WebSocket connection.
// console.log("OPEN")
// const socket = new WebSocket('ws://127.0.0.1:9160');

// // Connection opened
// socket.addEventListener('open', (event) => {
//     socket.send('Hi! I am bob');
// });

// // Listen for messages
// socket.addEventListener('message', (event) => {
//     console.log('Message from server ', event.data);
// });

// Other Events
// "close" event
// "error"
