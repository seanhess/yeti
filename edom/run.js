const JUNIPER_UPDATE_DOM = "updateDOM"

window.addEventListener("load", function () {
  var root = document.getElementById('juniper-root-content')

  var app = Elm.Main.init({
    node: document.getElementById('juniper-root-content'),

    // juniperState should be embedded: let juniperState = {}
    flags: [document.title, root.innerHTML, juniperState]
  });



  // fire an event of that name
  app.ports.sendEvent.subscribe(function(name) {
    if (name == JUNIPER_UPDATE_DOM) {
      document.dispatchEvent(new Event(name))
    }
  });
})


const Juniper = {}

Juniper.registerComponent = function(name, f) {

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
