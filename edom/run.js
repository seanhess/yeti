
  var startHtml = document.getElementById('juniper-root-content').innerHTML
  var app = Elm.Main.init({
    node: document.getElementById('juniper-root-content'),

    // juniperState should be embedded: let juniperState = {}
    flags: [document.title, startHtml, juniperState]
  });