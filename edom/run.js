
  var startHtml = document.getElementById('juniper-root-content').innerHTML
  var app = Elm.Main.init({
    node: document.getElementById('juniper-root-content'),
    flags: [document.title, startHtml]
  });