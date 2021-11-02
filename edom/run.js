
  var startHtml = document.getElementById('wookie-root-content').innerHTML
  var app = Elm.Main.init({
    node: document.getElementById('wookie-root-content'),
    flags: [document.title, startHtml]
  });