function test() {
  console.log("TEST")
}

function sendMessage(body) {
  return fetch(location.href, {
    method: "POST",
    headers: {"Accept": "application/vdom"},
    body: body
  })
}


function sendLoad(url) {
  console.log("Send load", url)
  return fetch(url, {
    method: "GET",
    headers: {"Accept": "application/vdom"}
  })
}

function messageBody(action) {
  return action
}

function runtime(action) {
  let body = messageBody(action)
  sendMessage(body)
    .then(onResponse)
    .then(onResponseBody)
}

function link(url) {
  sendLoad(url)
    .then(onResponse)
    .then(onResponseBody)
}

function onResponse(res) {
  let h = res.headers.get('X-Page-Url')
  window.history.pushState("State", "Title", h)
  return res.text()
}

function onResponseBody(body) {
  console.log("BODY", body)
  document.getElementById("content").innerHTML = body
  // window.location.hash = data.resUrl
}

function interceptClickEvent(e) {

    // Check for intercepted links
    var href;
    var target = e.target || e.srcElement;
    if (target.tagName === 'A') {
        href = target.getAttribute('href');

        // TODO this shouldn't be hard coded. how do we know it's an app request? Can we assume any local request goes to the app server? Any link click? Probably. What about a static page?
        // HMM... we probably need to send down some constants here
        if (href.startsWith("/app")) {
          console.log("INTERCEPT LINK", href)
          link(href)
          e.preventDefault();
          return
        }
    }

  var click = e.target.dataset.click;
  if (click) {
    runtime(click)
  }
}



//listen for link click events at the document level
if (document.addEventListener) {
    document.addEventListener('click', interceptClickEvent);
} else if (document.attachEvent) {
    document.attachEvent('onclick', interceptClickEvent);
}
