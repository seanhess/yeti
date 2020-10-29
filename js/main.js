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
  return fetch(url, {
    method: "GET",
    headers: {"Accept": "application/vdom"}
  })
}

function messageBody(action) {
  return action
}

function runtime(action) {
  console.log(action)
  let body = messageBody(action)
  sendMessage(body)
    .then(onResponse)
    .then(onResponseBody)
}

// when you click a link
function link(url) {
  sendLoad(url)
    .then(onResponse)
    .then(onResponseBody)
}

function restore(url) {
  // same as above, but don't push state
  sendLoad(url)
    .then(res => res.text())
    .then(onResponseBody)
}

function onResponse(res) {
  let pageUrl = res.headers.get('X-Page-Url')
  // TODO titles
  let title = "Wookie Tab Title"
  console.log(pageUrl)
  window.history.pushState({pageUrl: pageUrl}, title, pageUrl)
  return res.text()
}

function onResponseBody(body) {
  console.log(body)
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
        }
    }

  
}

function onClick(e) {
  // check for our clicks
  var click = e.target.dataset.click;
  if (click) {
    runtime(click)
  }

  else {
    interceptClickEvent(e)
  }
}



function onSubmit(e) {
  console.log("ONSUBMIT", e.target.dataset)
  e.preventDefault();

  var form = e.target
  var submit = form.dataset.submit
  var submit1 = form.dataset.submit1
  var inputs = "input,textarea"

  if (submit) {

    // find any named inputs and textareas and add them
    var formData = {}
    for (var child of form.querySelectorAll(inputs)) {
      if (child.name) {
        formData[child.name] = child.value
      }
      else if (child.id) {
        formData[child.id] = child.value
      }
    }

    var action = submit + " " + encodeMap(formData)
    console.log("Submit", action)
    runtime(action)
  }
  else if (submit1) {

    // collect the value from the first input or textarea we find
    var child = form.querySelector(inputs)
    var value = child.value || ""
    var action = submit1 + " " + JSON.stringify(value)
    console.log("Submit1", action)
    runtime(action)
  }
}

function onInput(e) {
  console.log("INPUT", e.target.dataset, e.target.value)
  var target = e.target
  var onInput = target.dataset.oninput
  if (onInput) {
    var value = target.value || ""
    var action = onInput + " " + JSON.stringify(value)
    runtime(action)
  }
}

//listen for link click events at the document level
if (document.addEventListener) {
  document.addEventListener('click', onClick);
  document.addEventListener('submit', onSubmit);

  // do I want to do this? Or just to the ones that have it?
  document.addEventListener('input', onInput);
} else if (document.attachEvent) {
    document.attachEvent('onclick', onClick);
    document.attachEvent('onsubmit', onSubmit);
  document.attachEven('oninput', onInput);
}

window.addEventListener('popstate', (event) => {
  // console.log("location:", document.location, "state:", event.state)
  console.log("popstate", event.state, event.state.pageUrl)
  if (event.state.pageUrl) {
    restore(event.state.pageUrl)
  }
})


function encodeMap(obj) {
  var pairs = []
  for (var key in obj) {
    pairs.push([key, obj[key]])
  }

  return "(fromList [" + pairs.map(encodePair).join(",") + "])"
}

function encodePair(pair) {
  return "(" + JSON.stringify(pair[0]) + "," + JSON.stringify(pair[1]) + ")"
}
