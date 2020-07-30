function test() {
  console.log("TEST")
}

function sendMessage(body) {
  // wait, I need to send my url somehow, also
  // I need to push to my location?
  // console.log(window.location.href)
  return fetch(location.href, { method: "POST", body: body })
}

function messageBody(action, url) {
  // return JSON.stringify({action: action, url: url})
  return action
}

function runtime(action) {
  let url = location.hash.substr(1)
      body = messageBody(action, url)
  console.log("RUNTIME:", action, url, body)
  sendMessage(body)
    // .then(res => res.json())
    .then(res => res.text())
    .then(onResponse)
    // let the server tell us what it is
    // TODO we also need to update the URL
}

function onResponse(data) {
  console.log("RES", data)
  document.body.innerHTML = data
  // window.location.hash = data.resUrl
}
