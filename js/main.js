function test() {
  console.log("TEST")
}

function sendMessage(body) {
  // wait, I need to send my url somehow, also
  // I need to push to my location?
  // console.log(window.location.href)
  return fetch(location.href, {
    method: "POST",
    headers: {"Accept": "application/vdom"},
    body: body
  })
}

function messageBody(action, url) {
  // return JSON.stringify({action: action, url: url})
  return action
}

function runtime(action) {
  let url = location.hash.substr(1)
      body = messageBody(action, url)
  // console.log("RUNTIME:", action, url, body)
  sendMessage(body)
    // .then(res => res.json())
    .then(res => {
      let h = res.headers.get('X-Page-Url')
      window.history.pushState("State", "Title", h)
      return res.text()
    })
    .then(onResponse)
}

function onResponse(data) {
  console.log("RES", data)
  document.body.innerHTML = data
  // window.location.hash = data.resUrl
}
