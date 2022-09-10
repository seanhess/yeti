console.log("Loaded Example.js")


// Ok, let's give the component the ability to delete items
Juniper.registerComponent("comp", function(items) {
  // console.log("Component Updated", items)
  // console.log(this)

  let comp = this
  // no way to vdom this
  this.replaceChildren()

  for (item of items) {
    let link = document.createElement("div")
    link.setAttribute("class", "link")
    link.innerHTML = item
    link.addEventListener("click", function() {
      let onInput = new Event("delete", {bubbles: true})
      this.value = this.innerHTML
      this.dispatchEvent(onInput)
    })
    // console.log(link)
    this.appendChild(link)
  }
})
