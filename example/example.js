console.log("Loaded Example.js")


// Ok, let's give the component the ability to delete items
Juniper.registerComponent("comp", function(items) {
  console.log("Component Updated", items)
  console.log(this)

  let comp = this
  // no way to vdom this
  this.replaceChildren()

  for (item of items) {
    let link = document.createElement("div")
    link.setAttribute("class", "link")
    link.innerHTML = item
    link.addEventListener("click", function() {
      console.log("Clicked", this.innerText)
      let onInput = new Event("input")
      onInput.value = this.innerText
      comp.dispatchEvent(onInput)
    })
    console.log(link)
    this.appendChild(link)

    comp.addEventListener("input", function() {
      console.log("GOT INPUT")
    })
  }

})
