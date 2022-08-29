console.log("Loaded Example.js")



Juniper.registerComponent(".comp", function(items) {
  console.log("Component Updated", items)
  this.innerHTML = items.map(function(item) {
    return "<div>" + item + "</div>"
  }).join("\n")

})
