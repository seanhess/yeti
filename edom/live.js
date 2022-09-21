
const PORT = 9160
const HOST = location.hostname
const PATH = location.pathname
console.log("Connecting: ", HOST, PORT)

const socket = new WebSocket('ws://' + HOST + ':' + PORT)

// Connection opened
socket.addEventListener('open', (event) => {
    // we are connecting to our path
    // we need to establish a protocol
    console.log("Open")
    socket.send(juniperState);
});

// Listen for messages
socket.addEventListener('message', (event) => {
    console.log('Message from server ', event.data);
    socket.send("Increment");
});

socket.addEventListener('close', (e) => {
  console.log("Closed")
});

socket.addEventListener('error', (e) => {
  console.log("Error", e)
});

window.addEventListener("load", function() {
  console.log("State:", juniperState)
})