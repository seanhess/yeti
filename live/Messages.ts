import {fromAction, Page, State} from "./types"
import { Action } from "./types";
import { VDOM } from "./VDOM"

export const WEBSOCKET_ADDRESS = "ws://" + location.host


// does this keep track of the state for you?
// hmm, I'd rather it doesn't


export class Messages {

  socket:WebSocket

  _update:Update;
  _close:() => void;

  queue:string[]
  isOpen:boolean
  page:Page

  constructor() {
    this.isOpen = false
    this.queue = []
  }

  onUpdate(up:Update) {
    this._update = up
  }

  onClose(close:() => void) {
    this._close = close
  }

  connect(page:Page, state:State) {
    this.socket = new WebSocket(WEBSOCKET_ADDRESS)

    this.socket.addEventListener('open', (event) => {
      console.log("Opened")
      this.isOpen = true

      this.register(page, state)

      // send any messages that were queued
      this.sendQueue()
    })

    // We need a better protocol!
    // Different message types might occur
    this.socket.addEventListener('message', (event) => {
      let [newState, params, vdoms]:string[] = event.data.split("\n")

      let vdom = JSON.parse(vdoms)

      if (this._update)
        this._update({fromState: newState}, params, vdom)
    })

    this.socket.addEventListener('error', (event) => {
      console.log("Socket Error", event)
    })

    this.socket.addEventListener('close', (event) => {
      console.log("Closed", event)
      if (this._close) {
        this._close()
      }
    })
  }

  register(page:Page, state:State) {
    this.send([page.fromPage, JSON.stringify(state.fromState)])
  }

  // don't actually send. Keep track of callbacks!
  // we want to wait to send the next one until this one goes through?
  // no, we can send them all immediately
  send(lines:string[]) {
    let msg = lines.join("\n")
    if (this.isOpen) {
      console.log("Send", lines)
      this.socket.send(msg)
    }
    else {
      console.log("Queue", msg)
      this.queue.push(msg)
    }
  }

  sendAction(action:string) {
    this.send([action])
  }

  sendActionVal(action:Action, val:string) {
    action.args.push(val)
    this.send([fromAction(action)])
  }

  sendQueue() {
    this.queue.forEach((m) => {
      console.log("SendQueue", m)
      this.socket.send(m)
    })

    this.queue = []
  }
}

type Update = (state:State, params:string, vdom:VDOM) => void;
