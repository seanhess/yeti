import { VDOM } from "./vdom"

export type Page = {
  fromPage: string
}

export type State = {
  fromState: Object
}

export type Delimiter = {
  fromDelimiter: string
}

export type Action = {
  constructor: string
  args: string[]
}

// Ok, we have: "Action", or "Action []"
export function toAction(src:string):Action {
  let grs = src.match(/(\w+) ?(.*)/)
  let con = grs[1]
  let arg = grs[2] || "[]"
  // console.log("MATCH", con, arg)
  return {
    constructor: con,
    args: JSON.parse(arg)
  }
}

export function fromAction(act:Action):string {
  return act.constructor + " " + JSON.stringify(act.args)
}

declare var YETI_PAGE: string;
declare var YETI_STATE: Object;

export type Init = {
  state: State
  page: Page
}

export const INIT_PAGE:Page = { fromPage: YETI_PAGE }
export const INIT_STATE:State = { fromState: YETI_STATE }



export type Update = (state:State, params:string, vdom:VDOM, classes:Class[]) => void;

export type Class = {selectorText:string, cssText:string}