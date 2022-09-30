export type Page = {
  tag: string
}

export type State = {
  fromState: string
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

declare var yetiInit: {
  state: string;
  page: { tag: string };
}

export type Init = {
  state: State
  page: Page
}

export const INIT_PAGE:Page = yetiInit.page
export const INIT_STATE:State = { fromState: yetiInit.state }