
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

declare var yetiPage: string;
declare var yetiState: Object;

export type Init = {
  state: State
  page: Page
}

export const INIT_PAGE:Page = { fromPage: yetiPage }
export const INIT_STATE:State = { fromState: yetiState }



