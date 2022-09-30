export type Page = {
  tag: string
}

export type State = {
  fromState: string
}

export type Delimiter = {
  fromDelimiter: string
}

declare var yetiInit: {
  state: string;
  page: { tag: string };
  delimiter: string;
}

export type Init = {
  state: State
  page: Page
  delimiter: Delimiter
}

export const INIT_PAGE:Page = yetiInit.page
export const INIT_STATE:State = { fromState: yetiInit.state }
export const DELIMITER:Delimiter = { fromDelimiter: yetiInit.delimiter }