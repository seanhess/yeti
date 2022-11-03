
import { VNode, Flags, m } from "million"




// VDOM //////////////

type TagName = string;
type AttName = string;
type AttValue = string;
type TextNode = string;

export type VDOM = Content[];
type Tag = [TagName, Record<AttName, AttValue>, Content[]]
type Content = Tag | TextNode;



export function fromVDOM(vdom:VDOM):VNode[] {
  return vdom.map(fromContent)

  function fromContent(content:Content):any {
    if (isContentText(content)) {
      return content
    }
    else {
      return fromTag(content as Tag)
    }
  }

  function fromTag([name, atts, children]:Tag):VNode {
    return m(
      name,
      atts,
      children.map(fromContent),
      childFlags(children)
    )
  }

  function childFlags(children:Content[]):Flags {
    if (children.length === 0) {
      return Flags.ELEMENT_NO_CHILDREN
    }

    else if (children.reduce(allContentText, true)) {
      return Flags.ELEMENT_TEXT_CHILDREN
    }

    // Other: Flags.ELEMENT_KEYED_CHILDREN
  }

  function allContentText(last:Boolean, content:Content) {
    return last && isContentText(content)
  }

  function isContentText(content:Content) {
    return (typeof content === 'string' || content instanceof String)
  }
}

