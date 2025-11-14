#![expect(missing_docs)]
use oxc::span::Atom;

pub const S_IDENT: &str = "__$S__";
pub const S_ELEM_IDENT: &str = "__$S_ELEM__";
pub const PREV_ATTRS_IDENT: &str = "__$prev_attrs__";

pub const CREATE_ELEMENT: &str = "createElement";
pub const CREATE_ELEMENT_NS: &str = "createElementNS";
pub const CREATE_TEXT_NODE: &str = "createTextNode";

pub const CHILDREN: &str = "children";
pub const COMPILE: &str = "compile";
pub const REPLACE_CHILDREN: &str = "replaceChildren";

pub const FN_ATTR: &str = "fn";
pub const REF_ATTR: &str = "ref";

pub const SET_ATTRIBUTE: &str = "setAttribute";
pub const SET_ATTRIBUTE_NS: &str = "setAttributeNS";
pub const REMOVE_ATTRIBUTE: &str = "removeAttribute";
pub const REMOVE_ATTRIBUTE_NS: &str = "removeAttributeNS";

pub const DOCUMENT: &str = "document";
pub const UNDEFINED: &str = "undefined";

pub const V: &str = "v";
pub const VAL: &str = "val";
pub const K: &str = "k";
pub const NEW_PREV: &str = "new_prev";

pub const S: Atom<'static> = Atom::new_const(S_IDENT);
