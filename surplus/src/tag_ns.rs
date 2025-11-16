//! Helper trait that returns a namespace for a given tag name.
//!
//! This is a workaround to the utterly broken "XML namespace"
//! system in HTML that makes it nearly impossible to work with
//! namespaces via Javascript. Sorry if this breaks you somehow.
//! It's an extremely difficult problem to solve.

use oxc::span::Atom;

/// The URI for SVG elements
const SVG_NS: &str = "http://www.w3.org/2000/svg";
/// The URI for MathML elements
const MATHML_NS: &str = "http://www.w3.org/1998/Math/MathML";

/// Returns the namespace for a given tag name,
/// or `None` for the default HTML namespace.
pub fn tag_ns(tag_name: &str) -> Option<Atom<'static>> {
	match tag_name {
		"animate"
		| "animateMotion"
		| "animateTransform"
		| "circle"
		| "clipPath"
		| "defs"
		| "desc"
		| "ellipse"
		| "feBlend"
		| "feColorMatrix"
		| "feComponentTransfer"
		| "feComposite"
		| "feConvolveMatrix"
		| "feDiffuseLighting"
		| "feDisplacementMap"
		| "feDistantLight"
		| "feDropShadow"
		| "feFlood"
		| "feFuncA"
		| "feFuncB"
		| "feFuncG"
		| "feFuncR"
		| "feGaussianBlur"
		| "feImage"
		| "feMerge"
		| "feMergeNode"
		| "feMorphology"
		| "feOffset"
		| "fePointLight"
		| "feSpecularLighting"
		| "feSpotLight"
		| "feTile"
		| "feTurbulence"
		| "filter"
		| "foreignObject"
		| "g"
		| "image"
		| "line"
		| "linearGradient"
		| "marker"
		| "mask"
		| "metadata"
		| "mpath"
		| "path"
		| "pattern"
		| "polygon"
		| "polyline"
		| "radialGradient"
		| "rect"
		| "script"
		| "set"
		| "stop"
		| "style"
		| "svg"
		| "switch"
		| "symbol"
		| "text"
		| "textPath"
		| "title"
		| "tspan"
		| "use"
		| "view" => Some(Atom::new_const(SVG_NS)),
		"math" | "maction" | "annotation" | "annotation-xml" | "menclose" | "merror"
		| "mfenced" | "mfrac" | "mi" | "mmultiscripts" | "mn" | "mo" | "mover" | "mpadded"
		| "mphantom" | "mprescripts" | "mroot" | "mrow" | "ms" | "semantics" | "mspace"
		| "msqrt" | "mstyle" | "msub" | "msup" | "msubsup" | "mtable" | "mtd" | "mtext" | "mtr"
		| "munder" | "munderover" => Some(Atom::new_const(MATHML_NS)),
		// Default to HTML namespace
		_ => None,
	}
}
