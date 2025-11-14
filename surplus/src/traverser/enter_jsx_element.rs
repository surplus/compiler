use std::cell::Cell;

use oxc::{
	allocator::Vec,
	ast::ast::{Expression, IdentifierReference, JSXElement, JSXElementName, ObjectExpression},
	semantic::{NodeId, ReferenceFlags, SymbolFlags},
	span::Atom,
};
use oxc_traverse::TraverseCtx;

use crate::{constants::S_ELEM_IDENT, element::ElemIdent};

impl<'a> super::SurplusTraverser<'a> {
	pub(crate) fn inner_enter_jsx_element(
		&mut self,
		node: &mut JSXElement<'a>,
		ctx: &mut TraverseCtx<'a>,
	) {
		let elem_ident_str = S_ELEM_IDENT;

		assert!(!node.opening_element.self_closing || node.children.is_empty());

		let elem = self.element_stack.last_mut().unwrap();

		elem.self_closing = node.opening_element.self_closing;

		let ident_span = node
			.opening_element
			.name
			.get_identifier()
			.map_or_else(|| node.opening_element.span, |i| i.span);
		let ident_sym = ctx.scoping_mut().create_symbol(
			ident_span,
			elem_ident_str,
			SymbolFlags::ConstVariable,
			elem.scope,
			NodeId::DUMMY,
		);
		let ident_ref =
			ctx.create_reference(elem_ident_str, Some(ident_sym), ReferenceFlags::Write);

		let elem_ident = Expression::Identifier(ctx.alloc(IdentifierReference {
			name: Atom::new_const(elem_ident_str),
			span: ident_span,
			reference_id: Cell::new(Some(ident_ref)),
		}));

		// Is this a custom element?
		let custom_element = match &node.opening_element.name {
			JSXElementName::Identifier(name) => {
				name.name.chars().next().is_some_and(char::is_uppercase)
			}
			JSXElementName::IdentifierReference(name) => {
				name.name.chars().next().is_some_and(char::is_uppercase)
			}
			JSXElementName::MemberExpression(_)
			| JSXElementName::NamespacedName(_)
			| JSXElementName::ThisExpression(_) => true,
		};

		if custom_element {
			// The "element" identifier is actually now an object literal.
			let obj_expr = ObjectExpression {
				span: node.span,
				properties: Vec::with_capacity_in(
					node.opening_element.attributes.len(),
					self.allocator,
				),
				trailing_comma: None,
			};
			assert!(elem.construction_obj.replace(obj_expr).is_none());
		}

		let old_ident = elem.elem.replace(ElemIdent {
			ident: elem_ident,
			sym: ident_sym,
			name: Atom::new_const(elem_ident_str),
		});
		assert!(old_ident.is_none());
	}
}
