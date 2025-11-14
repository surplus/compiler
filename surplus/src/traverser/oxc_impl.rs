use oxc::{
	allocator::Vec,
	ast::ast::{
		ArrayExpression, Expression, JSXAttribute, JSXAttributeValue, JSXChild, JSXElement,
		JSXExpression, Program,
	},
};
use oxc_traverse::{Traverse, TraverseCtx};

use crate::element::SurplusElement;

impl<'a> Traverse<'a> for super::SurplusTraverser<'a> {
	fn enter_jsx_element(&mut self, node: &mut JSXElement<'a>, ctx: &mut TraverseCtx<'a>) {
		self.inner_enter_jsx_element(node, ctx);
	}

	fn exit_jsx_element(&mut self, node: &mut JSXElement<'a>, ctx: &mut TraverseCtx<'a>) {
		self.inner_exit_jsx_element(node, ctx);
	}

	fn enter_jsx_attribute(&mut self, node: &mut JSXAttribute<'a>, ctx: &mut TraverseCtx<'a>) {
		if let Some(JSXAttributeValue::Element(_) | JSXAttributeValue::Fragment(_)) = &node.value {
			self.element_stack
				.push(SurplusElement::new_in(ctx, self.allocator));
		}
	}

	fn exit_jsx_attribute(&mut self, node: &mut JSXAttribute<'a>, ctx: &mut TraverseCtx<'a>) {
		self.inner_exit_jsx_attribute(node, ctx);
	}

	fn exit_jsx_spread_attribute(
		&mut self,
		node: &mut oxc::ast::ast::JSXSpreadAttribute<'a>,
		ctx: &mut TraverseCtx<'a>,
	) {
		self.inner_exit_jsx_spread_attribute(node, ctx);
	}

	fn enter_jsx_child(&mut self, node: &mut JSXChild<'a>, ctx: &mut TraverseCtx<'a>) {
		if matches!(node, JSXChild::Element(_) | JSXChild::Fragment(_)) {
			self.element_stack
				.push(SurplusElement::new_in(ctx, self.allocator));
		}
	}

	fn exit_jsx_child(&mut self, node: &mut JSXChild<'a>, ctx: &mut TraverseCtx<'a>) {
		self.inner_exit_jsx_child(node, ctx);
	}

	fn enter_jsx_expression(&mut self, node: &mut JSXExpression<'a>, ctx: &mut TraverseCtx<'a>) {
		match node {
			JSXExpression::JSXElement(_) | JSXExpression::JSXFragment(_) => {
				self.element_stack
					.push(SurplusElement::new_in(ctx, self.allocator));
			}
			_ => {}
		}
	}

	fn exit_jsx_expression(&mut self, node: &mut JSXExpression<'a>, ctx: &mut TraverseCtx<'a>) {
		match node {
			JSXExpression::JSXElement(elem) => {
				let expr = self.element_stack.pop().unwrap().into_surplus(
					elem.span,
					ctx,
					self.s_ref,
					self.allocator,
				);
				*node = expr.into();
			}
			JSXExpression::JSXFragment(_elem) => {
				todo!("fix fragment expression returns");
				// let expr = self.element_stack.pop().unwrap();
				// let expr = expr.into_surplus(elem.span, ctx, self.s_ref, self.allocator);
				//*node = expr.into();
			}
			_ => {}
		}
	}

	fn enter_expression(&mut self, node: &mut Expression<'a>, ctx: &mut TraverseCtx<'a>) {
		match node {
			Expression::JSXElement(_) | Expression::JSXFragment(_) => {
				self.element_stack
					.push(SurplusElement::new_in(ctx, self.allocator));
			}
			_ => {}
		}
	}

	fn exit_expression(&mut self, node: &mut Expression<'a>, ctx: &mut TraverseCtx<'a>) {
		match node {
			Expression::JSXElement(elem) => {
				let last_elem = self
					.element_stack
					.pop()
					.expect("surplus element stack underflow");
				*node = last_elem.into_surplus(elem.span, ctx, self.s_ref, self.allocator);
				self.performed_transformation = true;
			}
			Expression::JSXFragment(elem) => {
				let last_elem = self
					.element_stack
					.pop()
					.expect("surplus element stack underflow");
				*node = Expression::ArrayExpression(
					ctx.alloc(ArrayExpression {
						span: elem.span,
						elements: Vec::from_iter_in(
							last_elem
								.child_exprs
								.into_iter()
								.map(|child| child.unwrap().into()),
							self.allocator,
						),
						trailing_comma: None,
					}),
				);
				self.performed_transformation = true;
			}
			_ => {}
		}
	}

	fn exit_program(&mut self, _node: &mut Program<'a>, _ctx: &mut TraverseCtx<'a>) {
		assert!(self.element_stack.is_empty());
	}
}
