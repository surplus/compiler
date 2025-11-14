use std::cell::Cell;

use oxc::{
	allocator::{Box, CloneIn, Vec},
	ast::ast::{
		Argument, ArrayExpression, CallExpression, Expression, IdentifierName, IdentifierReference,
		JSXChild, JSXText, StaticMemberExpression, StringLiteral,
	},
	span::Atom,
};
use oxc_traverse::TraverseCtx;

use crate::{
	constants::{CREATE_TEXT_NODE, DOCUMENT},
	decode_html_entities,
	element::SurplusChildType,
};

impl<'a> super::SurplusTraverser<'a> {
	pub fn inner_exit_jsx_child(&mut self, node: &mut JSXChild<'a>, ctx: &mut TraverseCtx<'a>) {
		match node {
			JSXChild::Element(e) => {
				let child = self
					.element_stack
					.pop()
					.expect("surplus element stack underflow");

				// Removing leading/trailing whitespace from child elements
				self.element_stack
					.last_mut()
					.unwrap()
					.child_exprs
					.push(SurplusChildType::Normal(child.into_surplus_comp(
						e.span,
						ctx,
						self.s_ref,
						self.allocator,
					)));
			}
			JSXChild::Fragment(e) => {
				let child = self
					.element_stack
					.pop()
					.expect("surplus element stack underflow");
				self.element_stack
					.last_mut()
					.unwrap()
					.child_exprs
					.push(SurplusChildType::Normal(Expression::ArrayExpression(
						ctx.alloc(ArrayExpression {
							span: e.span,
							elements: Vec::from_iter_in(
								child.child_exprs.into_iter().map(|e| e.unwrap().into()),
								self.allocator,
							),
							trailing_comma: None,
						}),
					)));
			}
			JSXChild::Text(text) => {
				// Skip any empty text nodes.
				if text.value.is_empty() {
					return;
				}

				// Handle case of multiple spaces in text nodes.
				let is_whitespace = if text.value.chars().all(char::is_whitespace) {
					// If we've already serviced a whitespace text node,
					// we don't need to add another one.
					if self
						.element_stack
						.last_mut()
						.unwrap()
						.child_exprs
						.last()
						.is_none_or(SurplusChildType::is_whitespace)
					{
						return;
					}

					*text = Box::new_in(
						JSXText {
							span: text.span,
							value: Atom::new_const(" "),
							raw: None,
						},
						self.allocator,
					);
					true
				} else {
					false
				};

				let expr = Expression::CallExpression(ctx.alloc(CallExpression {
					span: text.span,
					callee: Expression::StaticMemberExpression(ctx.alloc(StaticMemberExpression {
						span: text.span,
						object: Expression::Identifier(ctx.alloc(IdentifierReference {
							span: text.span,
							name: Atom::new_const(DOCUMENT),
							reference_id: Cell::new(None),
						})),
						property: IdentifierName {
							span: text.span,
							name: Atom::new_const(CREATE_TEXT_NODE),
						},
						optional: false,
					})),
					type_arguments: None,
					arguments: Vec::from_array_in(
						[Argument::StringLiteral(ctx.alloc(StringLiteral {
							span: text.span,
							value: decode_html_entities(self.allocator, text.value),
							raw: None,
							lossy: false,
						}))],
						self.allocator,
					),
					optional: false,
					pure: false,
				}));

				self.element_stack.last_mut().unwrap().child_exprs.push(
					if is_whitespace {
						SurplusChildType::Whitespace(expr)
					} else {
						SurplusChildType::Normal(expr)
					},
				);
			}
			JSXChild::ExpressionContainer(expr) => {
				let expr_sexpr = self.s_expression(
					ctx,
					expr.span,
					expr.expression.clone_in(self.allocator).into_expression(),
				);

				self.element_stack
					.last_mut()
					.unwrap()
					.child_exprs
					.push(SurplusChildType::Normal(expr_sexpr));
			}
			JSXChild::Spread(_expr) => {
				todo!("JSXChild::Spread");
			}
		}
	}
}
