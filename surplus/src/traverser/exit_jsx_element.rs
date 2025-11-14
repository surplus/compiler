use std::cell::Cell;

use oxc::{
	allocator::{Box, CloneIn, FromIn, Vec},
	ast::ast::{
		Argument, ArrayExpression, AssignmentExpression, AssignmentOperator, AssignmentTarget,
		BindingIdentifier, BindingPattern, BindingPatternKind, CallExpression, Expression,
		ExpressionStatement, IdentifierName, IdentifierReference, JSXElement, JSXElementName,
		ObjectProperty, ObjectPropertyKind, ParenthesizedExpression, PropertyKey, PropertyKind,
		ReturnStatement, Statement, StaticMemberExpression, StringLiteral, VariableDeclaration,
		VariableDeclarationKind, VariableDeclarator,
	},
	diagnostics::OxcDiagnostic,
	semantic::ReferenceFlags,
	span::Atom,
};
use oxc_traverse::TraverseCtx;

use crate::{
	constants::{
		CHILDREN, COMPILE, CREATE_ELEMENT, CREATE_ELEMENT_NS, DOCUMENT, REPLACE_CHILDREN, S,
	},
	element::SurplusChildType,
};

impl<'a> super::SurplusTraverser<'a> {
	pub fn inner_exit_jsx_element(&mut self, node: &mut JSXElement<'a>, ctx: &mut TraverseCtx<'a>) {
		// Pop off any leading/trailing whitespace text nodes.
		{
			let child_exprs = &mut self.element_stack.last_mut().unwrap().child_exprs;
			while child_exprs
				.last()
				.is_some_and(SurplusChildType::is_whitespace)
			{
				child_exprs.pop();
			}
			while child_exprs
				.first()
				.is_some_and(SurplusChildType::is_whitespace)
			{
				child_exprs.remove(0);
			}
		}

		// Assemble the child list.
		let child_array = ArrayExpression {
			span: node.span,
			elements: Vec::from_iter_in(
				self.element_stack
					.last_mut()
					.unwrap()
					.child_exprs
					.drain(..)
					.map(|expr| expr.unwrap().into()),
				self.allocator,
			),
			trailing_comma: None,
		};

		// Is this a custom element?
		if self
			.element_stack
			.last()
			.unwrap()
			.construction_obj
			.is_some()
		{
			let target = match &node.opening_element.name {
				JSXElementName::Identifier(ident) => {
					Expression::Identifier(ctx.alloc(IdentifierReference {
						span: ident.span,
						name: ident.name,
						reference_id: Cell::new(None),
					}))
				}
				JSXElementName::IdentifierReference(ident) => {
					Expression::Identifier(ident.clone_in(self.allocator))
				}
				JSXElementName::NamespacedName(_) => unreachable!(),
				JSXElementName::ThisExpression(member) => {
					Expression::ThisExpression(member.clone_in(self.allocator))
				}
				JSXElementName::MemberExpression(member) => {
					self.member_to_expression(ctx, &member.object, &member.property)
				}
			};

			let elem = self.element_stack.last_mut().unwrap();
			let mut con_obj = elem.construction_obj.take().unwrap();

			if !child_array.elements.is_empty() {
				con_obj
					.properties
					.push(ObjectPropertyKind::ObjectProperty(ctx.alloc(
						ObjectProperty {
							span: node.span,
							kind: PropertyKind::Init,
							key: PropertyKey::Identifier(ctx.alloc(IdentifierReference {
								span: node.span,
								name: Atom::new_const(CHILDREN),
								reference_id: Cell::new(None),
							})),
							value: Expression::ArrayExpression(ctx.alloc(child_array)),
							method: false,
							shorthand: false,
							computed: false,
						},
					)));
			}

			// Call the relevant function.
			elem.statements
				.push(Statement::VariableDeclaration(ctx.alloc(
					VariableDeclaration {
						span: node.span,
						kind: VariableDeclarationKind::Const,
						declare: false,
						declarations: Vec::from_array_in(
							[VariableDeclarator {
								definite: false,
								span: node.span,
								kind: VariableDeclarationKind::Const,
								id: BindingPattern {
									kind: BindingPatternKind::BindingIdentifier(ctx.alloc(
										BindingIdentifier {
											span: node.span,
											name: elem.elem.as_ref().unwrap().name,
											symbol_id: Cell::new(Some(
												elem.elem.as_ref().unwrap().sym,
											)),
										},
									)),
									type_annotation: None,
									optional: false,
								},
								init: Some(Expression::CallExpression(ctx.alloc(CallExpression {
									span: node.span,
									callee: target,
									type_arguments: None,
									arguments: Vec::from_array_in(
										[Argument::ObjectExpression(Box::from_in(
											con_obj,
											self.allocator,
										))],
										self.allocator,
									),
									optional: false,
									pure: false,
								}))),
							}],
							self.allocator,
						),
					},
				)));
		} else {
			// Otherwise, create the element and stick it first in the statements list.
			let (ns, name, name_span) = match &node.opening_element.name {
				JSXElementName::Identifier(ident) => (None, ident.name, ident.span),
				JSXElementName::IdentifierReference(ident) => (None, ident.name, ident.span),
				JSXElementName::NamespacedName(nsident) => {
					(
						Some((nsident.namespace.name, nsident.namespace.span)),
						nsident.name.name,
						nsident.name.span,
					)
				}
				JSXElementName::ThisExpression(_) | JSXElementName::MemberExpression(_) => {
					unreachable!()
				}
			};

			assert!(name.chars().next().is_some_and(char::is_lowercase));

			let create_name = if ns.is_none() {
				CREATE_ELEMENT
			} else {
				CREATE_ELEMENT_NS
			};
			let mut args = Vec::with_capacity_in(2, self.allocator);
			if let Some((ns_val, span)) = ns {
				args.push(Argument::StringLiteral(ctx.alloc(StringLiteral {
					span,
					value: ns_val,
					raw: None,
					lossy: false,
				})));
			}
			args.push(Argument::StringLiteral(ctx.alloc(StringLiteral {
				span: name_span,
				value: name,
				raw: None,
				lossy: false,
			})));

			let children_expression = if child_array.elements.is_empty() {
				None
			} else {
				let compile_fragment = Expression::CallExpression(ctx.alloc(CallExpression {
					span: node.span,
					callee: Expression::StaticMemberExpression(ctx.alloc(StaticMemberExpression {
						span: node.span,
						object: Expression::Identifier(ctx.alloc(IdentifierReference {
							span: node.span,
							name: S,
							reference_id: Cell::new(Some(self.s_ref)),
						})),
						property: IdentifierName {
							span: node.span,
							name: Atom::new_const(COMPILE),
						},
						optional: false,
					})),
					type_arguments: None,
					optional: false,
					pure: false,
					arguments: Vec::from_array_in(
						[Argument::ArrayExpression(ctx.alloc(child_array))],
						self.allocator,
					),
				}));

				let replace_children = Expression::CallExpression(
					ctx.alloc(CallExpression {
						span: node.span,
						callee: Expression::StaticMemberExpression(
							ctx.alloc(StaticMemberExpression {
								span: node.span,
								object: self
									.element_stack
									.last()
									.unwrap()
									.elem
									.as_ref()
									.unwrap()
									.ident
									.clone_in(self.allocator),
								property: IdentifierName {
									span: node.span,
									name: Atom::new_const(REPLACE_CHILDREN),
								},
								optional: false,
							}),
						),
						type_arguments: None,
						arguments: Vec::from_array_in(
							[Argument::SpreadElement(ctx.alloc(
								oxc::ast::ast::SpreadElement {
									span: node.span,
									argument: compile_fragment,
								},
							))],
							self.allocator,
						),
						optional: false,
						pure: false,
					}),
				);

				let computation = self.s_expression(ctx, node.span, replace_children);

				Some(Statement::ExpressionStatement(ctx.alloc(
					ExpressionStatement {
						span: node.span,
						expression: computation,
					},
				)))
			};

			let elem = self.element_stack.last_mut().unwrap();

			elem.statements.insert(
				0,
				Statement::VariableDeclaration(ctx.alloc(VariableDeclaration {
					span: node.span,
					kind: VariableDeclarationKind::Const,
					declare: false,
					declarations: Vec::from_array_in(
						[VariableDeclarator {
							definite: false,
							span: node.span,
							kind: VariableDeclarationKind::Const,
							id: BindingPattern {
								kind: BindingPatternKind::BindingIdentifier(ctx.alloc(
									BindingIdentifier {
										span: node.span,
										name: elem.elem.as_ref().unwrap().name,
										symbol_id: Cell::new(Some(elem.elem.as_ref().unwrap().sym)),
									},
								)),
								type_annotation: None,
								optional: false,
							},
							init: Some(Expression::CallExpression(ctx.alloc(CallExpression {
								span: node.span,
								callee: Expression::StaticMemberExpression(ctx.alloc(
									StaticMemberExpression {
										span: node.span,
										object: Expression::Identifier(ctx.alloc(
											IdentifierReference {
												span: node.span,
												name: Atom::new_const(DOCUMENT),
												reference_id: Cell::new(None),
											},
										)),
										property: IdentifierName {
											span: node.span,
											name: Atom::new_const(create_name),
										},
										optional: false,
									},
								)),
								type_arguments: None,
								arguments: args,
								optional: false,
								pure: false,
							}))),
						}],
						self.allocator,
					),
				})),
			);

			if let Some(child_population_statement) = children_expression {
				elem.statements.push(child_population_statement);
			}
		}

		// Get the return value before we do a mutable borrow.
		let retval = self
			.element_stack
			.last()
			.unwrap()
			.elem
			.as_ref()
			.unwrap()
			.ident
			.clone_in(self.allocator);

		let elem = self.element_stack.last_mut().unwrap();

		// Now set the element reference.
		if let Some((ref_expr, ref_expr_span)) = elem.ref_var.take() {
			let target = match ref_expr.into_inner_expression() {
				Expression::StaticMemberExpression(expr) => {
					Some(AssignmentTarget::StaticMemberExpression(expr))
				}
				Expression::ComputedMemberExpression(expr) => {
					Some(AssignmentTarget::ComputedMemberExpression(expr))
				}
				Expression::PrivateFieldExpression(expr) => {
					Some(AssignmentTarget::PrivateFieldExpression(expr))
				}
				Expression::Identifier(expr) => {
					Some(AssignmentTarget::AssignmentTargetIdentifier(expr))
				}
				_ => {
					self.errors.push(
						OxcDiagnostic::error("Invalid `ref` attribute expression")
							.with_label(ref_expr_span)
							.with_help("expected an identifier or left-hand assignment target"),
					);
					None
				}
			};

			if let Some(target) = target {
				elem.statements
					.push(Statement::ExpressionStatement(ctx.alloc(
						ExpressionStatement {
							span: ref_expr_span,
							expression: Expression::AssignmentExpression(ctx.alloc(
								AssignmentExpression {
									span: ref_expr_span,
									operator: AssignmentOperator::Assign,
									left: target,
									right: retval.clone_in(self.allocator),
								},
							)),
						},
					)));
			}
		}

		// Now call any `fn`s.
		for fn_call in &mut elem.fn_expressions {
			let expr: Expression<'a> = fn_call
				.expression
				.clone_in(self.allocator)
				.into_expression();
			let id_ref = ctx.create_ident_reference(
				fn_call.span,
				elem.elem.as_ref().unwrap().name,
				Some(elem.elem.as_ref().unwrap().sym),
				ReferenceFlags::read_write(),
			);
			elem.statements
				.push(Statement::ExpressionStatement(ctx.alloc(
					ExpressionStatement {
						span: fn_call.span,
						expression: Expression::CallExpression(ctx.alloc(CallExpression {
							span: fn_call.span,
							callee: Expression::ParenthesizedExpression(ctx.alloc(
								ParenthesizedExpression {
									span: fn_call.span,
									expression: expr,
								},
							)),
							type_arguments: None,
							arguments: Vec::from_array_in(
								[Argument::Identifier(Box::from_in(id_ref, self.allocator))],
								self.allocator,
							),
							optional: false,
							pure: false,
						})),
					},
				)));
		}

		elem.statements
			.push(Statement::ReturnStatement(ctx.alloc(ReturnStatement {
				span: node.span,
				argument: Some(retval),
			})));
	}
}
