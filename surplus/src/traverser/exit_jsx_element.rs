use std::cell::Cell;

use oxc::{
	allocator::{Box, CloneIn, FromIn, Vec},
	ast::ast::{
		Argument, ArrayExpression, ArrowFunctionExpression, AssignmentExpression,
		AssignmentOperator, AssignmentTarget, BindingIdentifier, BindingPattern,
		BindingPatternKind, CallExpression, Expression, ExpressionStatement, FormalParameter,
		FormalParameterKind, FormalParameters, FunctionBody, IdentifierName, IdentifierReference,
		JSXElement, JSXElementName, ObjectProperty, ObjectPropertyKind, ParenthesizedExpression,
		PropertyKey, PropertyKind, ReturnStatement, Statement, StaticMemberExpression,
		StringLiteral, VariableDeclaration, VariableDeclarationKind, VariableDeclarator,
	},
	diagnostics::OxcDiagnostic,
	semantic::ReferenceFlags,
	span::{Atom, Span},
};
use oxc_traverse::TraverseCtx;

use crate::{
	constants::{
		ADD_EVENT_LISTENER, CHILDREN, CLEANUP, COMPILE, CREATE_ELEMENT, CREATE_ELEMENT_NS,
		DOCUMENT, REMOVE_EVENT_LISTENER, REPLACE_CHILDREN, S,
	},
	element::SurplusChildType,
	tag_ns::tag_ns,
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
				JSXElementName::NamespacedName(nsident) => {
					self.errors.push(
						OxcDiagnostic::error("Namespaced JSX elements are not supported")
							.with_label(nsident.span)
							.with_help("consider using a custom element or removing the namespace"),
					);
					Expression::Identifier(Box::from_in(
						IdentifierReference {
							span: nsident.name.span,
							name: nsident.name.name,
							reference_id: Cell::new(None),
						},
						self.allocator,
					))
				}
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
				JSXElementName::Identifier(ident) => {
					(tag_ns(ident.name.as_str()), ident.name, ident.span)
				}
				JSXElementName::IdentifierReference(ident) => {
					(tag_ns(ident.name.as_str()), ident.name, ident.span)
				}
				JSXElementName::NamespacedName(nsident) => {
					self.errors.push(
						OxcDiagnostic::error("Namespaced JSX elements are not supported")
							.with_label(nsident.span)
							.with_help("consider using a custom element or removing the namespace"),
					);
					(None, nsident.name.name, nsident.name.span)
				}
				JSXElementName::ThisExpression(_) | JSXElementName::MemberExpression(_) => {
					unreachable!()
				}
			};

			let ns = ns.map(|ns| (ns, Span::default()));

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

		// Get the return value and event handler data before we do a mutable borrow.
		let retval = self
			.element_stack
			.last()
			.unwrap()
			.elem
			.as_ref()
			.unwrap()
			.ident
			.clone_in(self.allocator);

		let event_handlers: std::vec::Vec<_> = self
			.element_stack
			.last()
			.unwrap()
			.event_handlers
			.iter()
			.map(|(name, expr, span)| (*name, expr.clone_in(self.allocator), *span))
			.collect();

		let target_ident = self
			.element_stack
			.last()
			.unwrap()
			.elem
			.as_ref()
			.unwrap()
			.ident
			.clone_in(self.allocator);
		// Build event handler statements before mutable borrow
		let event_stmts =
			if event_handlers.is_empty() {
				std::vec::Vec::new()
			} else {
				let mut stmts = std::vec::Vec::new();

				for (event_name, handler_expr, event_span) in event_handlers {
					let target_ref = target_ident.clone_in(self.allocator);

					// Build: S(((e, n, h) => { e.addEventListener(n, h); S.cleanup(() => e.removeEventListener(n, h)); })(element, "eventname", handler))

					// e.addEventListener(n, h)
					let add_listener =
						Statement::ExpressionStatement(ctx.alloc(ExpressionStatement {
							span: event_span,
							expression: Expression::CallExpression(ctx.alloc(CallExpression {
								span: event_span,
								callee: Expression::StaticMemberExpression(ctx.alloc(
									StaticMemberExpression {
										span: event_span,
										object: Expression::Identifier(ctx.alloc(
											IdentifierReference {
												span: event_span,
												name: Atom::new_const("e"),
												reference_id: Cell::new(None),
											},
										)),
										property: IdentifierName {
											span: event_span,
											name: Atom::new_const(ADD_EVENT_LISTENER),
										},
										optional: false,
									},
								)),
								type_arguments: None,
								arguments: Vec::from_array_in(
									[
										Argument::Identifier(Box::from_in(
											IdentifierReference {
												span: event_span,
												name: Atom::new_const("n"),
												reference_id: Cell::new(None),
											},
											self.allocator,
										)),
										Argument::Identifier(Box::from_in(
											IdentifierReference {
												span: event_span,
												name: Atom::new_const("h"),
												reference_id: Cell::new(None),
											},
											self.allocator,
										)),
									],
									self.allocator,
								),
								optional: false,
								pure: false,
							})),
						}));

					// S.cleanup(() => e.removeEventListener(n, h))
					let cleanup_stmt = Statement::ExpressionStatement(ctx.alloc(ExpressionStatement {
   					span: event_span,
   					expression: Expression::CallExpression(ctx.alloc(CallExpression {
   						span: event_span,
   						callee: Expression::StaticMemberExpression(ctx.alloc(StaticMemberExpression {
   							span: event_span,
   							object: Expression::Identifier(ctx.alloc(IdentifierReference {
   								span: event_span,
   								name: S,
   								reference_id: Cell::new(Some(self.s_ref)),
   							})),
   							property: IdentifierName {
   								span: event_span,
   								name: Atom::new_const(CLEANUP),
   							},
   							optional: false,
   						})),
   						type_arguments: None,
   						arguments: Vec::from_array_in(
   							[Argument::ArrowFunctionExpression(ctx.alloc(ArrowFunctionExpression {
   								span: event_span,
   								expression: true,
   								r#async: false,
   								type_parameters: None,
   								params: ctx.alloc(FormalParameters {
   									span: event_span,
   									kind: FormalParameterKind::ArrowFormalParameters,
   									items: Vec::new_in(self.allocator),
   									rest: None,
   								}),
   								return_type: None,
   								body: ctx.alloc(FunctionBody {
   									span: event_span,
   									directives: Vec::new_in(self.allocator),
   									statements: Vec::from_array_in(
   										[Statement::ExpressionStatement(ctx.alloc(ExpressionStatement {
   											span: event_span,
   											expression: Expression::CallExpression(ctx.alloc(CallExpression {
   												span: event_span,
   												callee: Expression::StaticMemberExpression(ctx.alloc(StaticMemberExpression {
   													span: event_span,
   													object: Expression::Identifier(ctx.alloc(IdentifierReference {
   														span: event_span,
   														name: Atom::new_const("e"),
   														reference_id: Cell::new(None),
   													})),
   													property: IdentifierName {
   														span: event_span,
   														name: Atom::new_const(REMOVE_EVENT_LISTENER),
   													},
   													optional: false,
   												})),
   												type_arguments: None,
   												arguments: Vec::from_array_in(
   													[
   														Argument::Identifier(Box::from_in(IdentifierReference {
   															span: event_span,
   															name: Atom::new_const("n"),
   															reference_id: Cell::new(None),
   														}, self.allocator)),
   														Argument::Identifier(Box::from_in(IdentifierReference {
   															span: event_span,
   															name: Atom::new_const("h"),
   															reference_id: Cell::new(None),
   														}, self.allocator)),
   													],
   													self.allocator,
   												),
   												optional: false,
   												pure: false,
   											})),
   										}))],
   										self.allocator,
   									),
   								}),
   								scope_id: Cell::new(None),
   								pure: false,
   							}))],
   							self.allocator,
   						),
   						optional: false,
   						pure: false,
   					})),
   				}));

					// ((e, n, h) => () => { ... }) - the arrow function that takes parameters and returns arrow
					let iife_function = Expression::ArrowFunctionExpression(ctx.alloc(ArrowFunctionExpression {
   					span: event_span,
   					expression: true,
   					r#async: false,
   					type_parameters: None,
   					params: ctx.alloc(FormalParameters {
   						span: event_span,
   						kind: FormalParameterKind::ArrowFormalParameters,
   						items: Vec::from_array_in(
   							[
   								FormalParameter {
   									span: event_span,
   									decorators: Vec::new_in(self.allocator),
   									pattern: BindingPattern {
   										kind: BindingPatternKind::BindingIdentifier(ctx.alloc(BindingIdentifier {
   											span: event_span,
   											name: Atom::new_const("e"),
   											symbol_id: Cell::new(None),
   										})),
   										type_annotation: None,
   										optional: false,
   									},
   									accessibility: None,
   									readonly: false,
   									r#override: false,
   								},
   								FormalParameter {
   									span: event_span,
   									decorators: Vec::new_in(self.allocator),
   									pattern: BindingPattern {
   										kind: BindingPatternKind::BindingIdentifier(ctx.alloc(BindingIdentifier {
   											span: event_span,
   											name: Atom::new_const("n"),
   											symbol_id: Cell::new(None),
   										})),
   										type_annotation: None,
   										optional: false,
   									},
   									accessibility: None,
   									readonly: false,
   									r#override: false,
   								},
   								FormalParameter {
   									span: event_span,
   									decorators: Vec::new_in(self.allocator),
   									pattern: BindingPattern {
   										kind: BindingPatternKind::BindingIdentifier(ctx.alloc(
   											BindingIdentifier {
   											span: event_span,
   											name: Atom::new_const("h"),
   											symbol_id: Cell::new(None),
   										})),
   										type_annotation: None,
   										optional: false,
   									},
   									accessibility: None,
   									readonly: false,
   									r#override: false,
   								},
   							],
   							self.allocator,
   						),
   						rest: None,
   					}),
   					return_type: None,
   					body: ctx.alloc(FunctionBody {
   						span: event_span,
   						directives: Vec::new_in(self.allocator),
   						statements: Vec::from_array_in(
   							[Statement::ExpressionStatement(ctx.alloc(ExpressionStatement {
   								span: event_span,
   								expression: Expression::ArrowFunctionExpression(ctx.alloc(
   									ArrowFunctionExpression {
   									span: event_span,
   									expression: false,
   									r#async: false,
   									type_parameters: None,
   									params: ctx.alloc(FormalParameters {
   										span: event_span,
   										kind: FormalParameterKind::ArrowFormalParameters,
   										items: Vec::new_in(self.allocator),
   										rest: None,
   									}),
   									return_type: None,
   									body: ctx.alloc(FunctionBody {
   										span: event_span,
   										directives: Vec::new_in(self.allocator),
   										statements: Vec::from_array_in([add_listener,
   											cleanup_stmt], self.allocator),
   									}),
   									scope_id: Cell::new(None),
   									pure: false,
   								})),
   							}))],
   							self.allocator,
   						),
   					}),
   					scope_id: Cell::new(None),
   					pure: false,
   				}));

					// (iife_function)(element, "eventname", handler) - call the IIFE
					let iife_call = Expression::CallExpression(ctx.alloc(CallExpression {
						span: event_span,
						callee: Expression::ParenthesizedExpression(ctx.alloc(
							ParenthesizedExpression {
								span: event_span,
								expression: iife_function,
							},
						)),
						type_arguments: None,
						arguments: Vec::from_array_in(
							[
								Argument::from(target_ref),
								Argument::StringLiteral(ctx.alloc(StringLiteral {
									span: event_span,
									value: event_name,
									raw: None,
									lossy: false,
								})),
								Argument::from(handler_expr),
							],
							self.allocator,
						),
						optional: false,
						pure: false,
					}));

					// S(iife_call) - wrap the IIFE call in S()
					let s_wrapped = Expression::CallExpression(ctx.alloc(CallExpression {
						span: event_span,
						callee: Expression::Identifier(ctx.alloc(IdentifierReference {
							span: event_span,
							name: S,
							reference_id: Cell::new(Some(self.s_ref)),
						})),
						type_arguments: None,
						arguments: Vec::from_array_in([Argument::from(iife_call)], self.allocator),
						optional: false,
						pure: false,
					}));

					stmts.push(Statement::ExpressionStatement(ctx.alloc(
						ExpressionStatement {
							span: event_span,
							expression: s_wrapped,
						},
					)));
				}

				stmts
			};
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

		// Add event handler statements
		for stmt in event_stmts {
			elem.statements.push(stmt);
		}

		elem.statements
			.push(Statement::ReturnStatement(ctx.alloc(ReturnStatement {
				span: node.span,
				argument: Some(retval),
			})));
	}
}
