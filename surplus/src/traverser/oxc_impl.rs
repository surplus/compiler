use std::cell::Cell;

use oxc::{
	allocator::{Box, CloneIn, FromIn, Vec},
	ast::ast::{
		Argument, ArrayExpression, ArrowFunctionExpression, AssignmentExpression,
		AssignmentOperator, AssignmentTarget, BinaryExpression, BinaryOperator, BindingIdentifier,
		BindingPattern, BindingPatternKind, BooleanLiteral, CallExpression, ConditionalExpression,
		Expression, ExpressionStatement, FormalParameter, FormalParameterKind, FormalParameters,
		FunctionBody, IdentifierName, IdentifierReference, JSXAttribute, JSXAttributeName,
		JSXAttributeValue, JSXChild, JSXElement, JSXElementName, JSXExpression, JSXText,
		ObjectExpression, ObjectProperty, ObjectPropertyKind, ParenthesizedExpression, Program,
		PropertyKey, PropertyKind, ReturnStatement, Statement, StaticMemberExpression,
		StringLiteral, VariableDeclaration, VariableDeclarationKind, VariableDeclarator,
	},
	diagnostics::OxcDiagnostic,
	semantic::{NodeId, ReferenceFlags, ScopeFlags, SymbolFlags},
	span::Atom,
	syntax::number::NumberBase,
};
use oxc_traverse::{Traverse, TraverseCtx};

use crate::{
	constants::{
		CHILDREN, COMPILE, CREATE_ELEMENT, CREATE_ELEMENT_NS, CREATE_TEXT_NODE, DOCUMENT, FN_ATTR,
		K, NEW_PREV, PREV_ATTRS_IDENT, REF_ATTR, REMOVE_ATTRIBUTE, REMOVE_ATTRIBUTE_NS,
		REPLACE_CHILDREN, S, S_ELEM_IDENT, SET_ATTRIBUTE, SET_ATTRIBUTE_NS, UNDEFINED, V, VAL,
	},
	decode_html_entities,
	element::{ElemIdent, SurplusChildType, SurplusElement},
};

impl<'a> Traverse<'a> for super::SurplusTraverser<'a> {
	fn enter_jsx_element(&mut self, node: &mut JSXElement<'a>, ctx: &mut TraverseCtx<'a>) {
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

	fn exit_jsx_element(&mut self, node: &mut JSXElement<'a>, ctx: &mut TraverseCtx<'a>) {
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

	fn enter_jsx_attribute(&mut self, node: &mut JSXAttribute<'a>, ctx: &mut TraverseCtx<'a>) {
		if let Some(JSXAttributeValue::Element(_) | JSXAttributeValue::Fragment(_)) = &node.value {
			self.element_stack
				.push(SurplusElement::new_in(ctx, self.allocator));
		}
	}

	fn exit_jsx_attribute(&mut self, node: &mut JSXAttribute<'a>, ctx: &mut TraverseCtx<'a>) {
		let (needs_computation, is_natural, value) = if let Some(value) = &node.value {
			match value {
				JSXAttributeValue::StringLiteral(lit) => {
					(
						false,
						true,
						Expression::StringLiteral(lit.clone_in(self.allocator)),
					)
				}
				JSXAttributeValue::Element(elem) => {
					// Pop it off (we pushed it in `enter_jsx_element`).
					let attr_elem = self.element_stack.pop().unwrap();
					(
						false,
						false,
						attr_elem.into_surplus(elem.span, ctx, self.s_ref, self.allocator),
					)
				}
				JSXAttributeValue::Fragment(elem) => {
					// Pop it off (we pushed it in `enter_jsx_element`).
					let attr_elem = self.element_stack.pop().unwrap();
					(
						false,
						false,
						attr_elem.into_surplus(elem.span, ctx, self.s_ref, self.allocator),
					)
				}
				JSXAttributeValue::ExpressionContainer(expr) => {
					match &expr.expression {
						JSXExpression::JSXElement(_) | JSXExpression::JSXFragment(_) => {
							unreachable!()
						}
						expr => (true, true, expr.clone_in(self.allocator).into_expression()),
					}
				}
			}
		} else {
			// If this is a dom element, we have to give an empty string.
			// Otherwise, we set the custom element's property to `true`.
			if self
				.element_stack
				.last()
				.unwrap()
				.construction_obj
				.is_some()
			{
				(
					false,
					true,
					Expression::BooleanLiteral(ctx.alloc(BooleanLiteral {
						span: node.span,
						value: true,
					})),
				)
			} else {
				(
					false,
					true,
					Expression::StringLiteral(ctx.alloc(StringLiteral {
						span: node.span,
						value: Atom::new_const(""),
						raw: None,
						lossy: false,
					})),
				)
			}
		};

		// We have to do this after value processing as the value might need
		// to modify the element stack (in the case of an Element attribute value)
		// and this clause might access the target element in order to emit an error
		// in some cases.
		let (ns, key) = match &node.name {
			JSXAttributeName::Identifier(ident) => {
				(
					None,
					ctx.alloc(StringLiteral {
						span: ident.span,
						value: ident.name,
						raw: None,
						lossy: false,
					}),
				)
			}
			JSXAttributeName::NamespacedName(ident) => {
				(
					Some(ctx.alloc(StringLiteral {
						span: ident.namespace.span,
						value: ident.namespace.name,
						raw: None,
						lossy: false,
					})),
					ctx.alloc(StringLiteral {
						span: ident.name.span,
						value: ident.name.name,
						raw: None,
						lossy: false,
					}),
				)
			}
		};

		// Handle edge case attributes.
		if ns.is_none() {
			match key.value.as_str() {
				FN_ATTR => {
					// Make sure the expression is actually an expression.
					match &node.value {
						None => {
							self.errors.push(
								OxcDiagnostic::error("`fn` attribute with no expression")
									.with_help(
										"`fn` attribute takes a function that is called with the \
										 created element",
									)
									.with_label(node.span),
							);
						}
						Some(JSXAttributeValue::ExpressionContainer(expr)) => {
							self.element_stack
								.last_mut()
								.unwrap()
								.fn_expressions
								.push(expr.clone_in(self.allocator));
						}
						Some(_) => {
							self.errors.push(
								OxcDiagnostic::error("`fn` attribute with non-expression value")
									.with_help(
										"`fn` attribute takes a function that is called with the \
										 created element",
									)
									.with_label(node.span),
							);
						}
					}
					return;
				}
				REF_ATTR => {
					if let Some(value) = &node.value {
						if let JSXAttributeValue::ExpressionContainer(expr) = value {
							let last = self.element_stack.last_mut().unwrap().ref_var.replace((
								expr.expression.clone_in(self.allocator).into_expression(),
								expr.span,
							));
							if let Some((_, last_span)) = last {
								self.errors.push(
									OxcDiagnostic::warn(
										"Multiple `ref` attributes; only the last takes effect",
									)
									.with_label(expr.span)
									.and_label(last_span)
									.with_help("remove all but one `ref` attribute"),
								);
							}
						} else {
							self.errors.push(
								OxcDiagnostic::error(
									"`ref` attribute with invalid expression (expected \
									 `ref={identifier}`)",
								)
								.with_help(
									"`ref` attribute takes a variable name that is assigned a \
									 reference to the newly created node",
								)
								.with_label(node.span),
							);
						}
					} else {
						self.errors.push(
							OxcDiagnostic::error("`ref` attribute with no expression")
								.with_help(
									"`ref` attribute takes a variable name that is assigned a \
									 reference to the newly created node",
								)
								.with_label(node.span),
						);
					}
					return;
				}
				CHILDREN => {
					if self.element_stack.last().unwrap().self_closing {
						todo!("`children` attribute in self-closing tag");
					} else {
						self.errors.push(
							OxcDiagnostic::error(
								"`children` attribute is only allowed on self-closing tags",
							)
							.with_label(node.span),
						);
					}
					return;
				}
				_ => {}
			}
		}

		// We have to pull this off after preparing the values since
		// if the attribute has an element value, then it was pushed
		// onto the stack and isn't what we want to use (and thus we
		// have to allow the above code to pop it off first).
		if self
			.element_stack
			.last()
			.unwrap()
			.construction_obj
			.is_some()
		{
			if let Some(ns) = &ns {
				self.errors.push(
					OxcDiagnostic::error(
						"Cannot pass namespaced attributes to non-DOM (custom JSX) elements",
					)
					.with_help("remove the namespace in the attribute, or check tag name")
					.with_label(ns.span),
				);
			}

			self.element_stack
				.last_mut()
				.unwrap()
				.construction_obj
				.as_mut()
				.unwrap()
				.properties
				.push(ObjectPropertyKind::ObjectProperty(ctx.alloc(
					ObjectProperty {
						span: node.span,
						kind: PropertyKind::Init,
						key: PropertyKey::StringLiteral(key),
						value,
						method: false,
						shorthand: false,
						computed: false,
					},
				)));
		} else {
			if !is_natural {
				self.errors.push(
					OxcDiagnostic::warn(
						"expression is likely incorrect here; will call `.setAttribute()` with a \
						 JSX element as the value",
					)
					.with_label(node.span),
				);
			}

			let (name, remove_name) = if ns.is_some() {
				(
					Atom::new_const(SET_ATTRIBUTE_NS),
					Atom::new_const(REMOVE_ATTRIBUTE_NS),
				)
			} else {
				(
					Atom::new_const(SET_ATTRIBUTE),
					Atom::new_const(REMOVE_ATTRIBUTE),
				)
			};

			let mut args = Vec::with_capacity_in(3, self.allocator);

			if let Some(ns) = ns {
				args.push(Argument::StringLiteral(ns));
			}

			args.push(Argument::StringLiteral(key));

			let test_expression =
				Argument::ParenthesizedExpression(ctx.alloc(ParenthesizedExpression {
					span: node.span,
					expression: value,
				}));

			let expression = if needs_computation {
				args.push(Argument::Identifier(ctx.alloc(IdentifierReference {
					span: node.span,
					name: Atom::new_const(V),
					reference_id: Cell::new(None),
				})));

				let test_and_set_expr = Expression::ConditionalExpression(
					ctx.alloc(ConditionalExpression {
						span: node.span,
						test: Expression::BinaryExpression(ctx.alloc(BinaryExpression {
							span: node.span,
							left: Expression::Identifier(ctx.alloc(IdentifierReference {
								span: node.span,
								name: Atom::new_const(V),
								reference_id: Cell::new(None),
							})),
							operator: BinaryOperator::StrictEquality,
							right: Expression::Identifier(ctx.alloc(IdentifierReference {
								span: node.span,
								name: Atom::new_const(UNDEFINED),
								reference_id: Cell::new(None),
							})),
						})),
						consequent: Expression::CallExpression(
							ctx.alloc(CallExpression {
								span: node.span,
								callee: Expression::StaticMemberExpression(
									ctx.alloc(StaticMemberExpression {
										span: node.span,
										optional: false,
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
											name: remove_name,
										},
									}),
								),
								type_arguments: None,
								arguments: Vec::from_iter_in(
									args.iter().take(2).map(|a| a.clone_in(self.allocator)),
									self.allocator,
								),
								optional: false,
								pure: false,
							}),
						),
						alternate: Expression::CallExpression(
							ctx.alloc(CallExpression {
								span: node.span,
								callee: Expression::StaticMemberExpression(
									ctx.alloc(StaticMemberExpression {
										span: node.span,
										optional: false,
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
											name,
										},
									}),
								),
								type_arguments: None,
								arguments: args,
								optional: false,
								pure: false,
							}),
						),
					}),
				);

				self.s_expression(
					ctx,
					node.span,
					Expression::CallExpression(ctx.alloc(CallExpression {
						span: node.span,
						callee: Expression::ArrowFunctionExpression(ctx.alloc(
							ArrowFunctionExpression {
								span: node.span,
								expression: true,
								r#async: false,
								type_parameters: None,
								params: ctx.alloc(FormalParameters {
									span: node.span,
									kind: FormalParameterKind::ArrowFormalParameters,
									items: Vec::from_array_in(
										[FormalParameter {
											accessibility: None,
											r#override: false,
											decorators: Vec::new_in(self.allocator),
											readonly: false,
											span: node.span,
											pattern: BindingPattern {
												optional: false,
												type_annotation: None,
												kind: BindingPatternKind::BindingIdentifier(
													ctx.alloc(BindingIdentifier {
														span: node.span,
														name: Atom::new_const(V),
														symbol_id: Cell::new(None),
													}),
												),
											},
										}],
										self.allocator,
									),
									rest: None,
								}),
								return_type: None,
								body: ctx.alloc(FunctionBody {
									span: node.span,
									directives: Vec::new_in(self.allocator),
									statements: Vec::from_array_in(
										[Statement::ExpressionStatement(ctx.alloc(
											ExpressionStatement {
												span: node.span,
												expression: test_and_set_expr,
											},
										))],
										self.allocator,
									),
								}),
								scope_id: Cell::new(None),
								pure: false,
							},
						)),
						type_arguments: None,
						arguments: Vec::from_array_in([test_expression], self.allocator),
						optional: false,
						pure: true,
					})),
				)
			} else {
				args.push(test_expression);
				Expression::CallExpression(
					ctx.alloc(CallExpression {
						span: node.span,
						callee: Expression::StaticMemberExpression(
							ctx.alloc(StaticMemberExpression {
								span: node.span,
								optional: false,
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
									name,
								},
							}),
						),
						type_arguments: None,
						arguments: args,
						optional: false,
						pure: false,
					}),
				)
			};

			self.element_stack
				.last_mut()
				.unwrap()
				.statements
				.push(Statement::ExpressionStatement(ctx.alloc(
					ExpressionStatement {
						span: node.span,
						expression,
					},
				)));
		}
	}

	fn exit_jsx_spread_attribute(
		&mut self,
		node: &mut oxc::ast::ast::JSXSpreadAttribute<'a>,
		ctx: &mut TraverseCtx<'a>,
	) {
		if self
			.element_stack
			.last()
			.unwrap()
			.construction_obj
			.is_some()
		{
			self.element_stack
				.last_mut()
				.unwrap()
				.construction_obj
				.as_mut()
				.unwrap()
				.properties
				.push(ObjectPropertyKind::SpreadProperty(ctx.alloc(
					oxc::ast::ast::SpreadElement {
						span: node.span,
						argument: node.argument.clone_in(self.allocator),
					},
				)));
		} else {
			// DOM element: Create unique symbol for prev_keys.
			let prev_ident_str = PREV_ATTRS_IDENT;
			let prev_span = node.span;
			let prev_sym = ctx.scoping_mut().create_symbol(
				prev_span,
				prev_ident_str,
				SymbolFlags::Variable,
				self.element_stack.last().unwrap().scope,
				NodeId::DUMMY,
			);
			let prev_ident_binding = BindingIdentifier {
				span: prev_span,
				name: Atom::new_const(prev_ident_str),
				symbol_id: Cell::new(Some(prev_sym)),
			};

			let read_ref_id =
				ctx.create_reference(prev_ident_str, Some(prev_sym), ReferenceFlags::Read);
			let write_ref_id =
				ctx.create_reference(prev_ident_str, Some(prev_sym), ReferenceFlags::Write);

			let elem = self.element_stack.last().unwrap();

			let mut new_statements = Vec::new_in(self.allocator);

			// Add `let prev = {};` to statements.
			new_statements.push(Statement::VariableDeclaration(ctx.alloc(
				VariableDeclaration {
					span: prev_span,
					kind: VariableDeclarationKind::Let,
					declare: false,
					declarations: Vec::from_array_in(
						[VariableDeclarator {
							definite: false,
							span: prev_span,
							kind: VariableDeclarationKind::Let,
							id: BindingPattern {
								kind: BindingPatternKind::BindingIdentifier(
									ctx.alloc(prev_ident_binding.clone()),
								),
								type_annotation: None,
								optional: false,
							},
							init: Some(Expression::ObjectExpression(ctx.alloc(ObjectExpression {
								span: prev_span,
								properties: Vec::new_in(self.allocator),
								trailing_comma: None,
							}))),
						}],
						self.allocator,
					),
				},
			)));

			// Build the inner logic for the S computation.
			let v_ident = Atom::new_const(V);
			let val_ident = Atom::new_const(VAL);
			let new_prev_ident = Atom::new_const(NEW_PREV);
			let k_ident = Atom::new_const(K);

			// Arrow function scope.
			let arrow_scope = ctx.create_child_scope_of_current(ScopeFlags::Arrow);

			// Create scope for blocks with declarations.
			let block_scope = ctx.create_child_scope_of_current(ScopeFlags::empty());

			// Statements inside the arrow function.
			let arrow_statements = Vec::from_array_in(
				[
					// let new_prev = {};
					Statement::VariableDeclaration(ctx.alloc(VariableDeclaration {
						span: node.span,
						kind: VariableDeclarationKind::Let,
						declare: false,
						declarations: Vec::from_array_in(
							[VariableDeclarator {
								definite: false,
								span: node.span,
								kind: VariableDeclarationKind::Let,
								id: BindingPattern {
									kind: BindingPatternKind::BindingIdentifier(ctx.alloc(
										BindingIdentifier {
											span: node.span,
											name: new_prev_ident,
											symbol_id: Cell::new(None),
										},
									)),
									type_annotation: None,
									optional: false,
								},
								init: Some(Expression::ObjectExpression(ctx.alloc(
									ObjectExpression {
										span: node.span,
										properties: Vec::new_in(self.allocator),
										trailing_comma: None,
									},
								))),
							}],
							self.allocator,
						),
					})),
					// for (let k in v) { ... }
					{
						let for_in_scope = ctx.create_child_scope_of_current(ScopeFlags::empty());
						Statement::ForInStatement(ctx.alloc(oxc::ast::ast::ForInStatement {
                            span: node.span,
                            left: oxc::ast::ast::ForStatementLeft::VariableDeclaration(ctx.alloc(VariableDeclaration {
                                span: node.span,
                                kind: VariableDeclarationKind::Let,
                                declare: false,
                                declarations: Vec::from_array_in(
                                    [VariableDeclarator {
                                        definite: false,
                                        span: node.span,
                                        kind: VariableDeclarationKind::Let,
                                        id: BindingPattern {
                                            kind: BindingPatternKind::BindingIdentifier(ctx.alloc(BindingIdentifier {
                                                span: node.span,
                                                name: k_ident,
                                                symbol_id: Cell::new(None),
                                            })),
                                            type_annotation: None,
                                            optional: false,
                                        },
                                        init: None,
                                    }],
                                    self.allocator,
                                ),
                            })),
                            right: Expression::Identifier(ctx.alloc(IdentifierReference {
                                span: node.span,
                                name: v_ident,
                                reference_id: Cell::new(None),
                            })),
                            body: Statement::BlockStatement(ctx.alloc(oxc::ast::ast::BlockStatement {
                                span: node.span,
                                body: Vec::from_array_in(
                                    [
                                        // let val = v[k];
                                        Statement::VariableDeclaration(ctx.alloc(VariableDeclaration {
                                            span: node.span,
                                            kind: VariableDeclarationKind::Let,
                                            declare: false,
                                            declarations: Vec::from_array_in(
                                                [VariableDeclarator {
                                                    definite: false,
                                                    span: node.span,
                                                    kind: VariableDeclarationKind::Let,
                                                    id: BindingPattern {
                                                        kind: BindingPatternKind::BindingIdentifier(ctx.alloc(BindingIdentifier {
                                                            span: node.span,
                                                            name: val_ident,
                                                            symbol_id: Cell::new(None),
                                                        })),
                                                        type_annotation: None,
                                                        optional: false,
                                                    },
                                                    init: Some(Expression::ComputedMemberExpression(ctx.alloc(oxc::ast::ast::ComputedMemberExpression {
                                                        span: node.span,
                                                        object: Expression::Identifier(ctx.alloc(IdentifierReference {
                                                            span: node.span,
                                                            name: v_ident,
                                                            reference_id: Cell::new(None),
                                                        })),
                                                        expression: Expression::Identifier(ctx.alloc(IdentifierReference {
                                                            span: node.span,
                                                            name: k_ident,
                                                            reference_id: Cell::new(None),
                                                        })),
                                                        optional: false,
                                                    }))),
                                                }],
                                                self.allocator,
                                            ),
                                        })),
                                        // if (val !== undefined) { elem.setAttribute(k, val); new_prev[k] = 1; } else { elem.removeAttribute(k); }
                                        Statement::IfStatement(ctx.alloc(oxc::ast::ast::IfStatement {
                                            span: node.span,
                                            test: Expression::BinaryExpression(ctx.alloc(BinaryExpression {
                                                span: node.span,
                                                left: Expression::Identifier(ctx.alloc(IdentifierReference {
                                                    span: node.span,
                                                    name: val_ident,
                                                    reference_id: Cell::new(None),
                                                })),
                                                operator: BinaryOperator::StrictInequality,
                                                right: Expression::Identifier(ctx.alloc(IdentifierReference {
                                                    span: node.span,
                                                    name: Atom::new_const(UNDEFINED),
                                                    reference_id: Cell::new(None),
                                                })),
                                            })),
                                            consequent: Statement::BlockStatement(ctx.alloc(oxc::ast::ast::BlockStatement {
                                                span: node.span,
                                                body: Vec::from_array_in(
                                                    [
                                                        // elem.setAttribute(k, val);
                                                        Statement::ExpressionStatement(ctx.alloc(ExpressionStatement {
                                                            span: node.span,
                                                            expression: Expression::CallExpression(ctx.alloc(CallExpression {
                                                                span: node.span,
                                                                callee: Expression::StaticMemberExpression(ctx.alloc(StaticMemberExpression {
                                                                    span: node.span,
                                                                    object: elem.elem.as_ref().unwrap().ident.clone_in(self.allocator),
                                                                    property: IdentifierName {
                                                                        span: node.span,
                                                                        name: Atom::new_const(SET_ATTRIBUTE),
                                                                    },
                                                                    optional: false,
                                                                })),
                                                                type_arguments: None,
                                                                arguments: Vec::from_array_in(
                                                                    [
                                                                        Argument::Identifier(ctx.alloc(IdentifierReference {
                                                                            span: node.span,
                                                                            name: k_ident,
                                                                            reference_id: Cell::new(None),
                                                                        })),
                                                                        Argument::Identifier(ctx.alloc(IdentifierReference {
                                                                            span: node.span,
                                                                            name: val_ident,
                                                                            reference_id: Cell::new(None),
                                                                        })),
                                                                    ],
                                                                    self.allocator,
                                                                ),
                                                                optional: false,
                                                                pure: false,
                                                            })),
                                                        })),
                                                        // new_prev[k] = 1;
                                                        Statement::ExpressionStatement(ctx.alloc(ExpressionStatement {
                                                            span: node.span,
                                                            expression: Expression::AssignmentExpression(ctx.alloc(AssignmentExpression {
                                                                span: node.span,
                                                                operator: AssignmentOperator::Assign,
                                                                left: AssignmentTarget::ComputedMemberExpression(ctx.alloc(oxc::ast::ast::ComputedMemberExpression {
                                                                    span: node.span,
                                                                    object: Expression::Identifier(ctx.alloc(IdentifierReference {
                                                                        span: node.span,
                                                                        name: new_prev_ident,
                                                                        reference_id: Cell::new(None),
                                                                    })),
                                                                    expression: Expression::Identifier(ctx.alloc(IdentifierReference {
                                                                        span: node.span,
                                                                        name: k_ident,
                                                                        reference_id: Cell::new(None),
                                                                    })),
                                                                    optional: false,
                                                                })),
                                                                right: Expression::NumericLiteral(ctx.alloc(oxc::ast::ast::NumericLiteral {
                                                                    span: node.span,
                                                                    value: 1.0,
                                                                    raw: None,
                                                                    base: NumberBase::Decimal,
                                                                })),
                                                            })),
                                                        })),
                                                    ],
                                                    self.allocator,
                                                ),
                                                scope_id: Cell::new(None),
                                            })),
                                            alternate: Some(Statement::BlockStatement(ctx.alloc(oxc::ast::ast::BlockStatement {
                                                span: node.span,
                                                body: Vec::from_array_in(
                                                    [
                                                        // elem.removeAttribute(k);
                                                        Statement::ExpressionStatement(ctx.alloc(ExpressionStatement {
                                                            span: node.span,
                                                            expression: Expression::CallExpression(ctx.alloc(CallExpression {
                                                                span: node.span,
                                                                callee: Expression::StaticMemberExpression(ctx.alloc(StaticMemberExpression {
                                                                    span: node.span,
                                                                    object: elem.elem.as_ref().unwrap().ident.clone_in(self.allocator),
                                                                    property: IdentifierName {
                                                                        span: node.span,
                                                                        name: Atom::new_const(REMOVE_ATTRIBUTE),
                                                                    },
                                                                    optional: false,
                                                                })),
                                                                type_arguments: None,
                                                                arguments: Vec::from_array_in(
                                                                    [Argument::Identifier(ctx.alloc(IdentifierReference {
                                                                        span: node.span,
                                                                        name: k_ident,
                                                                        reference_id: Cell::new(None),
                                                                    }))],
                                                                    self.allocator,
                                                                ),
                                                                optional: false,
                                                                pure: false,
                                                            })),
                                                        })),
                                                    ],
                                                    self.allocator,
                                                ),
                                                scope_id: Cell::new(None),
                                            }))),
                                        })),
                                    ],
                                    self.allocator,
                                ),
                                scope_id: Cell::new(Some(block_scope)),
                            })),
                            scope_id: Cell::new(Some(for_in_scope)),
                        }))
					},
					// for (let k in prev) { if (!(k in new_prev)) { elem.removeAttribute(k); } }
					{
						let for_in_scope = ctx.create_child_scope_of_current(ScopeFlags::empty());
						Statement::ForInStatement(ctx.alloc(oxc::ast::ast::ForInStatement {
                            span: node.span,
                            left: oxc::ast::ast::ForStatementLeft::VariableDeclaration(ctx.alloc(VariableDeclaration {
                                span: node.span,
                                kind: VariableDeclarationKind::Let,
                                declare: false,
                                declarations: Vec::from_array_in(
                                    [VariableDeclarator {
                                        definite: false,
                                        span: node.span,
                                        kind: VariableDeclarationKind::Let,
                                        id: BindingPattern {
                                            kind: BindingPatternKind::BindingIdentifier(ctx.alloc(BindingIdentifier {
                                                span: node.span,
                                                name: k_ident,
                                                symbol_id: Cell::new(None),
                                            })),
                                            type_annotation: None,
                                            optional: false,
                                        },
                                        init: None,
                                    }],
                                    self.allocator,
                                ),
                            })),
                            right: Expression::Identifier(ctx.alloc(IdentifierReference {
                                span: node.span,
                                name: Atom::new_const(PREV_ATTRS_IDENT),
                                reference_id: Cell::new(Some(read_ref_id)),
                            })),
                            body: Statement::BlockStatement(ctx.alloc(oxc::ast::ast::BlockStatement {
                                span: node.span,
                                body: Vec::from_array_in(
                                    [Statement::IfStatement(ctx.alloc(oxc::ast::ast::IfStatement {
                                        span: node.span,
                                        test: Expression::UnaryExpression(ctx.alloc(oxc::ast::ast::UnaryExpression {
                                            span: node.span,
                                            operator: oxc::ast::ast::UnaryOperator::LogicalNot,
                                            argument: Expression::BinaryExpression(ctx.alloc(BinaryExpression {
                                                span: node.span,
                                                left: Expression::Identifier(ctx.alloc(IdentifierReference {
                                                    span: node.span,
                                                    name: k_ident,
                                                    reference_id: Cell::new(None),
                                                })),
                                                operator: BinaryOperator::In,
                                                right: Expression::Identifier(ctx.alloc(IdentifierReference {
                                                    span: node.span,
                                                    name: new_prev_ident,
                                                    reference_id: Cell::new(None),
                                                })),
                                            })),
                                        })),
                                        consequent: Statement::BlockStatement(ctx.alloc(oxc::ast::ast::BlockStatement {
                                            span: node.span,
                                            body: Vec::from_array_in(
                                                [Statement::ExpressionStatement(ctx.alloc(ExpressionStatement {
                                                    span: node.span,
                                                    expression: Expression::CallExpression(ctx.alloc(CallExpression {
                                                        span: node.span,
                                                        callee: Expression::StaticMemberExpression(ctx.alloc(StaticMemberExpression {
                                                            span: node.span,
                                                            object: elem.elem.as_ref().unwrap().ident.clone_in(self.allocator),
                                                            property: IdentifierName {
                                                                span: node.span,
                                                                name: Atom::new_const(REMOVE_ATTRIBUTE),
                                                            },
                                                            optional: false,
                                                        })),
                                                        type_arguments: None,
                                                        arguments: Vec::from_array_in(
                                                            [Argument::Identifier(ctx.alloc(IdentifierReference {
                                                                span: node.span,
                                                                name: k_ident,
                                                                reference_id: Cell::new(None),
                                                            }))],
                                                            self.allocator,
                                                        ),
                                                        optional: false,
                                                        pure: false,
                                                    })),
                                                }))],
                                                self.allocator,
                                            ),
                                            scope_id: Cell::new(None),
                                        })),
                                        alternate: None,
                                    }))],
                                    self.allocator,
                                ),
                                scope_id: Cell::new(None),
                            })),
                            scope_id: Cell::new(Some(for_in_scope)),
                        }))
					},
					// prev = new_prev;
					Statement::ExpressionStatement(ctx.alloc(ExpressionStatement {
						span: node.span,
						expression: Expression::AssignmentExpression(ctx.alloc(
							AssignmentExpression {
								span: node.span,
								operator: AssignmentOperator::Assign,
								left: AssignmentTarget::AssignmentTargetIdentifier(ctx.alloc(
									IdentifierReference {
										span: node.span,
										name: Atom::new_const(PREV_ATTRS_IDENT),
										reference_id: Cell::new(Some(write_ref_id)),
									},
								)),
								right: Expression::Identifier(ctx.alloc(IdentifierReference {
									span: node.span,
									name: new_prev_ident,
									reference_id: Cell::new(None),
								})),
							},
						)),
					})),
				],
				self.allocator,
			);

			// Arrow function: (v) => { ... }
			let arrow = ctx.alloc(ArrowFunctionExpression {
				span: node.span,
				expression: false,
				r#async: false,
				type_parameters: None,
				params: ctx.alloc(FormalParameters {
					span: node.span,
					kind: FormalParameterKind::ArrowFormalParameters,
					items: Vec::from_array_in(
						[FormalParameter {
							accessibility: None,
							r#override: false,
							decorators: Vec::new_in(self.allocator),
							readonly: false,
							span: node.span,
							pattern: BindingPattern {
								optional: false,
								type_annotation: None,
								kind: BindingPatternKind::BindingIdentifier(ctx.alloc(
									BindingIdentifier {
										span: node.span,
										name: v_ident,
										symbol_id: Cell::new(None),
									},
								)),
							},
						}],
						self.allocator,
					),
					rest: None,
				}),
				return_type: None,
				body: ctx.alloc(FunctionBody {
					span: node.span,
					directives: Vec::new_in(self.allocator),
					statements: arrow_statements,
				}),
				scope_id: Cell::new(Some(arrow_scope)),
				pure: false,
			});

			// S(arrow)
			let s_call = self.s_expression(
				ctx,
				node.span,
				Expression::CallExpression(ctx.alloc(CallExpression {
					span: node.span,
					callee: Expression::ArrowFunctionExpression(arrow),
					type_arguments: None,
					arguments: Vec::new_in(self.allocator),
					optional: false,
					pure: false,
				})),
			);

			// Add S(...)(spread_expr) to statements.
			new_statements.push(Statement::ExpressionStatement(ctx.alloc(
				ExpressionStatement {
					span: node.span,
					expression: Expression::CallExpression(ctx.alloc(CallExpression {
						span: node.span,
						callee: s_call,
						type_arguments: None,
						arguments: Vec::from_array_in(
							[node.argument.clone_in(self.allocator).into()],
							self.allocator,
						),
						optional: false,
						pure: false,
					})),
				},
			)));

			self.element_stack
				.last_mut()
				.unwrap()
				.statements
				.extend(new_statements);
		}
	}

	fn enter_jsx_child(&mut self, node: &mut JSXChild<'a>, ctx: &mut TraverseCtx<'a>) {
		if matches!(node, JSXChild::Element(_) | JSXChild::Fragment(_)) {
			self.element_stack
				.push(SurplusElement::new_in(ctx, self.allocator));
		}
	}

	fn exit_jsx_child(&mut self, node: &mut JSXChild<'a>, ctx: &mut TraverseCtx<'a>) {
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
