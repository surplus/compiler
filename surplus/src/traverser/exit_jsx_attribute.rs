use std::cell::Cell;

use oxc::{
	allocator::{CloneIn, Vec},
	ast::ast::{
		Argument, ArrowFunctionExpression, BinaryExpression, BinaryOperator, BindingIdentifier,
		BindingPattern, BindingPatternKind, BooleanLiteral, CallExpression, ConditionalExpression,
		Expression, ExpressionStatement, FormalParameter, FormalParameterKind, FormalParameters,
		FunctionBody, IdentifierName, IdentifierReference, JSXAttribute, JSXAttributeName,
		JSXAttributeValue, JSXExpression, ObjectProperty, ObjectPropertyKind,
		ParenthesizedExpression, PropertyKey, PropertyKind, Statement, StaticMemberExpression,
		StringLiteral,
	},
	diagnostics::OxcDiagnostic,
	span::Atom,
};
use oxc_traverse::TraverseCtx;

use crate::constants::{
	CHILDREN, FN_ATTR, REF_ATTR, REMOVE_ATTRIBUTE, REMOVE_ATTRIBUTE_NS, SET_ATTRIBUTE,
	SET_ATTRIBUTE_NS, UNDEFINED, V,
};

impl<'a> super::SurplusTraverser<'a> {
	pub(crate) fn inner_exit_jsx_attribute(
		&mut self,
		node: &mut JSXAttribute<'a>,
		ctx: &mut TraverseCtx<'a>,
	) {
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

		// Handle event namespace (on:eventname)
		if let Some(ns_lit) = &ns
			&& ns_lit.value.as_str() == crate::constants::EVENT_NS
		{
			// Store event handler for processing in exit_jsx_element
			self.element_stack
				.last_mut()
				.unwrap()
				.event_handlers
				.push((key.value, value, node.span));
			return;
		}

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
}
