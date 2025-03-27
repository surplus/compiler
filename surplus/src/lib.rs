//! # Surplus
//! This is the core library for the Surplus JSX compiler.

use std::cell::Cell;

use oxc::{
	allocator::{Allocator, Box, CloneIn, FromIn, Vec},
	ast::ast::{
		Argument, ArrayExpression, ArrowFunctionExpression, AssignmentExpression,
		AssignmentOperator, AssignmentTarget, BinaryExpression, BinaryOperator, BindingIdentifier,
		BindingPattern, BindingPatternKind, BooleanLiteral, CallExpression, ConditionalExpression,
		Expression, ExpressionStatement, FormalParameter, FormalParameterKind, FormalParameters,
		FunctionBody, IdentifierName, IdentifierReference, ImportDeclaration,
		ImportDeclarationSpecifier, ImportDefaultSpecifier, ImportOrExportKind, JSXAttribute,
		JSXAttributeName, JSXAttributeValue, JSXChild, JSXElement, JSXElementName, JSXExpression,
		JSXExpressionContainer, JSXIdentifier, JSXMemberExpressionObject, ObjectExpression,
		ObjectProperty, ObjectPropertyKind, ParenthesizedExpression, Program, PropertyKey,
		PropertyKind, ReturnStatement, Statement, StaticMemberExpression, StringLiteral,
		VariableDeclaration, VariableDeclarationKind, VariableDeclarator,
	},
	diagnostics::OxcDiagnostic,
	semantic::{
		NodeId, Reference, ReferenceFlags, ReferenceId, ScopeFlags, ScopeId, Scoping, SymbolFlags,
		SymbolId,
	},
	span::{Atom, Span},
};
use oxc_traverse::{Traverse, TraverseCtx};

const S_IDENT: &str = "__$S__";
const S: Atom<'static> = Atom::new_const(S_IDENT);

/// Return value from [`transform()`].
///
/// Be sure to check [`SurplusTransformResult::errors`]
/// to see if anything failed during transformation.
pub struct SurplusTransformResult {
	/// The [`Scoping`] instance after transformation.
	pub scoping: Scoping,
	/// Any errors that were emitted during compilation.
	///
	/// Should not be ignored; an error means that some
	/// portions of the code were simply skipped in the
	/// modified AST!
	pub errors: std::vec::Vec<OxcDiagnostic>,
}

/// Transforms Surplus JSX in [`Program`], in-place.
pub fn transform<'a>(
	allocator: &'a Allocator,
	program: &mut Program<'a>,
	mut scoping: Scoping,
	s_import_from: &'a str,
) -> SurplusTransformResult {
	let unresolved_s =
		scoping.create_reference(Reference::new(NodeId::DUMMY, ReferenceFlags::read()));
	let mut traverser = SurplusTraverser::new_in(unresolved_s, allocator);
	scoping.add_root_unresolved_reference(S_IDENT, unresolved_s);
	let mut scoping = oxc_traverse::traverse_mut(&mut traverser, allocator, program, scoping);

	if traverser.performed_transformation {
		let s_sym = scoping.create_symbol(
			Span::default(),
			S_IDENT,
			SymbolFlags::Import,
			scoping.root_scope_id(),
			NodeId::DUMMY,
		);
		scoping.add_resolved_reference(s_sym, unresolved_s);

		program.body.insert(
			0,
			Statement::ImportDeclaration(Box::new_in(
				ImportDeclaration {
					import_kind: ImportOrExportKind::Value,
					phase: None,
					span: Span::empty(0),
					specifiers: Some(Vec::from_array_in(
						[ImportDeclarationSpecifier::ImportDefaultSpecifier(
							Box::new_in(
								ImportDefaultSpecifier {
									span: Span::empty(0),
									local: BindingIdentifier {
										span: Span::empty(0),
										name: S,
										symbol_id: Cell::new(Some(s_sym)),
									},
								},
								allocator,
							),
						)],
						allocator,
					)),
					source: StringLiteral {
						lossy: false,
						raw: None,
						value: Atom::from_in(s_import_from, allocator),
						span: Span::empty(0),
					},
					with_clause: None,
				},
				allocator,
			)),
		);
	}

	assert!(traverser.element_stack.is_empty());

	SurplusTransformResult {
		scoping,
		errors: traverser.errors,
	}
}

struct SurplusTraverser<'a> {
	s_ref: ReferenceId,
	performed_transformation: bool,
	element_stack: Vec<'a, SurplusElement<'a>>,
	allocator: &'a Allocator,
	errors: std::vec::Vec<OxcDiagnostic>,
}

impl<'a> SurplusTraverser<'a> {
	fn new_in(s_ref: ReferenceId, allocator: &'a Allocator) -> Self {
		Self {
			s_ref,
			performed_transformation: false,
			element_stack: Vec::new_in(allocator),
			allocator,
			errors: std::vec::Vec::new(),
		}
	}
}

impl<'a> SurplusTraverser<'a> {
	#[expect(dead_code)]
	fn s_computation<const N: usize>(
		&self,
		ctx: &mut TraverseCtx<'a>,
		span: Span,
		stmts: [Statement<'a>; N],
	) -> Expression<'a> {
		Expression::CallExpression(ctx.alloc(CallExpression {
			span,
			callee: Expression::Identifier(ctx.alloc(IdentifierReference {
				span: Span::default(),
				name: S,
				reference_id: Cell::new(Some(self.s_ref)),
			})),
			type_arguments: None,
			arguments: Vec::from_array_in(
				[Argument::ArrowFunctionExpression(ctx.alloc(
					ArrowFunctionExpression {
						span,
						expression: false,
						r#async: false,
						type_parameters: None,
						params: ctx.alloc(FormalParameters {
							span,
							kind: FormalParameterKind::ArrowFormalParameters,
							items: Vec::new_in(self.allocator),
							rest: None,
						}),
						return_type: None,
						body: ctx.alloc(FunctionBody {
							span,
							directives: Vec::new_in(self.allocator),
							statements: Vec::from_array_in(stmts, self.allocator),
						}),
						scope_id: Cell::new(None),
						pure: false,
					},
				))],
				self.allocator,
			),
			optional: false,
			pure: false,
		}))
	}

	fn s_expression(
		&self,
		ctx: &mut TraverseCtx<'a>,
		span: Span,
		expr: Expression<'a>,
	) -> Expression<'a> {
		Expression::CallExpression(ctx.alloc(CallExpression {
			span,
			callee: Expression::Identifier(ctx.alloc(IdentifierReference {
				span: Span::default(),
				name: S,
				reference_id: Cell::new(Some(self.s_ref)),
			})),
			type_arguments: None,
			arguments: Vec::from_array_in(
				[Argument::ArrowFunctionExpression(ctx.alloc(
					ArrowFunctionExpression {
						span,
						expression: true,
						r#async: false,
						type_parameters: None,
						params: ctx.alloc(FormalParameters {
							span,
							kind: FormalParameterKind::ArrowFormalParameters,
							items: Vec::new_in(self.allocator),
							rest: None,
						}),
						return_type: None,
						body: ctx.alloc(FunctionBody {
							span,
							directives: Vec::new_in(self.allocator),
							statements: Vec::from_array_in(
								[Statement::ExpressionStatement(ctx.alloc(
									ExpressionStatement {
										span,
										expression: expr,
									},
								))],
								self.allocator,
							),
						}),
						scope_id: Cell::new(None),
						pure: false,
					},
				))],
				self.allocator,
			),
			optional: false,
			pure: false,
		}))
	}

	fn member_to_expression(
		&self,
		ctx: &mut TraverseCtx<'a>,
		object: &JSXMemberExpressionObject<'a>,
		property: &JSXIdentifier<'a>,
	) -> Expression<'a> {
		match object {
			JSXMemberExpressionObject::ThisExpression(expr) => {
				Expression::ThisExpression(expr.clone_in(self.allocator))
			}
			JSXMemberExpressionObject::IdentifierReference(expr) => {
				Expression::Identifier(expr.clone_in(self.allocator))
			}
			JSXMemberExpressionObject::MemberExpression(member) => {
				let member_expr = self.member_to_expression(ctx, &member.object, &member.property);
				Expression::StaticMemberExpression(ctx.alloc(StaticMemberExpression {
					span: member.span,
					object: member_expr,
					property: IdentifierName {
						span: property.span,
						name: property.name,
					},
					optional: false,
				}))
			}
		}
	}
}

impl<'a> Traverse<'a> for SurplusTraverser<'a> {
	fn enter_jsx_element(&mut self, node: &mut JSXElement<'a>, ctx: &mut TraverseCtx<'a>) {
		const IDENT: &str = "__$S_ELEM__";

		assert!(!node.opening_element.self_closing || node.children.is_empty());

		let elem = self.element_stack.last_mut().unwrap();

		elem.self_closing = node.opening_element.self_closing;

		let ident_span = node
			.opening_element
			.name
			.get_identifier()
			.map(|i| i.span)
			.unwrap_or_else(|| node.opening_element.span);
		let ident_sym = ctx.scoping_mut().create_symbol(
			ident_span,
			IDENT,
			SymbolFlags::ConstVariable,
			elem.scope,
			NodeId::DUMMY,
		);
		let ident_ref = ctx.create_reference(IDENT, Some(ident_sym), ReferenceFlags::Write);

		let ident = Expression::Identifier(ctx.alloc(IdentifierReference {
			name: Atom::new_const("__$S_ELEM__"),
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
			JSXElementName::MemberExpression(_) => true,
			JSXElementName::NamespacedName(_) => false,
			JSXElementName::ThisExpression(_) => true,
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
			ident,
			sym: ident_sym,
			name: Atom::new_const(IDENT),
		});
		assert!(old_ident.is_none());
	}

	fn exit_jsx_element(&mut self, node: &mut JSXElement<'a>, ctx: &mut TraverseCtx<'a>) {
		// Assemble the child list.

		let child_array = ArrayExpression {
			span: node.span,
			elements: Vec::from_iter_in(
				self.element_stack
					.last_mut()
					.unwrap()
					.child_exprs
					.drain(..)
					.map(|expr| expr.into()),
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
								name: Atom::new_const("children"),
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
				JSXElementName::ThisExpression(_) => unreachable!(),
				JSXElementName::MemberExpression(_) => unreachable!(),
			};

			assert!(name.chars().next().is_some_and(char::is_lowercase));

			let create_name = if ns.is_none() {
				"createElement"
			} else {
				"createElementNS"
			};
			let mut args = Vec::with_capacity_in(2, self.allocator);
			if let Some((ns, span)) = ns {
				args.push(Argument::StringLiteral(ctx.alloc(StringLiteral {
					span,
					value: ns,
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
							name: Atom::new_const("compile"),
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
									name: Atom::new_const("replaceChildren"),
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
												name: Atom::new_const("document"),
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
				"fn" => {
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
				"ref" => {
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
				"children" => {
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
					Atom::new_const("setAttributeNS"),
					Atom::new_const("removeAttributeNS"),
				)
			} else {
				(
					Atom::new_const("setAttribute"),
					Atom::new_const("removeAttribute"),
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
					name: Atom::new_const("v"),
					reference_id: Cell::new(None),
				})));

				let test_and_set_expr = Expression::ConditionalExpression(
					ctx.alloc(ConditionalExpression {
						span: node.span,
						test: Expression::BinaryExpression(ctx.alloc(BinaryExpression {
							span: node.span,
							left: Expression::Identifier(ctx.alloc(IdentifierReference {
								span: node.span,
								name: Atom::new_const("v"),
								reference_id: Cell::new(None),
							})),
							operator: BinaryOperator::StrictEquality,
							right: Expression::Identifier(ctx.alloc(IdentifierReference {
								span: node.span,
								name: Atom::new_const("undefined"),
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
														name: Atom::new_const("v"),
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
						arguments: Vec::from_array_in([test_expression.into()], self.allocator),
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
				self.element_stack
					.last_mut()
					.unwrap()
					.child_exprs
					.push(child.into_surplus_comp(e.span, ctx, self.s_ref, self.allocator));
			}
			JSXChild::Fragment(e) => {
				let child = self
					.element_stack
					.pop()
					.expect("surplus element stack underflow");
				self.element_stack.last_mut().unwrap().child_exprs.push(
					Expression::ArrayExpression(ctx.alloc(ArrayExpression {
						span: e.span,
						elements: Vec::from_iter_in(
							child.child_exprs.into_iter().map(|e| e.into()),
							self.allocator,
						),
						trailing_comma: None,
					})),
				);
			}
			JSXChild::Text(text) => {
				// Skip any purely-whitespace text.
				if text.value.trim().is_empty() {
					return;
				}

				self.element_stack.last_mut().unwrap().child_exprs.push(
					Expression::CallExpression(ctx.alloc(CallExpression {
						span: text.span,
						callee: Expression::StaticMemberExpression(ctx.alloc(
							StaticMemberExpression {
								span: text.span,
								object: Expression::Identifier(ctx.alloc(IdentifierReference {
									span: text.span,
									name: Atom::new_const("document"),
									reference_id: Cell::new(None),
								})),
								property: IdentifierName {
									span: text.span,
									name: Atom::new_const("createTextNode"),
								},
								optional: false,
							},
						)),
						type_arguments: None,
						arguments: Vec::from_array_in(
							[Argument::StringLiteral(ctx.alloc(StringLiteral {
								span: text.span,
								value: text.value,
								raw: text.raw,
								lossy: false,
							}))],
							self.allocator,
						),
						optional: false,
						pure: false,
					})),
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
					.push(expr_sexpr);
			}
			JSXChild::Spread(_expr) => {
				todo!("JSXChild::Spread");
			}
		}
	}

	fn enter_jsx_expression(&mut self, node: &mut JSXExpression<'a>, ctx: &mut TraverseCtx<'a>) {
		match node {
			JSXExpression::JSXElement(_) => {
				self.element_stack
					.push(SurplusElement::new_in(ctx, self.allocator));
			}
			JSXExpression::JSXFragment(_) => {
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
				*node = Expression::ArrayExpression(ctx.alloc(ArrayExpression {
					span: elem.span,
					elements: Vec::from_iter_in(
						last_elem.child_exprs.into_iter().map(|child| child.into()),
						self.allocator,
					),
					trailing_comma: None,
				}));
				self.performed_transformation = true;
			}
			_ => {}
		}
	}

	fn exit_program(&mut self, _node: &mut Program<'a>, _ctx: &mut TraverseCtx<'a>) {
		assert!(self.element_stack.is_empty());
	}
}

struct ElemIdent<'a> {
	name: Atom<'a>,
	sym: SymbolId,
	ident: Expression<'a>,
}
struct SurplusElement<'a> {
	self_closing: bool,
	statements: Vec<'a, Statement<'a>>,
	elem: Option<ElemIdent<'a>>,
	scope: ScopeId,
	construction_obj: Option<ObjectExpression<'a>>,
	fn_expressions: Vec<'a, Box<'a, JSXExpressionContainer<'a>>>,
	ref_var: Option<(Expression<'a>, Span)>,
	child_exprs: Vec<'a, Expression<'a>>,
}

impl<'a> SurplusElement<'a> {
	fn new_in(ctx: &mut TraverseCtx<'a>, allocator: &'a Allocator) -> Self {
		Self {
			self_closing: false,
			statements: Vec::new_in(allocator),
			elem: None,
			scope: ctx.create_child_scope_of_current(ScopeFlags::Arrow),
			construction_obj: None,
			fn_expressions: Vec::new_in(allocator),
			ref_var: None,
			child_exprs: Vec::new_in(allocator),
		}
	}

	fn into_surplus_comp(
		self,
		span: Span,
		ctx: &mut TraverseCtx<'a>,
		s_ref: ReferenceId,
		allocator: &'a Allocator,
	) -> Expression<'a> {
		// Should have been handled by the transformers before we got here.
		assert!(self.construction_obj.is_none());
		assert!(self.ref_var.is_none());

		let scope = ctx.create_child_scope_of_current(ScopeFlags::Arrow);

		let function_body = ctx.alloc(FunctionBody {
			directives: Vec::from_array_in([], allocator),
			span,
			statements: self.statements,
		});

		let computation = ctx.alloc(ArrowFunctionExpression {
			r#async: false,
			span,
			expression: false,
			type_parameters: None,
			scope_id: Cell::new(Some(scope)),
			params: ctx.alloc(FormalParameters {
				span: Span::default(),
				kind: FormalParameterKind::ArrowFormalParameters,
				items: Vec::new_in(allocator),
				rest: None,
			}),
			return_type: None,
			pure: false,
			body: function_body,
		});

		Expression::CallExpression(ctx.alloc(CallExpression {
			arguments: Vec::from_array_in(
				[Argument::ArrowFunctionExpression(computation)],
				allocator,
			),
			optional: false,
			pure: false,
			span,
			type_arguments: None,
			callee: Expression::Identifier(ctx.alloc(IdentifierReference {
				name: S,
				span: Span::default(),
				reference_id: Cell::new(Some(s_ref)),
			})),
		}))
	}

	fn into_surplus(
		self,
		span: Span,
		ctx: &mut TraverseCtx<'a>,
		s_ref: ReferenceId,
		allocator: &'a Allocator,
	) -> Expression<'a> {
		let expression = self.into_surplus_comp(span, ctx, s_ref, allocator);
		Expression::ParenthesizedExpression(ctx.alloc(ParenthesizedExpression {
			span,
			expression: Expression::CallExpression(ctx.alloc(CallExpression {
				arguments: Vec::new_in(allocator),
				span,
				type_arguments: None,
				pure: false,
				optional: false,
				callee: expression,
			})),
		}))
	}
}
