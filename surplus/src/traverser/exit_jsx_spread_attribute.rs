use std::cell::Cell;

use oxc::{
	allocator::{CloneIn, Vec},
	ast::ast::{
		Argument, ArrowFunctionExpression, AssignmentExpression, AssignmentOperator,
		AssignmentTarget, BinaryExpression, BinaryOperator, BindingIdentifier, BindingPattern,
		BindingPatternKind, CallExpression, Expression, ExpressionStatement, FormalParameter,
		FormalParameterKind, FormalParameters, FunctionBody, IdentifierName, IdentifierReference,
		ObjectExpression, ObjectPropertyKind, Statement, StaticMemberExpression,
		VariableDeclaration, VariableDeclarationKind, VariableDeclarator,
	},
	semantic::{NodeId, ReferenceFlags, ScopeFlags, SymbolFlags},
	span::Atom,
	syntax::number::NumberBase,
};
use oxc_traverse::TraverseCtx;

use crate::constants::{
	K, NEW_PREV, PREV_ATTRS_IDENT, REMOVE_ATTRIBUTE, SET_ATTRIBUTE, UNDEFINED, V, VAL,
};

impl<'a> super::SurplusTraverser<'a> {
	pub fn inner_exit_jsx_spread_attribute(
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
}
