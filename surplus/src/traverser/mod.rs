mod oxc_impl;

use std::cell::Cell;

use oxc::{
	allocator::{Allocator, CloneIn, Vec},
	ast::ast::{
		Argument, ArrowFunctionExpression, CallExpression, Expression, ExpressionStatement,
		FormalParameterKind, FormalParameters, FunctionBody, IdentifierName, IdentifierReference,
		JSXIdentifier, JSXMemberExpressionObject, Statement, StaticMemberExpression,
	},
	diagnostics::OxcDiagnostic,
	semantic::ReferenceId,
	span::Span,
};
use oxc_traverse::TraverseCtx;

use crate::{constants::S, element::SurplusElement};

/// Traverser for Surplus JSX elements.
pub struct SurplusTraverser<'a> {
	/// The reference to the `S` identifier
	pub s_ref: ReferenceId,
	/// Whether a transformation was performed.
	pub performed_transformation: bool,
	/// The stack of Surplus elements being processed.
	pub element_stack: Vec<'a, SurplusElement<'a>>,
	/// The underlying Bumpalo allocator.
	pub allocator: &'a Allocator,
	/// Any errors that were emitted during compilation.
	pub errors: std::vec::Vec<OxcDiagnostic>,
}

impl<'a> SurplusTraverser<'a> {
	/// Creates a new Surplus traverser using the given Bumpalo allocator.
	pub fn new_in(s_ref: ReferenceId, allocator: &'a Allocator) -> Self {
		Self {
			s_ref,
			performed_transformation: false,
			element_stack: Vec::new_in(allocator),
			allocator,
			errors: std::vec::Vec::new(),
		}
	}

	/// Transforms the given set of statements to a Surplus computation
	/// expression.
	#[expect(dead_code)]
	pub fn s_computation<const N: usize>(
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

	/// Transforms the given expression into a Surplus computation using an
	/// expression arrow function.
	pub fn s_expression(
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

	/// Helper for converting a JSX member expression to an expression.
	pub fn member_to_expression(
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
