use std::cell::Cell;

use oxc::{
	allocator::{Allocator, Box, Vec},
	ast::ast::{
		Argument, ArrowFunctionExpression, CallExpression, Expression, FormalParameterKind,
		FormalParameters, FunctionBody, IdentifierReference, JSXExpressionContainer,
		ObjectExpression, ParenthesizedExpression, Statement,
	},
	semantic::{ReferenceId, ScopeFlags, ScopeId, SymbolId},
	span::{Atom, Span},
};
use oxc_traverse::TraverseCtx;

use crate::constants::S;

/// An identifier for a Surplus element.
pub struct ElemIdent<'a> {
	/// The name of the element, e.g. `div`, `span`, etc.
	pub name: Atom<'a>,
	/// The symbol ID of the element, used for references.
	pub sym: SymbolId,
	/// The identifier reference for the element, used for creating the element.
	pub ident: Expression<'a>,
}

/// Marker enum for identifying the contextual function lf a child expression.
/// This is used to determine whether or not to emit filling whitespace between
/// sibling elements.
pub enum SurplusChildType<'a> {
	/// A non-whitespace filler element.
	Normal(Expression<'a>),
	/// A whitespace filler element, which is used to fill gaps between elements.
	Whitespace(Expression<'a>),
}

impl<'a> SurplusChildType<'a> {
	/// Returns whether this child type is a whitespace filler.
	pub fn is_whitespace(&self) -> bool {
		matches!(self, SurplusChildType::Whitespace(_))
	}

	/// Converts this child type into an expression.
	pub fn unwrap(self) -> Expression<'a> {
		match self {
			SurplusChildType::Normal(expr) | SurplusChildType::Whitespace(expr) => expr,
		}
	}
}

/// A Surplus element, which is a custom JSX element that
/// can be transformed into a Surplus component.
pub struct SurplusElement<'a> {
	/// Whether this element is self-closing.
	pub self_closing: bool,
	/// The statements that will be executed when this element is created.
	pub statements: Vec<'a, Statement<'a>>,
	/// The element identifier, if this is a Surplus element.
	pub elem: Option<ElemIdent<'a>>,
	/// The scope in which this element is created.
	pub scope: ScopeId,
	/// The object that is constructed for this element.
	pub construction_obj: Option<ObjectExpression<'a>>,
	/// Any `fn={...}` attributes that are present on this element.
	pub fn_expressions: Vec<'a, Box<'a, JSXExpressionContainer<'a>>>,
	/// The `ref` attribute target expression, if present.
	pub ref_var: Option<(Expression<'a>, Span)>,
	/// The child expressions of this element, which are the children of the JSX element.
	pub child_exprs: Vec<'a, SurplusChildType<'a>>,
}

impl<'a> SurplusElement<'a> {
	/// Creates a new Surplus element with the given context and allocator.
	pub fn new_in(ctx: &mut TraverseCtx<'a>, allocator: &'a Allocator) -> Self {
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

	/// Converts this Surplus element into an S.js computation.
	pub fn into_surplus_comp(
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

	/// Converts this Surplus element into a Surplus component expression.
	pub fn into_surplus(
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
