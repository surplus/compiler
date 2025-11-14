//! # Surplus
//! This is the core library for the Surplus JSX compiler.

pub(crate) mod constants;
pub(crate) mod traverser;
pub(crate) mod element;

use std::cell::Cell;

use oxc::{
	allocator::{Allocator, Box, FromIn, Vec},
	ast::ast::{
		BindingIdentifier,
		ImportDeclaration, ImportDeclarationSpecifier, ImportDefaultSpecifier, ImportOrExportKind, Program, Statement,
		StringLiteral,
	},
	diagnostics::OxcDiagnostic,
	semantic::{
		NodeId, Reference, ReferenceFlags, Scoping, SymbolFlags,
	},
	span::{Atom, Span},
};

use crate::{
	constants::{S, S_IDENT},
	traverser::SurplusTraverser,
};

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

/// Helper function to decode HTML entities in a string literal.
fn decode_html_entities<'a>(allocator: &'a Allocator, value: Atom<'a>) -> Atom<'a> {
	let decoded = htmlentity::entity::decode(value.as_bytes()).bytes();
	let decoded = String::from_utf8_lossy(decoded.as_ref());
	Atom::from_in(decoded.as_ref(), allocator)
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

	debug_assert!(traverser.element_stack.is_empty());

	SurplusTransformResult {
		scoping,
		errors: traverser.errors,
	}
}
