//! High level CLI-as-a-library for the Surplus compiler.
//!
//! This is directly called to by the `surplus` CLI binary.
//! It's not intended to be used directly by end users; it's a thin
//! wrapper primarily for use by the WASM wrapper crate.
use std::sync::Arc;

#[cfg(feature = "clap")]
use clap::Parser;
use oxc::{
	allocator::Allocator,
	codegen::{Codegen, CodegenOptions},
	diagnostics::Severity,
	mangler::{MangleOptions, MangleOptionsKeepNames},
	semantic::SemanticBuilder,
	span::SourceType,
};

/// The Surplus JSX compiler.
#[cfg_attr(feature = "clap", derive(Debug, Parser))]
pub struct Args {
	/// Where to output the bundle. Defaults to
	/// stdout; intermediate folders must exist
	#[cfg_attr(feature = "clap", arg(short = 'o', long = "output"))]
	#[cfg(feature = "clap")]
	pub output: Option<String>,
	/// The name of the `S.js` package to import from
	/// if transformation occurs
	#[cfg_attr(
		feature = "clap",
		arg(short = 'S', long = "import", default_value = "@surplus/s")
	)]
	pub import_sjs: String,
	/// Treat warnings as errors
	#[cfg_attr(feature = "clap", arg(short = 'W'))]
	pub warnings_as_errors: bool,
	/// Don't minify the output
	#[cfg_attr(feature = "clap", arg(short = 'M', long = "no-minify"))]
	pub no_minify: bool,
	/// When set, enables sourcemaps (embedded in the output).
	#[cfg_attr(feature = "clap", arg(short = 'm', long = "map"))]
	pub generate_sourcemaps: bool,
	/// The entry point of the Surplus bundle
	/// (defaults to stdin)
	pub entry_point: Option<String>,
	/// Allow typescript syntax in the input
	#[cfg_attr(feature = "clap", arg(short = 'T', long = "typescript"))]
	pub typescript: bool,
}

/// The `Ok` result type for the [`run`] function.
pub struct Compilation {
	/// The generated code.
	pub code: String,
	/// Any warnings
	pub warnings: Vec<String>,
	/// Any errors; if non-empty, `code` will be empty.
	pub errors: Vec<String>,
}

/// Runs the Surplus compiler with the given arguments.
///
/// This is identical to running the surplus CLI (except for parsing the arguments).
///
/// `Err` results indicate fatal errors that prevent compilation from completing.
/// This does **not** include warnings or syntax errors, which are included in the [`Compilation`]
/// result.
pub fn run(source: String, args: &Args) -> Result<Compilation, Box<dyn std::error::Error>> {
	let mut result = Compilation {
		code: String::new(),
		warnings: Vec::new(),
		errors: Vec::new(),
	};

	let source = Arc::new(source);
	let mut errors = 0;

	let allocator = Allocator::default();
	let parse_result = oxc::parser::Parser::new(
		&allocator,
		&source,
		if args.typescript {
			SourceType::tsx()
		} else {
			SourceType::jsx()
		},
	)
	.parse();

	if parse_result.panicked || !parse_result.errors.is_empty() {
		if parse_result.errors.is_empty() {
			return Err("parser panicked, but no errors were reported".into());
		}

		for mut error in parse_result.errors {
			if args.warnings_as_errors {
				error = error.with_severity(Severity::Error);
			}

			if error.severity == Severity::Error {
				errors += 1;
			}

			result
				.errors
				.push(format!("{:?}", error.with_source_code(Arc::clone(&source))));
		}

		if errors > 0 {
			return Ok(result);
		}
	}

	let mut program = parse_result.program;

	let semantic = SemanticBuilder::new()
		.with_check_syntax_error(true)
		.build(&program);

	if !semantic.errors.is_empty() {
		errors = 0;
		for mut error in semantic.errors {
			if args.warnings_as_errors {
				error = error.with_severity(Severity::Error);
			}

			if error.severity == Severity::Error {
				errors += 1;
			}

			result
				.errors
				.push(format!("{:?}", error.with_source_code(Arc::clone(&source))));
		}

		if errors > 0 {
			return Ok(result);
		}
	}

	let scoping = semantic.semantic.into_scoping();
	let surplus_result = surplus::transform(&allocator, &mut program, scoping, &args.import_sjs);

	if !surplus_result.errors.is_empty() {
		errors = 0;
		for mut error in surplus_result.errors {
			if args.warnings_as_errors {
				error = error.with_severity(Severity::Error);
			}

			if error.severity == Severity::Error {
				errors += 1;
			}

			result
				.errors
				.push(format!("{:?}", error.with_source_code(Arc::clone(&source))));
		}

		if errors > 0 {
			return Ok(result);
		}
	}

	let codegen_options = CodegenOptions {
		minify: !args.no_minify,
		comments: args.no_minify,
		source_map_path: if args.generate_sourcemaps {
			if let Some(ref entry) = args.entry_point {
				Some(entry.into())
			} else {
				Some("surplus.js.map".into())
			}
		} else {
			None
		},
		..CodegenOptions::default()
	};

	let scoping = if args.no_minify {
		surplus_result.scoping
	} else {
		let semantic = SemanticBuilder::new()
			.with_check_syntax_error(false)
			.with_scope_tree_child_ids(true)
			.build(&program);

		debug_assert!(semantic.errors.is_empty());

		let options = MangleOptions {
			keep_names: MangleOptionsKeepNames::all_false(),
			top_level: true,
			..MangleOptions::default()
		};

		oxc::mangler::Mangler::new()
			.with_options(options)
			.build_with_semantic(semantic.semantic, &program)
	};

	let generated = Codegen::new()
		.with_options(codegen_options)
		.with_scoping(Some(scoping))
		.build(&program);

	let sourcemap_string = if args.generate_sourcemaps {
		if let Some(ref sourcemap) = generated.map {
			Some(sourcemap.to_data_url())
		} else {
			result
				.warnings
				.push("sourcemap generation requested, but no sourcemap was generated".into());
			None
		}
	} else {
		None
	};

	result.code = generated.code;
	if let Some(ref sm) = sourcemap_string {
		result.code.push_str("\n//# sourceMappingURL=");
		result.code.push_str(sm);
	}

	Ok(result)
}
