//! CLI for the Surplus JSX compiler.
use std::{
	io::{Read, Write},
	sync::Arc,
};

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
#[derive(Debug, Parser)]
struct Args {
	/// Where to output the bundle. Defaults to
	/// stdout; intermediate folders must exist
	#[arg(short = 'o', long = "output")]
	output: Option<String>,
	/// The name of the `S.js` package to import from
	/// if transformation occurs
	#[arg(short = 'S', long = "import", default_value = "@surplus/s")]
	import_sjs: String,
	/// Treat warnings as errors
	#[arg(short = 'W')]
	warnings_as_errors: bool,
	/// Don't minify the output
	#[arg(short = 'M', long = "no-minify")]
	no_minify: bool,
	/// The entry point of the Surplus bundle
	/// (defaults to stdin)
	entry_point: Option<String>,
	/// When set, enables sourcemaps (embedded in the output).
	#[arg(short = 'm', long = "map")]
	generate_sourcemaps: bool,
}

fn main() {
	let args = Args::parse();

	let source = if let Some(ref entry) = args.entry_point {
		std::fs::read_to_string(&entry).expect("failed to read entry point")
	} else {
		let mut str = String::with_capacity(4096);
		std::io::stdin()
			.read_to_string(&mut str)
			.expect("failed to read stdin");
		str
	};

	let source = Arc::new(source);
	let mut errors = 0;

	let allocator = Allocator::default();
	let parse_result = oxc::parser::Parser::new(&allocator, &source, SourceType::jsx()).parse();
	if parse_result.panicked || !parse_result.errors.is_empty() {
		if parse_result.errors.is_empty() {
			eprintln!("parser panicked, but emitted no errors");
			std::process::exit(1);
		} else {
			for mut error in parse_result.errors {
				if args.warnings_as_errors {
					error = error.with_severity(Severity::Error);
				}

				if error.severity == Severity::Error {
					errors += 1;
				}

				eprintln!("{:?}", error.with_source_code(Arc::clone(&source)));
			}
		}

		if errors > 0 {
			eprintln!("\nexiting due to {errors} errors");
			std::process::exit(1);
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

			eprintln!("{:?}", error.with_source_code(Arc::clone(&source)));
		}
		if errors > 0 {
			eprintln!("\nexiting due to {errors} errors");
			std::process::exit(1);
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

			eprintln!("{:?}", error.with_source_code(Arc::clone(&source)));
		}
		if errors > 0 {
			eprintln!("\nexiting due to {errors} errors");
			std::process::exit(1);
		}
	}

	let mut codegen_options = CodegenOptions::default();
	codegen_options.minify = !args.no_minify;
	codegen_options.comments = args.no_minify;
	codegen_options.source_map_path = Some(std::path::PathBuf::from("surplus.js.map"));

	let scoping = if args.no_minify {
		surplus_result.scoping
	} else {
		let semantic = SemanticBuilder::new()
			.with_check_syntax_error(false)
			.with_scope_tree_child_ids(true)
			.build(&program);

		assert!(semantic.errors.is_empty());

		let mut options = MangleOptions::default();
		options.keep_names = MangleOptionsKeepNames::all_false();
		options.top_level = true;

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
			eprintln!("warning: sourcemap generation requested, but no sourcemap was generated");
			None
		}
	} else {
		None
	};

	if let Some(output) = args.output {
		let mut fd = std::fs::File::create(&output).expect("failed to create output file");
		fd.write_all(generated.code.as_bytes())
			.expect("failed to write to output");
		if let Some(sm) = sourcemap_string {
			fd.write_all(b"//# sourceMappingURL=")
				.expect("failed to write sourcemap URL prelude");
			fd.write_all(sm.as_bytes())
				.expect("failed to write sourcemap to output");
		}
	} else {
		std::io::stdout()
			.write_all(generated.code.as_bytes())
			.expect("failed to write to stdout");
		if let Some(sm) = sourcemap_string {
			std::io::stdout()
				.write_all(b"//# sourceMappingURL=")
				.expect("failed to write sourcemap URL prelude");
			std::io::stdout()
				.write_all(sm.as_bytes())
				.expect("failed to write sourcemap to stdout");
		}
	}
}
