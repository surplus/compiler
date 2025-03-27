//! CLI for the Surplus JSX compiler.
use std::{
	io::{Read, Write},
	sync::Arc,
};

use clap::Parser;
use oxc::{
	allocator::Allocator, codegen::Codegen, diagnostics::Severity, semantic::SemanticBuilder,
	span::SourceType,
};

/// The Surplus JSX compiler.
#[derive(Debug, Parser)]
struct Args {
	/// Where to output the bundle. Defaults to
	/// stdout. Intermediate folders must exist.
	#[arg(short = 'o', long = "output")]
	output: Option<String>,
	/// The name of the `S.js` package to import from
	/// if transformation occurs.
	#[arg(short = 'S', long = "import", default_value = "@surplus/s")]
	import_sjs: String,
	/// Treat warnings as errors.
	#[arg(short = 'W')]
	warnings_as_errors: bool,
	/// The entry point of the Surplus bundle.
	/// If not provided, uses stdin.
	entry_point: Option<String>,
}

fn main() {
	let args = Args::parse();

	let source = if let Some(entry) = args.entry_point {
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

	let generated = Codegen::new().build(&program);
	if let Some(output) = args.output {
		std::fs::write(output, &generated.code).expect("failed to write to output");
	} else {
		std::io::stdout()
			.write_all(generated.code.as_bytes())
			.expect("failed to write to stdout");
	}
}
