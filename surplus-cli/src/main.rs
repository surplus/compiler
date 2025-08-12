//! CLI for the Surplus JSX compiler.

use std::io::{Read, Write};

use clap::Parser;

fn main() {
	let args = surplus_cli::Args::parse();

	let source = if let Some(ref entry) = args.entry_point {
		std::fs::read_to_string(entry)
			.map_err(|e| format!("failed to read entry point: {e}"))
			.expect("fatal error")
	} else {
		let mut str = String::with_capacity(4096);
		std::io::stdin()
			.read_to_string(&mut str)
			.map_err(|e| format!("failed to read stdin: {e}"))
			.expect("fatal error");
		str
	};

	let result = surplus_cli::run(source, &args).expect("fatal error during compilation");

	if !result.errors.is_empty() {
		for error in &result.errors {
			eprintln!("{error}");
		}
		std::process::exit(1);
	}

	if !result.warnings.is_empty() {
		for warning in &result.warnings {
			eprintln!("warning: {warning}");
		}
	}

	if let Some(output) = args.output {
		std::fs::write(output, result.code)
			.map_err(|e| format!("failed to write output file: {e}"))
			.expect("failed to write output file");
	} else {
		std::io::stdout()
			.write_all(result.code.as_bytes())
			.expect("failed to write to stdout");
	}
}
