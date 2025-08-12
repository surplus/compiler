#![expect(private_interfaces)]
use wasm_bindgen::prelude::*;

#[wasm_bindgen(getter_with_clone)]
#[derive(Clone)]
struct CompilationResult {
	pub code: String,
	pub warnings: Vec<String>,
	pub errors: Vec<String>,
}

/// `source` is the Surplus JSX source code to compile.
#[wasm_bindgen]
pub fn compile_internal(
	source: &str,
	source_name: &str,
	import_sjs: &str,
	warnings_as_errors: bool,
	no_minify: bool,
	generate_sourcemaps: bool,
) -> Result<CompilationResult, String> {
	let args = surplus_cli::Args {
		import_sjs: import_sjs.into(),
		warnings_as_errors,
		no_minify,
		entry_point: if source_name.is_empty() {
			None
		} else {
			Some(source_name.into())
		},
		generate_sourcemaps,
	};

	surplus_cli::run(source.into(), &args)
		.map(|result| {
			CompilationResult {
				code: result.code,
				warnings: result.warnings,
				errors: result.errors,
			}
		})
		.map_err(|e| format!("fatal error during compilation: {e}"))
}
