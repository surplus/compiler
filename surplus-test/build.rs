#![allow(
	missing_docs,
	clippy::missing_docs_in_private_items,
	clippy::similar_names
)]

use std::{collections::HashMap, io::Write, path::PathBuf};

use quote::quote;

#[derive(Debug)]
struct TestSuite {
	jsx_file: PathBuf,
	cases: HashMap<String, PathBuf>,
}

fn main() {
	println!("cargo:rerun-if-changed=tests/");
	println!("cargo:rerun-if-changed=build.rs");
	println!("cargo:rerun-if-changed=package.json");

	check_node_version();

	std::process::Command::new("npm")
		.arg("install")
		.status()
		.expect("Failed to run 'npm install'");

	let suites = discover_suites();
	let out_dir = PathBuf::from(std::env::var("OUT_DIR").unwrap());
	let dest_path = out_dir.join("tests.rs");

	let mut file = std::fs::File::create(dest_path).unwrap();

	for (suite_name, suite) in suites {
		let jsx_src = std::fs::read_to_string(&suite.jsx_file).unwrap_or_else(|_| {
			panic!(
				"Failed to read JSX fixture file: {}",
				suite.jsx_file.display()
			)
		});

		let mut tests = vec![];

		for (case_name, case_path) in suite.cases {
			let js_src = std::fs::read_to_string(&case_path).unwrap_or_else(|_| {
				panic!("Failed to read JS test-case file: {}", case_path.display())
			});

			let test_fn_name = syn::Ident::new(&case_name, cs());
			let suite_path = suite.jsx_file.to_string_lossy().to_string();

			let case_path_str = case_path.to_string_lossy().to_string();

			tests.push(quote! {
				#[test]
				fn #test_fn_name() {
					const CASE_SRC: &str = #js_src;

					use std::io::Write;
					use base64::prelude::*;

					let output = surplus_cli::run(JSX_SRC.to_string(), &surplus_cli::Args {
						entry_point: Some(#suite_path.into()),
						output: None,
						typescript: false,
						warnings_as_errors: true,
						generate_sourcemaps: false,
						import_sjs: "@surplus/s".to_string(),
						no_minify: true,
					}).expect("Compilation failed");

					assert!(output.warnings.is_empty(), "surplus library emitted warnings when asked not to");

					if !output.errors.is_empty() {
						for error in &output.errors {
							eprintln!("{error}");
						}
						panic!("compilation failed");
					}

					eprintln!("--------------- JSX SOURCE ---------------");
					eprintln!("{}", output.code);

					let mut child = std::process::Command::new("node")
						.arg("--experimental-vm-modules")
						.arg("harness.mjs")
						.stdin(std::process::Stdio::piped())
						.stdout(std::process::Stdio::piped())
						.stderr(std::process::Stdio::piped())
						.spawn()
						.expect("failed to spawn node test harness");

					let mut stdin = child.stdin.take().expect("failed to open stdin");
					let suite_filename_b64 = BASE64_STANDARD.encode(#suite_path);
					let case_filename_b64 = BASE64_STANDARD.encode(#case_path_str);
					let jsx_b64 = BASE64_STANDARD.encode(output.code);
					let case_b64 = BASE64_STANDARD.encode(CASE_SRC);
					stdin.write_all(format!("{suite_filename_b64}|{case_filename_b64}|{jsx_b64}|{case_b64}").as_bytes())
						.expect("failed to write to stdin");
					drop(stdin);
					let output = child
						.wait_with_output()
						.expect("failed to read node test harness output");

					eprintln!("--------------- CASE STDOUT ---------------");
					eprintln!("{}", String::from_utf8_lossy(&output.stdout));
					eprintln!("--------------- CASE STDERR ---------------");
					eprintln!("{}", String::from_utf8_lossy(&output.stderr));

					assert!(output.status.success(), "test case failed");
				}
			});
		}

		let mod_name = syn::Ident::new(&suite_name, cs());
		let ts = quote! {
			#[cfg(test)]
			mod #mod_name {
				const JSX_SRC: &str = #jsx_src;

				#(#tests)*
			}
		};

		let f = syn::parse2::<syn::File>(ts).unwrap();

		write!(file, "{}", prettyplease::unparse(&f)).unwrap();
	}
}

fn check_node_version() {
	let output = std::process::Command::new("node")
		.arg("--version")
		.output()
		.expect("Failed to execute 'node --version'");

	assert!(output.status.success(), "'node --version' failed to run");

	let version_str = String::from_utf8_lossy(&output.stdout);
	let version_str = version_str.trim_start_matches('v').trim();

	let parts: Vec<&str> = version_str.split('.').collect();
	assert!(
		parts.len() >= 2,
		"Unexpected node version format: {version_str}"
	);

	let major: u32 = parts[0].parse().unwrap_or(0);
	let minor: u32 = parts[1].parse().unwrap_or(0);
	let patch: u32 = if parts.len() >= 3 {
		parts[2].parse().unwrap_or(0)
	} else {
		0
	};

	assert!(
		!(major < 22 || (major == 22 && minor < 21) || (major == 22 && minor == 21 && patch < 1)),
		"Node.js version 22.21.1 or higher is required. Found: {version_str}"
	);
}

fn cs() -> proc_macro2::Span {
	proc_macro2::Span::call_site()
}

fn discover_suites() -> HashMap<String, TestSuite> {
	// 1. Crawl `CARGO_MANIFEST_DIR/tests` for `.jsx` and `.js` files.
	// 2. Suites are `<suite>.jsx`; cases are `<suite>.<case>.js`.
	// 3. Panic on any case without a matching suite.
	// 4. Panic on any folders.
	// 5. Panic on any suites without cases.
	// 6. Panic on any filenames that do not match the above patterns.
	// 7. Suite names and test case names must be valid Rust identifiers,
	//    and must be snake case. Panic if not.
	let mut suites = HashMap::new();
	let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
	let tests_dir = PathBuf::from(manifest_dir).join("tests");
	for entry in std::fs::read_dir(tests_dir).unwrap() {
		let entry = entry.unwrap();
		let path = entry.path();
		assert!(
			!path.is_dir(),
			"Directories are not allowed in the tests/ folder: {}",
			path.display()
		);
		let filename = path.file_name().unwrap().to_string_lossy();
		if let Some(suite_name) = filename.strip_suffix(".jsx") {
			assert!(
				is_valid_rust_identifier(suite_name),
				"Suite name is not a valid Rust identifier: {suite_name}"
			);
			suites.insert(
				suite_name.to_string(),
				TestSuite {
					jsx_file: path,
					cases: HashMap::new(),
				},
			);
		} else if let Some(suite_pair) = filename.strip_suffix(".js") {
			let parts: Vec<&str> = suite_pair.rsplitn(2, '.').collect();
			assert!(
				parts.len() == 2,
				"Test case filename does not match pattern <suite>.<case>.js: {filename}"
			);
			let case_name = parts[0];
			let suite_name = parts[1];
			assert!(
				is_valid_rust_identifier(suite_name),
				"Suite name is not a valid Rust identifier: {suite_name}"
			);
			assert!(
				is_valid_rust_identifier(case_name),
				"Case name is not a valid Rust identifier: {case_name}"
			);
			let suite = suites.get_mut(suite_name).unwrap_or_else(|| {
				panic!("Test case '{case_name}' has no matching suite '{suite_name}'")
			});
			suite.cases.insert(case_name.to_string(), path);
		} else {
			panic!("Invalid file in tests/ folder: {filename}");
		}
	}
	for (suite_name, suite) in &suites {
		assert!(
			!suite.cases.is_empty(),
			"Suite '{suite_name}' has no test cases"
		);
	}
	suites
}

fn is_valid_rust_identifier(name: &str) -> bool {
	let mut chars = name.chars();
	match chars.next() {
		Some(c) if c.is_ascii_alphabetic() || c == '_' => (),
		_ => return false,
	}
	for c in chars {
		if !(c.is_ascii_alphanumeric() || c == '_') {
			return false;
		}
	}
	true
}
