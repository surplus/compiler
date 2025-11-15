# Surplus Test Suite

A quick primer on how the tests work:

- Write/edit tests in `src/tests/*.jsx`. **Do not edit `src/tests/*.compiled.js`.**
- Run `cargo run -p surplus-test` to generated the compiled outputs. Run with `--check`
  to perform existence checks.
- Check the generated output for correctness.
- Run `cargo test` to actually run the tests.

It's not a perfect system, but it guards against compiler outputs being changed
without expecting those changes.
