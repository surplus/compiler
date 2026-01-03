# Surplus Test Suite

The Surplus test suite is a [JSDOM](https://github.com/jsdom/jsdom)-based
custom test harness that tests most aspects related to Surplus.

There are test 'suites' and 'cases' - for every suite, there exists
a single `tests/<suite_name>.jsx` file, and one or more test cases
in `tests/<suite_name>.<case_name>.jsx`. These are automatically
build as `cargo test` cases.

Suite names become `mod suite_name { /* cases */ }`. Thus, suite names
_must_ be valid Rust `mod`ule identifiers.

The harness expects the `<suite_name>.jsx` file to `export default`
an element (_not_ an element function) or fragment (or array)
that is first appended to an otherwise empty HTML document's `<body>`.
This occurs cleanly for each and every test case; no test case risks
sharing any state from any other case.

Then, the case code is run. It's expected to be an asynchronous module;
no exports necessary. It runs immediately after the element is mounted.

The typical `window`, `document`, etc. are made available to the context.
Further, a number of assertion helpers are installed (after mounting,
before test case code runs) that includes a `$()` function (which is
a wrapper around `document.body.querySelectorAll()`, by default
targeting the actual elements that were mounted) as well as a number
of _intrusive_ `.prototype` functions useful for testing values.

This test case API can be seen and extended in `/harness-api.mjs`. As a rule,
any equality checks use `.is()`, and all errors that propagate upward
are caught, set as a `.cause` on a new error object providing context,
and re-thrown.

The harness also outputs a lot of debugging information. Most of the time
this information can be ignored, but it is printed in full if a test case
fails. It includes the transpiled JSX output from Surplus, followed by
any and all output from the test case - including stacktraces. These
stacktraces will then include the exact, contextual cause for assertion
failure due to the extended `.cause`-based assertions.

## Adding a Suite

Add a new "suite" (harness component, testing some aspect of Surplus)
by adding a `some_suite.jsx` file in the `tests/` directory. It is automatically
picked up by the `build.rs` of the `surplus-test` crate and will be included
on next `cargo test`.

Every suite _must_ have at least one test case (see below);
otherwise, `build.rs` will panic.

It should `export default` the component to mount (_not_ a function that returns
an element).

For example:

```jsx
const MyRootComponent = () => <div>My Root!</div>;

//export default MyRootComponent;    // WRONG - exports the function, not an Element.
export default <MyRootComponent />;  // Correct
```

While atypical of real applications, the harness also supports default exports
that are fragments (`<>...</>`), and - by proxy - arrays (`[e1, e2, ...]`).
All of them are appended to the `<body>`, in order, during the mount phase
of the test case. They are not wrapped in any element, each becoming a _direct_
child of `<body>`.

## Adding a Case

To add a case to a new or existing suite (see above), it _must_ be named
`<suite_name>.some_case.js`, where `some_case` becomes the rust `#[test] fn some_case()`
function name. Thus, case names _must_ be valid Rust function identifier names.

The code within each test case is guaranteed to be run after the corresponding
suite elements have been compiled and mounted onto `<body>`. Test case code
is run immediately, and can be `async`/`await`.

Further, a number of assertion helpers are included as part of this environment,
installed _after_ the suite code has run and the exported element has been
mounted.

This code is located in `harness-api.mjs`, and includes - among other things -
the `$()` function and various intrusive `.prototype`-based assertion functions.

The `$()` function takes a CSS selector string and queries against the mounted
component directly using `querySelectAll`. Unliked that method, it returns
a _real_ array.

From there, various assertions can be made about primitive values, or against
`Element` types, as method invocations - many of which are chainable. See
the `harness-api.mjs` for all provided methods (and feel free to add any
you feel are helpful but missing), and see the various existing test cases
to see how they're most often used.

For example, to make sure that a `<div>Hello!</div>` only mounts a single
`<div>` with the text `Hello!`, one might write:

```javascript
$('*').only().isTag('div');
$('div').only().text().is('Hello!');
```

where `$('*')` recursively selects all elements, `$('div')` recursively
selects all `<div>` elements, `.only()` ensures that exactly one item
is returned (or fails with an error), `isTag()` checks the tag name
of an element, `.text()` is a convenience function that
returns the text content of an element, and `.is()` performs an
equality check on steroids.

## Restrictions

Test cases and suite fixtures are limited to a single allowed import:

```javascript
import S from '@surplus/s';
```

No other imports are allowed, and will error if attempted.

## Broken suites/cases and Ignoring Them

If a test case is worthwhile to keep tracked and tested against
(e.g. for TDD purposes) -- which should be an _infrequent_ case --
this can be done by prefixing the _entire_ file with a single underscore.

This will annotate either the individual test case, or all test cases
in the suite, with `#[ignored]`. Under a normal `cargo test`, these tests
do not affect the overall test result (including in Surplus's CI/CD pipeline)
but can be selectively enabled via `cargo test -- --ignored`.

To re-enable it, remove the leading underscore.
