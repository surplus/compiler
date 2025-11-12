const { compile_internal } = require('./surplus-wasm/pkg/node.js');

module.exports.compile = function surplusCompiler({
	source,
	sourcemapFilename,
	importName,
	warningsAsErrors,
	minify,
	typescript,
}) {
	importName ??= '@surplus/s';
	warningsAsErrors = Boolean(warningsAsErrors ?? false);
	typescript = Boolean(typescript ?? false);
	const noMinify = !minify;
	const sourcemaps = Boolean(sourcemapFilename);

	if (!source)
		throw new TypeError('missing `source` property on options');
	if (typeof source !== 'string')
		throw new TypeError('`source` must be a string');
	if (sourcemapFilename && typeof sourcemapFilename !== 'string')
		throw new TypeError('`sourcemapFilename` must be a string');
	if (typeof importName !== 'string')
		throw new TypeError('`importName` must be a string');

	const r = compile_internal(source, sourcemapFilename || '', importName, warningsAsErrors, noMinify, sourcemaps, typescript);

	// This is required to activate getters.
	const code = r.code;
	const errors = r.errors;
	const warnings = r.warnings;

	return {
		code: code ? code : null,
		errors,
		warnings,
		get errorString() {
			if (this.errors.length === 0) return null;
			return this.errors.join('');
		},
		get warningString() {
			if (this.warnings.length === 0) return null;
			return this.warnings.join('');
		}
	};
}
