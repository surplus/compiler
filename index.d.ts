/**
 * Options for the compile function.
 */
export interface CompileOptions {
    /**
     * The JSX source code to compile.
     */
    source: string;

    /**
     * If specified, sourcemaps are generated inline using the given string as the original
	 * filename.
	 *
	 * Do not set this property if you do not want sourcemaps to be generated.
     */
    sourcemapFilename?: string;

    /**
     * The name of the `S.js` package to import.
     * @default '@surplus/s'
     */
    importName?: string;

    /**
     * Treat warnings as errors.
     * @default false
     */
    warningsAsErrors?: boolean;

    /**
     * Minify the output.
     * @default false
     */
    minify?: boolean;

	/**
	 * Allow TypeScript syntax in the input.
	 * @default false
	 */
	typescript?: boolean;
}

/**
 * Result of the compilation process.
 */
export interface CompileResult {
    /**
     * The compiled code, or null if compilation failed.
     */
    code: string | null;

    /**
     * Array of error messages from the compilation.
     */
    errors: string[];

    /**
     * Array of warning messages from the compilation.
     */
    warnings: string[];

    /**
     * Joined string of all error messages, or null if no errors.
     */
    readonly errorString: string | null;

    /**
     * Joined string of all warning messages, or null if no warnings.
     */
    readonly warningString: string | null;
}

/**
 * Compiles JSX source code using the Surplus compiler.
 * @param options Compilation options.
 * @returns The compilation result.
 */
export function compile(options: CompileOptions): CompileResult;
