// Main entry point for the test harness.

import vm from "node:vm";
import fsp from "node:fs/promises";
import {URL} from "node:url";
import {JSDOM} from "jsdom";

async function readStdin() {
	const chunks = [];
	for await (let chunk of process.stdin) {
		chunks.push(chunk);
	}
	return Buffer.concat(chunks).toString();
}

function resolve(impname) {
	const url = import.meta.resolve(impname);
	const u = new URL(url);
	if (u.protocol !== 'file:') {
		throw new Error('Cannot convert non-file URL to path: ' + url);
	}
	return u.pathname;
}

const stdinRaw = await readStdin();
const [suiteFilenameB64, caseFilenameB64, jsxB64, caseB64] = stdinRaw.split("|");
const suiteFilename = Buffer.from(suiteFilenameB64, "base64").toString();
const caseFilename = Buffer.from(caseFilenameB64, "base64").toString();
const jsxSrc = Buffer.from(jsxB64, "base64").toString();
const caseSrc = Buffer.from(caseB64, "base64").toString();
const harnessApiPath = resolve('./harness-api.mjs');
const harnessApiSrc = await fsp.readFile(harnessApiPath, "utf-8");

const dom = new JSDOM(`<!DOCTYPE html><html><body></body></html>`, {
	url: "http://localhost/",
	runScripts: "outside-only",
	resources: "usable",
});

const root = dom.window;
root.console = console;

const context = vm.createContext(root);
const surplusSPath = resolve('@surplus/s');
const sMod = new vm.SourceTextModule(await fsp.readFile(surplusSPath, "utf-8"), { identifier: surplusSPath, context });

await sMod.link(() => {}); // Has no dependencies.
await sMod.evaluate({
	timeout: 5000,
	breakOnSigint: true,
});

const S = sMod.namespace.default;

let ranCompletely = false;
await S.unsafeAsyncRoot(async (disposeRoot) => {
	const suiteScript = new vm.SourceTextModule(jsxSrc, { identifier: suiteFilename, context });
	const caseScript = new vm.SourceTextModule(caseSrc, { identifier: caseFilename, context });
	const harnessApiScript = new vm.SourceTextModule(harnessApiSrc, { identifier: harnessApiPath, context });

	async function linker(specifier, referencingModule, extra) {
		if (specifier === '@surplus/s') {
			return sMod;
		}

		throw new Error(`Test attempted to load module: ${specifier}`);
	}

	await suiteScript.link(linker);
	await caseScript.link(linker);
	await harnessApiScript.link(linker);

	await suiteScript.evaluate({
		timeout: 5000,
		breakOnSigint: true,
	});
	const rootElement = suiteScript.namespace.default;

	if (!rootElement) {
		throw new Error("Test suite did not export a root element");
	}

	const children = root.Array.isArray(rootElement) ? rootElement : [rootElement];
	children.forEach(child => root.document.body.appendChild(child));

	// Loads the harness API, which sets up global.$ and adds a bunch of
	// intrusive `.prototype` methods for assertions.
	await harnessApiScript.evaluate({
		timeout: 5000,
		breakOnSigint: true,
	});

	// Initial test case runs. This might also set up cleanup computations
	// that themselves perform more assertions, so we have to make sure
	// to dispose the root after this.
	await caseScript.evaluate({
		timeout: 5000,
		breakOnSigint: true,
	});

	// Clear the DOM.
	root.document.body.textContent = "";
	if (root.document.body.children.length !== 0) {
		throw new Error("Failed to clear DOM between test cases");
	}

	// Allows any DOM cleanup to occur via `S.onCleanup` handlers.
	disposeRoot();

	// Must be last. This is a failsafe to ensure that the harness ran to completion.
	ranCompletely = true;
});

if (!ranCompletely) {
	throw new Error("Test did not complete running; something didn't await properly");
}
