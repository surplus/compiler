// This implements the testcase API for surplus-test.
//
// This is loaded directly before the case code runs,
// after the JSX code for the suite has been loaded and evaluated.

window.$ = (...args) => [].slice.call(document.body.querySelectorAll(...args));

const valueOf = (v) => typeof v?.valueOf === 'function' ? v.valueOf() : v;
const toString = (v) => typeof v?.toString === 'function' ? v.toString() : Object.prototype.toString.call(v);
const valueToString = (v) => toString(valueOf(v));

Array.prototype.is = (o) => {
	if (!Array.isArray(o)) throw new Error(`.is() argument is not an array`);
	if (this.length !== o.length) {
		throw new Error(`.is() array length mismatch: expected ${o.length}, got ${this.length}`);
	}
	for (let i = 0; i < this.length; i++) {
		try {
			this[i].is(o[i]);
		} catch (e) {
			const e2 = new Error(`.is() array element ${i} mismatch`);
			e2.cause = e;
			throw e2;
		}
	}
}

Array.prototype.only = function() {
	if (this.length !== 1) {
		throw new Error(`.only() array length mismatch: expected 1, got ${this.length}`);
	}
	return this[0];
}

String.prototype.is = function(o) {
	o = valueToString(o);
	if (typeof o !== 'string') throw new Error(`.is() argument is not a string`);
	if (valueOf(this) !== o) {
		throw new Error(`.is() string mismatch: expected "${o}", got "${valueOf(this)}"`);
	}
}

Boolean.prototype.is = function(o) {
	o = Boolean(valueOf(o));
	if (typeof o !== 'boolean') throw new Error(`.is() argument is not a boolean`);
	if (valueOf(this) !== o) {
		throw new Error(`.is() boolean mismatch: expected ${o}, got ${valueOf(this)}`);
	}
}

Number.prototype.is = function(o) {
	const us = valueOf(this);
	o = Number(valueOf(o));
	if (typeof o !== 'number') throw new Error(`.is() argument is not a number`);
	if (Number.isNaN(o) && !Number.isNaN(us)) throw new Error(`.is() number mismatch: expected NaN, got ${us}`);
	if (Number.isNaN(us) && !Number.isNaN(o)) throw new Error(`.is() number mismatch: expected ${o}, got NaN`);
	if (us !== o) throw new Error(`.is() number mismatch: expected ${o}, got ${valueOf(this)}`);
}

Element.prototype.is = function(o) {
	// We do a basic check here; more complex checks can be done
	// by the test case itself if needed.
	if (!(o instanceof Element)) throw new Error(`.is() argument is not an Element`);
	if (this.tagName !== o.tagName) {
		throw new Error(`.is() Element tagName mismatch: expected <${o.tagName}>, got <${this.tagName}>`);
	}
	if (this !== o) {
		throw new Error(`.is() Element mismatch: expected ${o}, got ${this}`);
	}
}

Promise.prototype.is = async function(o) {
	throw new Error(`.is() cannot be used directly on a Promise; await it first`);
}

Object.prototype.is = function(o) {
	o = valueOf(o);
	if (typeof o !== 'object' || o === null) throw new Error('.is() argument is not an object');
	const thisKeys = Object.keys(this);
	const oKeys = Object.keys(o);
	if (thisKeys.length !== oKeys.length) {
		throw new Error(`.is() object key count mismatch: expected ${oKeys.length}, got ${thisKeys.length}`);
	}
	const missingKeys = [];
	for (const key of thisKeys) {
		if (!oKeys.includes(key)) {
			missingKeys.push(key);
		}
	}
	if (missingKeys.length > 0) {
		throw new Error(`.is() object missing keys: ${missingKeys.join(', ')}`);
	}
	const extraKeys = [];
	for (const key of oKeys) {
		if (!thisKeys.includes(key)) {
			extraKeys.push(key);
		}
	}
	if (extraKeys.length > 0) {
		throw new Error(`.is() object has extra keys: ${extraKeys.join(', ')}`);
	}
	for (const key of thisKeys) {
		try {
			this[key].is(o[key]);
		} catch (e) {
			const e2 = new Error(`.is() object property "${key}" mismatch`);
			e2.cause = e;
			throw e2;
		}
	}
}

Element.prototype.isTag = function(tagName) {
	const oTagName = valueToString(tagName).toUpperCase();
	const thisTagName = this.tagName.toUpperCase();
	try {
		thisTagName.is(oTagName);
	} catch (e) {
		const e2 = new Error(`.isTag() tagName mismatch`);
		e2.cause = e;
		throw e2;
	}
}

Element.prototype.exactProps = function(props) {
	props = valueOf(props);
	if (typeof props !== 'object' || props === null) {
		throw new Error(`.exactProps() argument is not an object`);
	}
	const thisProps = this.getAttributeNames();
	const propKeys = Object.keys(props);
	const extraProps = thisProps.filter(p => !propKeys.includes(p));
	if (extraProps.length > 0) {
		throw new Error(`.exactProps() target has extra properties: ${extraProps.join(', ')}`);
	}
	const missingProps = propKeys.filter(p => !thisProps.includes(p));
	if (missingProps.length > 0) {
		throw new Error(`.exactProps() target is missing properties: ${missingProps.join(', ')}`);
	}
	for (const key of propKeys) {
		const thisValue = this.getAttribute(key);
		const oValue = valueToString(props[key]);
		try {
			thisValue.is(oValue);
		} catch (e) {
			const e2 = new Error(`.exactProps() property "${key}" mismatch`);
			e2.cause = e;
			throw e2;
		}
	}
}

Element.prototype.text = function() {
	return this.textContent;
}
