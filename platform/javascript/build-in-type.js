var yume = {};

// ***************** boolean *******************
yume._boolean = function(bool) {
	if (typeof bool !== "boolean") {
		throw "Boolean constructor: " + name;
	}
	this.v = bool;
};

yume._boolean.prototype = {
	__type__: "boolean",
	eqv: function(that) {
		return yume.is_boolean(that) && this.v === that.v;
	}
};

yume._boolean._true = new yume._boolean(true);
yume._boolean._false = new yume._boolean(false);

yume.is_boolean = function(p) {
	return typeof p === "object" && yume._boolean.prototype.isPrototypeOf(p);
};

// ****************** symbol ******************
yume._symbol = function(name) {
	if (typeof name !== "string") {
		throw "Symbol constructor: " + name;
	}
	this.name = name;
};

yume._symbol.prototype = {
	__type__: "symbol",
	eqv: function(that) {
		return yume.is_symbol(that) && this.name === that.name;
	},
	get_name: function() {
		return this.name;
	}
};

yume.is_symbol = function(p) {
	return typeof p === "object" && yume._symbol.prototype.isPrototypeOf(p);
};

// ******************* char ********************
yume._char = function(c) {
	if (typeof c !== "number") {
		throw "Char constructor: " + c;
	}
	this.v = c;
};

yume._char.prototype = {
	__type__: "char",
	eqv: function(that) {
		return yume.is_char(that) && this.v === that.v;
	},
	get_value: function() {
		return this.v;
	}
};

yume.is_char = function(p) {
	return typeof p === "object" && yume._char.prototype.isPrototypeOf(p);
};

// ****************** vector *******************
yume._vector = function() {
};

yume._vector.prototype = {
	__type__: "vector",
	eqv: function(that) {
		return this === that;
	}
};

yume.is_vector = function(p) {
	return typeof p === "object" && yume._vector.prototype.isPrototypeOf(p);
};

// ***************** procedure *****************
yume._procedure = function(fun, scope, n_args, is_vargs) {
	if (typeof fun !== "function") {
		throw "Procedure constructor: " + c;
	}
	if (! (yume.is_null_list(scope) || yume.is_list(scope))) {
		throw "Procedure constructor: " + scope;
	}
	if (typeof n_args !== "number") {
		throw "Procedure constructor: " + n_args;
	}
	if (typeof is_vargs !== "boolean") {
		throw "Procedure constructor: " + is_vargs;
	}

	this.fun = fun;
	this.n_args = n_args;
	this.is_vargs = is_vargs;
	this.scope = scope;
};

yume._procedure.prototype = {
	__type__: "procedure",
	eqv: function(that) {
		return yume.is_procedure(that) && this.fun === that.fun && this.n_args === that.n_args && this.is_vargs === that.is_vargs && this.scope === that.scope;
	},
	_call: function(cps, args) {
		if (this.is_vargs) {
			if (yume.length(args) < this.n_args) {
				throw "runtime-error: Procedure call " + this + " require at lease " + this.n_args + " provide " + yume.length(args) + " " + args;
			}
		} else {
			if (yume.length(args) !== this.n_args) {
				throw "runtime-error: Procedure call " + this + " require " + this.n_args + " provide " + yume.length(args) + " " + args;
			}
		}

		return this.fun(cps, yume.cons(args, this.scope));
	}
};

yume.is_procedure = function(p) {
	return typeof p === "object" && yume._procedure.prototype.isPrototypeOf(p);
};

yume.procedure_call = function(procedure, cps, args) {
	if (!yume.is_procedure(procedure)) {
		throw "Procedure call: " + procedure + " is not a procedure";
	}
	return procedure._call(cps, args);
};

// ***************** continue ******************
yume._continue = function(fun, cps, scope) {
	if (typeof fun !== "function") {
		throw "Procedure constructor: " + c;
	}
	if (typeof cps !== "object" || ! yume.is_continue(cps)) {
		if (cps !== null) { // cps can be null or continue
			throw "Procedure constructor: " + is_vargs;
		}
	}
	// we do not check scope because it may be borrowed to pass anything
	this.fun = fun;
	this.cps = cps;
	this.scope = scope;
};

yume._continue.prototype = {
	__type__: "continue",
	_call: function(result) {
		return this.fun(this.cps, this.scope, result);
	}
};

yume.is_continue = function(p) {
	return typeof p === "object" && yume._continue.prototype.isPrototypeOf(p);
};

yume.continue_run = function(r) {
	var cps = r.cps;
	if (!yume.is_continue(cps)) {
		throw "internal-runtime-error: Continue call: " + cps + " is not a continue";
	}
	return cps._call(r.result);
};

yume.continue_call = function(cps, result) {
	if (!yume.is_continue(cps)) {
		throw "internal-runtime-error: Continue call: " + cps + " is not a continue";
	}
	return {
		cps: cps,
		result: result
	};
};

yume.continue_apply = function(cps, result) {
	if (!yume.is_continue(cps)) {
		throw "internal-runtime-error: Continue call: " + cps + " is not a continue";
	}
	return {
		cps: cps,
		result: new yume._values(result)
	};
};

yume.continue_to_procedure = function(cont) {
	return new yume._procedure(function (cps, scope) {
		if (yume.is_pair(scope.car()) && yume.is_null_list(scope.car().cdr())) {
			return yume.continue_call(cont, scope.car().car());
		} else {
			return yume.continue_apply(cont, scope.car());
		}
	}, yume._null_list, 0, true);
};

// ******************* pair ********************
yume._pair = function(a, d) {
	this.a = a;
	this.d = d;
};

yume._pair.prototype = {
	__type__: "pair",
	eqv: function(that) {
		return this === that;
	},
	car: function() {
		return this.a;
	},
	cdr: function() {
		return this.d;
	},
	set_car: function(v) {
		this.a = v;
	},
	set_cdr: function(v) {
		this.d = v;
	}
};

yume.is_pair = function(p) {
	return typeof p === "object" && yume._pair.prototype.isPrototypeOf(p);
};

// ***************** null_list *****************
yume._null_list = {
	__type__: "null_list",
	eqv: function(that) {
		return yume.is_null_list(that);
	}
};

yume.is_null_list = function(p) {
	return typeof p === "object" && p === yume._null_list;
};

// ****************** number *******************
yume._number = function(n) {
	if (typeof n !== "number") {
		throw "Number constructor: " + name;
	}
	this.v = n;
};

yume._number.prototype = {
	__type__: "number",
	eqv: function(that) {
		return yume.is_number(that) && this.v === that.v;
	},
	get_value: function() {
		return this.v;
	}
};

yume.is_number = function(p) {
	return typeof p === "object" && yume._number.prototype.isPrototypeOf(p);
};

// ****************** string *******************
yume._string = function(s) {
	if (typeof s !== "string") {
		throw "String constructor: " + s;
	}
	this.v = s;
};

yume._string.prototype = {
	__type__: "string",
	eqv: function(that) {
		return yume.is_string(that) && this.v === that.v;
	},
	get_value: function() {
		return this.v;
	},
	set_value: function(s) {
		if (typeof s !== "string") {
			throw "String set: " + s;
		}
		this.v = s;
	}
};

yume.is_string = function(p) {
	return typeof p === "object" && yume._string.prototype.isPrototypeOf(p);
};

// ******************* port ********************
yume._eof = {
	__type__: "eof",
	eqv: function(that) {
		return yume.is_eof(that);
	}
};

yume.is_eof = function(p) {
	return typeof p === "object" && p === yume._eof;
};

yume._input = function(input) {
	// accept any input who support read/peed/ready/close
	if (typeof input.read !== "function" || typeof input.peek !== "function" || typeof input.ready !== "function" || typeof input.close !== "function") {
		throw "Input constructor: " + input;
	}
	this.i = input;
};

yume._input.prototype = {
	__type__: "input",
	eqv: function(that) {
		return yume.is_input(that) && this.i === that.i;
	},
	read: function() {
		return this.i.read();
	},
	peek: function() {
		return this.i.peek();
	},
	ready: function() {
		return this.i.ready();
	},
	close: function() {
		return this.i.close();
	}
};

yume.is_input = function(p) {
	return typeof p === "object" && yume._input.prototype.isPrototypeOf(p);
};

yume._output = function(output) {
	// accept any output who support write
	if (typeof output.write !== "function" || typeof output.close !== "function") {
		throw "Output constructor: " + output;
	}
	this.o = output;
};

yume._output.prototype = {
	__type__: "output",
	eqv: function(that) {
		return yume.is_output(that) && this.o === that.o;
	},
	write: function(c) {
		return this.o.write(c);
	},
	write_string: function(s) {
		return this.o.write_string(s);
	},
	close: function() {
		return this.i.close();
	}
};

yume.is_output = function(p) {
	return typeof p === "object" && yume._output.prototype.isPrototypeOf(p);
};

// ****************** global *******************
(function() {
	var global_table = {}; // create a closure to make global_table private
	yume.global_add = function(name, value) {
		if (value === undefined) {
			throw "internal-runtime-error: global_add undefined to global " + name;
		}
		global_table[name] = value;
	};
	yume.global_set = function(name, value) {
		if (value === undefined) {
			throw "internal-runtime-error:  global_set undefined to global " + name;
		}
		if (global_table[name] === undefined) {
			throw "runtime-error: unbound value: " + name;
		}
		global_table[name] = value;
	};
	yume.global_get = function(name) {
		if (typeof name !== "string") {
			throw "internal-runtime-error: global_get " + name + "is not string";
		}
		if (global_table[name] === undefined) {
			throw "runtime-error: unbound value: " + name;
		}
		return global_table[name];
	};
})();

// ****************** values *******************
yume._values = function(list) {
	this.l = list;
};

yume._values.prototype = {
	__type__: "values",
	get_value: function() {
		return this.l;
	}
};

yume.is_values = function(p) {
	return typeof p === "object" && yume._values.prototype.isPrototypeOf(p);
};

// ****************** record *******************
yume._record = function(length) {
	if (typeof length !== "number") {
		throw "Record constructor: " + length;
	}
	this.obj = [];
	this.obj.length = length;
};

yume._record.prototype = {
	__type__: "record",
	eqv: function(that) {
		return this === that;
	},
	get: function(index) {
		if (typeof index !== "number") {
			throw "Record get: " + index;
		}
		if (index >= 0 && index < this.obj.length) {
			return this.obj[index];
		} else {
			throw "Record out of index";
		}
	},
	set: function(index, value) {
		if (typeof index !== "number") {
			throw "Record get: " + index;
		}
		if (index >= 0 && index < this.obj.length) {
			this.obj[index] = value;
		} else {
			throw "Record out of index";
		}
	}
};

yume.is_record = function(p) {
	return typeof p === "object" && yume._record.prototype.isPrototypeOf(p);
};

// **************** list helper ****************
yume.cons = function(head, tail) {
	return new yume._pair(head, tail);
};

yume.car = function(p) {
	return p.car();
};

yume.cdr = function(p) {
	return p.cdr();
};

yume.set_car_bang = function(p, v) {
	p.set_car(v);
};

yume.set_cdr_bang = function(p, v) {
	p.set_cdr(v);
};

yume.is_list = function(p) {
	while (yume.is_pair(p)) {
		p = p.cdr();
	}
	return yume.is_null_list(p);
};

yume.length = function(p) {
	var i = 0;
	while (yume.is_pair(p)) {++i;
		p = p.cdr();
	}
	return i;
};

yume.test = function(p) {
	return (p !== yume._boolean._false);
};

