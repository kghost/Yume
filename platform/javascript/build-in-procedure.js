var js_bool_to_scheme_bool = function(b) {
	return b ? yume._boolean._true: yume._boolean._false;
};

// ********** Equivalence predicates ***********
yume.global_add("eqv?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var me = frame.car();
	var you = frame.cdr().car();
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(me.eqv(you))
	};

},
yume._null_list, 2, false));

yume.global_add("eq?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var me = frame.car();
	var you = frame.cdr().car();
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(me.eqv(you))
	};
},
yume._null_list, 2, false));

// *********** Numerical operations ************
yume.global_add("number?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var n = frame.car();
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(yume.is_number(n))
	};
},
yume._null_list, 1, false));

yume.global_add("complex?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var n = frame.car();
	if (!yume.is_number(n)) {
		throw "runtime-error: " + i + " is not number";
	}
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(false)
	};
},
yume._null_list, 1, false));

yume.global_add("real?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var n = frame.car();
	if (!yume.is_number(n)) {
		throw "runtime-error: " + i + " is not number";
	}
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(true)
	};
},
yume._null_list, 1, false));

yume.global_add("rational?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var n = frame.car();
	if (!yume.is_number(n)) {
		throw "runtime-error: " + i + " is not number";
	}
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(false)
	};
},
yume._null_list, 1, false));

yume.global_add("integer?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var n = frame.car();
	if (!yume.is_number(n)) {
		throw "runtime-error: " + i + " is not number";
	}
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(n.get_value() % 1 === 0)
	};
},
yume._null_list, 1, false));

yume.global_add("exact?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var n = frame.car();
	if (!yume.is_number(n)) {
		throw "runtime-error: " + i + " is not number";
	}
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(false)
	};
},
yume._null_list, 1, false));

yume.global_add("inexact?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var n = frame.car();
	if (!yume.is_number(n)) {
		throw "runtime-error: " + i + " is not number";
	}
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(true)
	};
},
yume._null_list, 1, false));

yume.global_add("=", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var a = frame.car();
	var b = frame.cdr().car();
	if (!yume.is_number(a)) {
		throw "runtime-error: " + a + " is not number";
	}
	if (!yume.is_number(b)) {
		throw "runtime-error: " + b + " is not number";
	}
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(a.get_value() === b.get_value())
	};
},
yume._null_list, 2, false));

yume.global_add("<", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var a = frame.car();
	var b = frame.cdr().car();
	if (!yume.is_number(a)) {
		throw "runtime-error: " + a + " is not number";
	}
	if (!yume.is_number(b)) {
		throw "runtime-error: " + b + " is not number";
	}
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(a.get_value() < b.get_value())
	};
},
yume._null_list, 2, false));

yume.global_add(">", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var a = frame.car();
	var b = frame.cdr().car();
	if (!yume.is_number(a)) {
		throw "runtime-error: " + a + " is not number";
	}
	if (!yume.is_number(b)) {
		throw "runtime-error: " + b + " is not number";
	}
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(a.get_value() > b.get_value())
	};
},
yume._null_list, 2, false));

yume.global_add("<=", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var a = frame.car();
	var b = frame.cdr().car();
	if (!yume.is_number(a)) {
		throw "runtime-error: " + a + " is not number";
	}
	if (!yume.is_number(b)) {
		throw "runtime-error: " + b + " is not number";
	}
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(a.get_value() <= b.get_value())
	};
},
yume._null_list, 2, false));

yume.global_add(">=", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var a = frame.car();
	var b = frame.cdr().car();
	if (!yume.is_number(a)) {
		throw "runtime-error: " + a + " is not number";
	}
	if (!yume.is_number(b)) {
		throw "runtime-error: " + b + " is not number";
	}
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(a.get_value() >= b.get_value())
	};
},
yume._null_list, 2, false));

yume.global_add("+", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var s = 0;
	for (; yume.is_pair(frame); frame = frame.cdr()) {
		i = frame.car();
		if (!yume.is_number(i)) {
			throw "runtime-error: " + i + " is not number";
		}
		s += i.get_value();
	}
	if (!yume.is_null_list(frame)) {
		throw "runtime-error: " + scope.car() + " is not proper-list";
	}
	return {
		cps: cps,
		result: new yume._number(s)
	};
},
yume._null_list, 0, true));

yume.global_add("*", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var s = 1;
	for (; yume.is_pair(frame); frame = frame.cdr()) {
		i = frame.car();
		if (!yume.is_number(i)) {
			throw "runtime-error: " + i + " is not number";
		}
		s *= i.get_value();
	}
	if (!yume.is_null_list(frame)) {
		throw "runtime-error: " + scope.car() + " is not proper-list";
	}
	return {
		cps: cps,
		result: new yume._number(s)
	};
},
yume._null_list, 0, true));

yume.global_add("-", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var f = frame.car();
	if (!yume.is_number(f)) {
		throw "runtime-error: " + f + " is not number";
	}
	var s = f.get_value();
	frame = frame.cdr();
	if (yume.is_null_list(frame)) {
		s = - s;
	} else {
		for (; yume.is_pair(frame); frame = frame.cdr()) {
			i = frame.car();
			if (!yume.is_number(i)) {
				throw "runtime-error: " + i + " is not number";
			}
			s -= i.get_value();
		}
		if (!yume.is_null_list(frame)) {
			throw "runtime-error: " + scope.car() + " is not proper-list";
		}
	}
	return {
		cps: cps,
		result: new yume._number(s)
	};
},
yume._null_list, 1, true));

yume.global_add("/", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var f = frame.car();
	if (!yume.is_number(f)) {
		throw "runtime-error: " + f + " is not number";
	}
	var s = f.get_value();
	frame = frame.cdr();
	if (yume.is_null_list(frame)) {
		s = 1.0 / s;
	} else {
		for (; yume.is_pair(frame); frame = frame.cdr()) {
			i = frame.car();
			if (!yume.is_number(i)) {
				throw "runtime-error: " + i + " is not number";
			}
			s /= i.get_value();
		}
		if (!yume.is_null_list(frame)) {
			throw "runtime-error: " + scope.car() + " is not proper-list";
		}
	}
	return {
		cps: cps,
		result: new yume._number(s)
	};
},
yume._null_list, 0, true));

yume.global_add("quotient", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("remainder", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("modulo", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("numerator", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("denominator", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("floor", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("ceiling", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("truncate", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("round", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("exp", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("log", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("sin", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("cos", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("tan", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("asin", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("acos", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("atan", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("atan", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("sqrt", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("expt", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("make-rectangular", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("make-polar", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("real-part", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("imag-part", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("magnitude", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("angle", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("exact->inexact", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("inexact->exact", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

// ******** Numerical input and output *********
yume.global_add("number->string", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var n = frame.car();
	if (!yume.is_number(n)) {
		throw "runtime-error: " + n + "is not number";
	}
	return {
		cps: cps,
		result: new yume._string("" + n.get_value())
	};
},
yume._null_list, 1, false));

yume.global_add("string->number", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var s = frame.car();
	if (!yume.is_string(s)) {
		throw "runtime-error: " + s + "is not string";
	}
	return {
		cps: cps,
		result: new yume._number(parseInt(s.get_value(), 10))
	};
},
yume._null_list, 1, false));

// ************** Pairs and lists **************
yume.global_add("pair?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(yume.is_pair(p))
	};
},
yume._null_list, 1, false));

yume.global_add("cons", new yume._procedure(function(cps, scope) {
	// arguments syntax is already checked outside
	var frame = scope.car();
	var a = frame.car();
	var d = frame.cdr().car();
	return {
		cps: cps,
		result: yume.cons(a, d)
	};
},
yume._null_list, 2, false));

yume.global_add("car", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	if (!yume.is_pair(p)) {
		throw "runtime-error: " + p + " is not pair";
	}
	return {
		cps: cps,
		result: p.car()
	};
},
yume._null_list, 1, false));

yume.global_add("cdr", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	if (!yume.is_pair(p)) {
		throw "runtime-error: " + p + " is not pair";
	}
	return {
		cps: cps,
		result: p.cdr()
	};
},
yume._null_list, 1, false));

yume.global_add("set-car!", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	var v = frame.cdr().car();
	if (!yume.is_pair(p)) {
		throw "runtime-error: " + p + " is not pair";
	}
	return {
		cps: cps,
		result: p.set_car(v)
	};
},
yume._null_list, 2, false));

yume.global_add("set-cdr!", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	var v = frame.cdr().car();
	if (!yume.is_pair(p)) {
		throw "runtime-error: " + p + " is not pair";
	}
	return {
		cps: cps,
		result: p.set_cdr(v)
	};
},
yume._null_list, 2, false));

// ****************** Symbols ******************
yume.global_add("symbol?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(yume.is_symbol(p))
	};
},
yume._null_list, 1, false));

yume.global_add("symbol->string", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	if (!yume.is_symbol(p)) {
		throw "runtime-error: " + p + " is not symbol";
	}
	return {
		cps: cps,
		result: new yume._string(p.get_name())
	};
},
yume._null_list, 1, false));

yume.global_add("string->symbol", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	if (!yume.is_string(p)) {
		throw "runtime-error: " + p + " is not string";
	}
	return {
		cps: cps,
		result: new yume._symbol(p.get_value())
	};
},
yume._null_list, 1, false));

// **************** Characters *****************
yume.global_add("char?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var c = frame.car();
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(yume.is_char(c))
	};
},
yume._null_list, 1, false));

yume.global_add("char=?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var c1 = frame.car();
	var c2 = frame.cdr().car();
	if (!yume.is_char(c1)) {
		throw "runtime-error: " + c1 + " is not char";
	}
	if (!yume.is_char(c2)) {
		throw "runtime-error: " + c2 + " is not char";
	}
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(c1.get_value() === c2.get_value())
	};
},
yume._null_list, 2, false));

yume.global_add("char<?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var c1 = frame.car();
	var c2 = frame.cdr().car();
	if (!yume.is_char(c1)) {
		throw "runtime-error: " + c1 + " is not char";
	}
	if (!yume.is_char(c2)) {
		throw "runtime-error: " + c2 + " is not char";
	}
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(c1.get_value() < c2.get_value())
	};
},
yume._null_list, 2, false));

yume.global_add("char>?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var c1 = frame.car();
	var c2 = frame.cdr().car();
	if (!yume.is_char(c1)) {
		throw "runtime-error: " + c1 + " is not char";
	}
	if (!yume.is_char(c2)) {
		throw "runtime-error: " + c2 + " is not char";
	}
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(c1.get_value() > c2.get_value())
	};
},
yume._null_list, 2, false));

yume.global_add("char<=?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var c1 = frame.car();
	var c2 = frame.cdr().car();
	if (!yume.is_char(c1)) {
		throw "runtime-error: " + c1 + " is not char";
	}
	if (!yume.is_char(c2)) {
		throw "runtime-error: " + c2 + " is not char";
	}
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(c1.get_value() <= c2.get_value())
	};
},
yume._null_list, 2, false));

yume.global_add("char>=?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var c1 = frame.car();
	var c2 = frame.cdr().car();
	if (!yume.is_char(c1)) {
		throw "runtime-error: " + c1 + " is not char";
	}
	if (!yume.is_char(c2)) {
		throw "runtime-error: " + c2 + " is not char";
	}
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(c1.get_value() >= c2.get_value())
	};
},
yume._null_list, 2, false));

yume.global_add("char->integer", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var c = frame.car();
	if (!yume.is_char(c)) {
		throw "runtime-error: " + c + " is not char";
	}
	return {
		cps: cps,
		result: new yume._number(c.get_value())
	};
},
yume._null_list, 1, false));

yume.global_add("integer->char", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var c = frame.car();
	if (!yume.is_null_list(c)) {
		throw "runtime-error: " + c + " is not number";
	}
	return {
		cps: cps,
		result: new yume._char(c.get_value())
	};
},
yume._null_list, 1, false));

// ****************** Strings ******************
yume.global_add("string?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var s = frame.car();
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(yume.is_string(s))
	};
},
yume._null_list, 1, false));

yume.global_add("make-string", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var n = frame.car();
	if (!yume.is_number(n)) {
		throw "runtime-error: " + n + "is not number";
	}
	var c;
	if (yume.is_pair(frame.cdr())) {
		var ch = frame.cdr().car();
		if (!yume.is_char(ch)) {
			throw "runtime-error: " + ch + "is not char";
		}
		c = String.fromCharCode(ch.get_value());
		if (!yume.is_null_list(frame.cdr().cdr())) {
			throw "runtime-error: " + frame + " extra arguments";
		}
	} else {
		c = " ";
	}
	return {
		cps: cps,
		result: new yume._string(new Array(n.get_value() + 1).join(c))
	};
},
yume._null_list, 1, true));

yume.global_add("string-length", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var s = frame.car();
	if (!yume.is_string(s)) {
		throw "runtime-error: " + s + "is not string";
	}
	return {
		cps: cps,
		result: new yume._number(s.get_value().length)
	};
},
yume._null_list, 1, false));

yume.global_add("string-ref", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var s = frame.car();
	if (!yume.is_string(s)) {
		throw "runtime-error: " + s + " is not string";
	}
	var i = frame.cdr().car();
	if (!yume.is_number(i)) {
		throw "runtime-error: " + i + " is not number";
	}
	var sv = s.get_value();
	var iv = i.get_value();
	if (iv < 0 || iv >= sv.length) {
		throw "runtime-error: " + iv + " out of index " + sv;
	}
	return {
		cps: cps,
		result: new yume._char(sv.charCodeAt(iv))
	};
},
yume._null_list, 2, false));

yume.global_add("string-set!", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var s = frame.car();
	var i = frame.cdr().car();
	var c = frame.cdr().cdr().car();
	if (!yume.is_string(s)) {
		throw "runtime-error: " + s + "is not string";
	}
	if (!yume.is_number(i)) {
		throw "runtime-error: " + i + "is not number";
	}
	if (!yume.is_char(c)) {
		throw "runtime-error: " + c + "is not char";
	}
	var index = i.get_value();
	var v = s.get_value();
	if (index >= v.length) {
		throw "runtime-error: " + i + " out of bound";
	}
	s.set_value(v.substr(0, index) + String.fromCharCode(c.get_value()) + v.substr(index + 1, v.length));
	return {
		cps: cps,
		result: undefined
	};
},
yume._null_list, 3, false));

yume.global_add("string-append", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var h = frame.car();
	if (!yume.is_string(h)) {
		throw "runtime-error: " + h + " is not string";
	}
	var r = h.get_value();
	for (var t = frame.cdr();
	(!yume.is_null_list(t));
	t = t.cdr()) {
		if (!yume.is_pair(t)) {
			throw "runtime-error: " + frame + " is not propper list";
		}
		var s = t.car();
		if (!yume.is_string(s)) {
			throw "runtime-error: " + s + " is not string";
		}
		r += s.get_value();
	}
	return {
		cps: cps,
		result: new yume._string(r)
	};
},
yume._null_list, 1, true));

yume.global_add("list->string", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var s = "";
	for (var l = frame.car(); ! yume.is_null_list(l);
	l = l.cdr()) {
		if (!yume.is_pair(l)) {
			throw "runtime-error: " + l + " is not propper-list";
		}
		var c = l.car();
		if (!yume.is_char(c)) {
			throw "runtime-error: " + c + " is not char";
		}
		s += String.fromCharCode(c.get_value());
	}
	return {
		cps: cps,
		result: new yume._string(s)
	};
},
yume._null_list, 1, false));

yume.global_add("string->list", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var str = frame.car();
	if (!yume.is_string(str)) {
		throw "runtime-error: " + str + " is not string";
	}
	var s = str.get_value();
	var l = yume._null_list;
	for (var i = s.length - 1; i >= 0; --i) {
		l = yume.cons(new yume._char(s.charCodeAt(i)), l);
	}
	return {
		cps: cps,
		result: l
	};
},
yume._null_list, 1, false));

// ***************** Vectors *******************
yume.global_add("vector?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(yume.is_vector(p))
	};
},
yume._null_list, 1, false));

// ***************** Records *******************
yume.global_add("record?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(yume.is_record(p))
	};
},
yume._null_list, 1, false));

yume.global_add("make-record", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	if (!yume.is_number(p)) {
		throw "runtime-error: " + p + " is not number";
	}
	return {
		cps: cps,
		result: new yume._record(p.get_value())
	};
},
yume._null_list, 1, false));

yume.global_add("record-ref", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	var i = frame.cdr().car();
	if (!yume.is_record(p)) {
		throw "runtime-error: " + p + " is not record";
	}
	if (!yume.is_number(i)) {
		throw "runtime-error: " + p + " is not number";
	}
	return {
		cps: cps,
		result: p.get(i.get_value())
	};
},
yume._null_list, 2, false));

yume.global_add("record-set!", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	var i = frame.cdr().car();
	var v = frame.cdr().cdr().car();
	if (!yume.is_record(p)) {
		throw "runtime-error: " + p + " is not record";
	}
	if (!yume.is_number(i)) {
		throw "runtime-error: " + p + " is not number";
	}
	p.set(i.get_value(), v);
	return {
		cps: cps,
		result: undefined
	};
},
yume._null_list, 3, false));

// ************* Control features **************
yume.global_add("procedure?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(yume.is_procedure(p))
	};
},
yume._null_list, 1, false));

yume.global_add("apply", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	if (!yume.is_procedure(p)) {
		throw "runtime-error: " + p + " is not procedure";
	}
	var llast = frame;
	var last = frame.cdr();
	while (!yume.is_null_list(last.cdr())) {
		llast = last;
		last = last.cdr();
	}
	llast.set_cdr(last.car());
	return yume.procedure_call(p, cps, frame.cdr());
},
yume._null_list, 2, true));

yume.global_add("call-with-current-continuation", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	if (!yume.is_procedure(p)) {
		throw "runtime-error: " + p + " is not procedure";
	}
	return yume.procedure_call(p, cps, yume.cons(yume.continue_to_procedure(cps), yume._null_list));
},
yume._null_list, 1, false));

yume.global_add("values", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var r;
	if (yume.is_pair(frame) && yume.is_null_list(frame.cdr())) {
		r = frame.car();
	} else {
		r = new yume._values(frame);
	}
	return {
		cps: cps,
		result: r
	};
},
yume._null_list, 0, true));

yume.global_add("call-with-values", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var producer = frame.car();
	var consumer = frame.cdr().car();
	if (!yume.is_procedure(producer)) {
		throw "runtime-error: " + producer + " is not procedure";
	}
	if (!yume.is_procedure(consumer)) {
		throw "runtime-error: " + consumer + " is not procedure";
	}
	return yume.procedure_call(producer, new yume._continue(function(cps, scope, result) {
		var args;
		if (yume.is_values(result)) {
			args = result.get_value();
		} else {
			args = yume.cons(result, yume._null_list);
		}
		return yume.procedure_call(consumer, cps, args);
	},
	cps), yume._null_list);
},
yume._null_list, 2, true));

yume.global_add("dynamic-wind", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

// ******************* Eval ********************
yume.global_add("eval", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

// ************* Input and output **************
// ****************** Ports ********************
yume.global_add("input-port?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(yume.is_input(p))
	};
},
yume._null_list, 1, false));

yume.global_add("output-port?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(yume.is_output(p))
	};
},
yume._null_list, 1, false));

yume.global_add("current-input-port", new yume._procedure(function(cps, scope) {
	return {
		cps: cps,
		result: yume.current_input_port()
	};
},
yume._null_list, 0, false));

yume.global_add("current-output-port", new yume._procedure(function(cps, scope) {
	return {
		cps: cps,
		result: yume.current_output_port()
	};
},
yume._null_list, 0, false));

yume.global_add("open-input-file", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var s = frame.car();
	if (!yume.is_string(s)) {
		throw "runtime-error: " + s + " is not string";
	}
	return yume.open_input_file(cps, s.get_value());
},
yume._null_list, 1, false));

yume.global_add("open-output-file", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var s = frame.car();
	if (!yume.is_string(s)) {
		throw "runtime-error: " + s + " is not string";
	}
	return yume.open_output_file(cps, s.get_value());
},
yume._null_list, 1, false));

yume.global_add("close-input-port", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	if (!yume.is_input(p)) {
		throw "runtime-error: " + p + " is not input port";
	}
	p.close();
	return {
		cps: cps,
		result: undefined
	};
},
yume._null_list, 1, false));

yume.global_add("close-output-port", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	if (!yume.is_output(p)) {
		throw "runtime-error: " + s + " is not output port";
	}
	p.close();
	return {
		cps: cps,
		result: undefined
	};
},
yume._null_list, 1, false));

// ******************* Input *******************
yume.global_add("read-char", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p;
	if (yume.is_pair(frame)) {
		p = frame.car();
		if (!yume.is_input(p)) {
			throw "runtime-error: " + p + " is not input port";
		}
		if (!yume.is_null_list(frame.cdr())) {
			throw "runtime-error: read-char " + frame + " extra arguments";
		}
	} else {
		p = yume.current_input_port();
	}
	var result = p.read();
	if (! (yume.is_char(result) || yume.is_eof(result))) {
		throw "internal-runtime-error: read-char return " + result;
	}
	return {
		cps: cps,
		result: result
	};
},
yume._null_list, 0, true));

yume.global_add("peek-char", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p;
	if (yume.is_pair(frame)) {
		p = frame.car();
		if (!yume.is_input(p)) {
			throw "runtime-error: " + p + " is not input port";
		}
		if (!yume.is_null_list(frame.cdr())) {
			throw "runtime-error: peek-char " + frame + " extra arguments";
		}
	} else {
		p = yume.current_input_port();
	}
	var result = p.peek();
	if (! (yume.is_char(result) || yume.is_eof(result))) {
		throw "internal-runtime-error: peek-char return " + result;
	}
	return {
		cps: cps,
		result: result
	};
},
yume._null_list, 0, true));

yume.global_add("eof-object?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p = frame.car();
	return {
		cps: cps,
		result: js_bool_to_scheme_bool(yume.is_eof(p))
	};
},
yume._null_list, 1, false));

yume.global_add("char-ready?", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var p;
	if (yume.is_pair(frame)) {
		p = frame.car();
		if (!yume.is_input(p)) {
			throw "runtime-error: " + p + " is not input port";
		}
		if (!yume.is_null_list(frame.cdr())) {
			throw "runtime-error: char-ready? " + frame + " extra arguments";
		}
	} else {
		p = yume.current_input_port();
	}
	var result = p.ready();
	if (!yume.is_boolean(result)) {
		throw "internal-runtime-error: char-ready? return " + result;
	}
	return {
		cps: cps,
		result: result
	};
},
yume._null_list, 0, true));

// ****************** Output *******************
yume.global_add("write-char", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var c = frame.car();
	if (!yume.is_char(c)) {
		throw "runtime-error: " + c + " is not char";
	}
	var p;
	if (yume.is_pair(frame.cdr())) {
		p = frame.cdr().car();
		if (!yume.is_output(p)) {
			throw "runtime-error: " + p + " is not output port";
		}
		if (!yume.is_null_list(frame.cdr().cdr())) {
			throw "runtime-error: write-char " + frame + " extra arguments";
		}
	} else {
		p = yume.current_output_port();
	}
	return {
		cps: cps,
		result: p.write(c.get_value())
	};
},
yume._null_list, 1, true));

yume.global_add("write-string", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var s = frame.car();
	if (!yume.is_string(s)) {
		throw "runtime-error: " + s + " is not string";
	}
	var p;
	if (yume.is_pair(frame.cdr())) {
		p = frame.cdr().car();
		if (!yume.is_output(p)) {
			throw "runtime-error: " + p + " is not output port";
		}
		if (!yume.is_null_list(frame.cdr().cdr())) {
			throw "runtime-error: write-string" + frame + " extra arguments";
		}
	} else {
		p = yume.current_output_port();
	}
	return {
		cps: cps,
		result: p.write_string(s.get_value())
	};
},
yume._null_list, 1, true));

// ************* System interface **************
yume.global_add("load", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var s = frame.car();
	if (!yume.is_string(s)) {
		throw "runtime-error: " + s + " is not string";
	}
	return yume.load(cps, s.get_value());
},
yume._null_list, 1, false));

yume.global_add("transcript-on", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("transcript-off", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

