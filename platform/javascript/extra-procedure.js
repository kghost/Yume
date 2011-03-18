yume.global_add("debug", new yume._procedure(function(cps, scope) {
	throw "not-implement";
	/*return { cps: cps, result: "not-implement" };*/
},
yume._null_list, 0, true));

yume.global_add("escape", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var s = frame.car();
	if (!yume.is_string(s)) {
		throw "runtime-error: " + s + "is not string";
	}
	return {
		cps: cps,
		result: new yume._string(s.get_value().replace(/\\/g, "\\\\").replace(/\"/g, "\\\""))
	};
},
yume._null_list, 1, false));

(function() {
	var sym = 0;
	yume.global_add("gensym", new yume._procedure(function(cps, scope) {
		var frame = scope.car();
		var s = frame.car();
		if (!yume.is_string(s)) {
			throw "runtime-error: " + s + "is not string";
		}
		return {
			cps: cps,
			result: new yume._symbol(s.get_value() + sym++)
		};
	},
	yume._null_list, 1, false));
})();

yume.global_add("write-string/partial", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var s = frame.car();
	var o = frame.cdr().car();
	if (!yume.is_string(s)) {
		throw "runtime-error: " + s + "is not string";
	}
	if (!yume.is_output(o)) {
		throw "runtime-error: " + o + "is not output";
	}
	o.write_string(s.get_value());
	return {
		cps: cps,
		result: undefined
	};
},
yume._null_list, 2, false));

yume.global_add("newline", new yume._procedure(function(cps, scope) {
	var frame = scope.car();
	var o = frame.car();
	if (!yume.is_output(o)) {
		throw "runtime-error: " + o + "is not output";
	}
	o.write_string("\n");
	return {
		cps: cps,
		result: undefined
	};
},
yume._null_list, 1, false));

