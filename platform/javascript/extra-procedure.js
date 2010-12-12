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

