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

