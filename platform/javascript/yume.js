var dummy = function() {};

jQuery(window).load(function() {
	var run = function(r) {
		while (r !== undefined) {
			r = yume.continue_run(r.cps, r.result);
		}
	};

	yume.load = function(cps, url) {
		jQuery.get("gen/" + url, function(data) {
			var module = eval(data);
			run(module(cps, yume._null_list));
		});
		return undefined; // interrupt schme engine
	};

	var input = function(s) {
		this.closed = false;
		this.index = 0;
		this.s = s;
	};

	input.prototype = {
		read: function() {
			if (this.closed) {
				throw "runtime-error: port closed";
			}
			if (this.index >= this.s.length) {
				return yume._eof;
			} else {
				return new yume._char(s.charCodeAt(index++));
			}
		},
		peek: function() {
			if (this.closed) {
				throw "runtime-error: port closed";
			}
			if (this.index >= this.s.length) {
				return yume._eof;
			} else {
				return new yume._char(s.charCodeAt(index));
			}
		},
		ready: function() {
			return yume._boolean._true;
		},
		close: function() {
			this.closed = true;
			return undefined;
		}
	};

	var output = function(flush) {
		this.closed = false;
		this.flush = flush;
		this.buffer = "";
	};

	output.prototype = {
		write: function(c) {
			if (this.closed) {
				throw "runtime-error: port closed";
			}
			if (buffer.length < 4095) {
				buffer += String.fromCharCode(c);
			} else {
				this.flush(buffer);
				buffer = String.fromCharCode(c);
			}
			return undefined;
		},
		write_string: function(s) {
			if (this.closed) {
				throw "runtime-error: port closed";
			}
			if (buffer.length + s.length < 4096) {
				buffer += s;
			} else {
				this.flush(buffer);
				buffer = s;
			}
			return undefined;
		},
		close: function() {
			this.closed = true;
			return undefined;
		}
	};

	var current_input;
	var current_output;

	yume.current_input_port = function() {
		return current_input;
	};

	yume.current_output_port = function() {
		return current_output;
	};

	var application = test();
	var running = false;

	jQuery("#yume-run").click(function() {
		$(this).attr("disabled", true);
		if (running) {
			alert("Yume running !!!");
		} else {
			current_input = new input(jQuery("#yume-input").val());
			var o = jQuery("#yume-output");
			o.val("");
			current_output = new output(function(s) {
				o.val(o.val() + s);
			});
			run(application(new yume._continue(function(cps, scope, result) {
				current_input = undefined;
				current_output.flush(current_output.buffer);
				current_output = undefined;
				running = false;
				$(this).attr("disabled", false);
				return undefined;
			},
			null), yume._null_list));
		}
	});
});

