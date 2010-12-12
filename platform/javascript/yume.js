var dummy = function() {};

jQuery(window).load(function() {
	var run = function(r) {
		while (r !== undefined) {
			r = yume.continue_run(r.cps, r.result);
		}
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
				return new yume._char(this.s.charCodeAt(this.index++));
			}
		},
		peek: function() {
			if (this.closed) {
				throw "runtime-error: port closed";
			}
			if (this.index >= this.s.length) {
				return yume._eof;
			} else {
				return new yume._char(this.s.charCodeAt(this.index));
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
			if (this.buffer.length < 4095) {
				this.buffer += String.fromCharCode(c);
			} else {
				this.flush(this.buffer);
				this.buffer = String.fromCharCode(c);
			}
			return undefined;
		},
		write_string: function(s) {
			if (this.closed) {
				throw "runtime-error: port closed";
			}
			if (this.buffer.length + s.length < 4096) {
				this.buffer += s;
			} else {
				this.flush(this.buffer);
				this.buffer = s;
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

	yume.load = function(cps, url) {
		jQuery.get("gen/" + url + ".js", function(data) {
			var module = eval(data);
			run(module(cps, yume._null_list));
		});
		return undefined; // interrupt schme engine
	};

	yume.open_input_file = function(cps, url) {
		jQuery.get("files/" + url, function(data) {
			run({
				cps: cps,
				result: new yume._input(new input(data))
			});
		});
		return undefined; // interrupt schme engine
	};

	var application = test();
	var running = false;

	var button_run = jQuery("#yume-run");
	button_run.click(function() {
		button_run.attr("disabled", true);
		if (running) {
			alert("Yume running !!!");
		} else {
			current_input = new yume._input(new input(jQuery("#yume-input").val()));
			var o = jQuery("#yume-output");
			o.val("");
			var buffered_output = new output(function(s) {
				o.val(o.val() + s);
			});
			current_output = new yume._output(buffered_output);
			run(application(new yume._continue(function(cps, scope, result) {
				current_input = undefined;
				current_output = undefined;
				buffered_output.flush(buffered_output.buffer);
				buffered_output = undefined;
				running = false;
				button_run.removeAttr("disabled");
				return undefined;
			},
			null), yume._null_list));
		}
	});
});

