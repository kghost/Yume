var dummy = function() {};

jQuery(window).load(function() {
	var run = function(r) {
		try {
			for (var count = 0; count < 10000 && r !== null; count++) {
				r = yume.continue_run(r);
			}
			if (r !== null) {
				window.setTimeout(function() {
					run(r);
				},
				0);
			}
		} catch(s) {
			alert("OOps ... got error " + s);
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

	var output = function(out) {
		this.closed = false;
		this.out = out;
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
				this.out(this.buffer);
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
				this.out(this.buffer);
				this.buffer = s;
			}
			return undefined;
		},
		flush: function() {
		       this.out(this.buffer);
		       this.buffer = "";
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

	var notify = (function() {
		var box = $("#yume-notify");
		var messages = new list();
		var show = function() {
			if (!messages.empty()) {
				box.text(messages.tail().e);
				box.show();
			} else {
				box.hide();
			}
		};
		return function(text) {
			var n = messages.push_back(text);
			show();
			return {
				dismiss: function() {
					n.remove();
					show();
				}
			};
		};
	})();
	yume.load = function(cps, url) {
		var box = notify("loading " + url + " ...");
		jQuery.get("gen/" + url + ".js", function(data) {
			box.dismiss();
			var module = eval(data);
			run(module(cps, yume._null_list));
		});
		return null; // interrupt schme engine
	};

	yume.open_input_file = function(cps, url) {
		jQuery.get("files/" + url, function(data) {
			run({
				cps: cps,
				result: new yume._input(new input(data))
			});
		});
		return null; // interrupt schme engine
	};

	var application = test();
	var running = false;

	var button_compile = jQuery("#yume-compile");
	var button_run = jQuery("#yume-run");
	var text_expanded_code = jQuery("#yume-expanded-code");
	var text_compiled_code = jQuery("#yume-compiled-code");
	var text_output = jQuery("#yume-output");
	var a_expanded_code_toggle = jQuery("#yume-expanded-code-toggle");
	var a_compiled_code_toggle = jQuery("#yume-compiled-code-toggle");

	a_expanded_code_toggle.click(function() {
		text_expanded_code.toggle();
	});

	a_compiled_code_toggle.click(function() {
		text_compiled_code.toggle();
	});

	yume.global_add("expanded-port", new yume._procedure(function(cps, scope) {
		text_expanded_code.val("");
		var buffered_output = new output(function(s) {
			text_expanded_code.val(text_expanded_code.val() + s);
		});
		return {
			cps: cps,
			result: new yume._output(buffered_output)
		};
	},
	yume._null_list, 0, false));

	// run compiled js
	button_run.click(function() {
		if (text_compiled_code.val().length > 0) {
			var box = notify("running ...");
			button_run.attr("disabled", true);
			current_input = new yume._input(new input(jQuery("#yume-input").val()));
			text_output.val("");
			var buffered_output = new output(function(s) {
				text_output.val(text_output.val() + s);
			});
			current_output = new yume._output(buffered_output);
			run(eval(text_compiled_code.val())(new yume._continue(function(cps, scope, result) {
				current_input = undefined;
				current_output = undefined;
				buffered_output.flush(buffered_output.buffer);
				buffered_output = undefined;
				running = false;
				button_run.removeAttr("disabled");
				box.dismiss();
				return null;
			},
			null), yume._null_list));
		}
	});

	// compile scheme
	button_compile.click(function() {
		button_compile.attr("disabled", true);
		if (running) {
			alert("Yume running !!!");
		} else {
			var box = notify("compiling ...");
			current_input = new yume._input(new input(jQuery("#yume-source").val()));
			text_compiled_code.val("");
			var buffered_output = new output(function(s) {
				text_compiled_code.val(text_compiled_code.val() + s);
			});
			current_output = new yume._output(buffered_output);
			run(application(new yume._continue(function(cps, scope, result) {
				current_input = undefined;
				current_output = undefined;
				buffered_output.flush(buffered_output.buffer);
				buffered_output = undefined;
				running = false;
				button_compile.removeAttr("disabled");
				box.dismiss();
				return null;
			},
			null), yume._null_list));
		}
	});

	function getParameterByName(name)
	{
		name = name.replace(/[\[]/, "\\\[").replace(/[\]]/, "\\\]");
		var regexS = "[\\?&]" + name + "=([^&#]*)";
		var regex = new RegExp(regexS);
		var results = regex.exec(window.location.search);
		if(results == null)
			return undefined;
		else
			return decodeURIComponent(results[1].replace(/\+/g, " "));
	}

	var code = getParameterByName("code");
	if (code !== undefined) {
		jQuery("#yume-source").val(code);
	}
});

