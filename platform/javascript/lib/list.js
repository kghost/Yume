var list = (function() {
	var node = function(e, prev, next) {
		this.e = e;
		this.prev = prev || null;
		this.next = next || null;
	};
	node.prototype.remove = function() {
		this.prev.next = this.next;
		this.next.prev = this.prev;
		delete this.e;
		delete this.next;
		delete this.prev;
	};

	var node_nil = function() {};
	node_nil.prototype.remove = function() {
		throw "Can't delete NIL";
	};

	var list = function() {
		this.head_ = new node_nil();
		this.tail_ = new node_nil();
		this.head_.next = this.tail_;
		this.tail_.prev = this.head_;
	};
	list.prototype.empty = function() {
		return this.begin() === this.end();
	};
	list.prototype.head = function() {
		return this.head_;
	};
	list.prototype.begin = function() {
		return this.head_.next;
	};
	list.prototype.tail = function() {
		return this.tail_.prev;
	};
	list.prototype.end = function() {
		return this.tail_;
	};
	list.prototype.push_back = function(e) {
		var n = new node(e, this.tail_.prev, this.tail_);
		this.tail_.prev.next = n;
		this.tail_.prev = n;
		return n;
	};
	return list;
})();

