console = {
    log: function() {
	print(Array.prototype.join.call(arguments, ' '));
    },
    warn: function() {
	print(Array.prototype.join.call(arguments, ' '));
    }
};
