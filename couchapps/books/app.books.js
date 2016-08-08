'use strict';

var ddoc = {
    _id: '_design/books',
    views: {
	all : {
	    map: function (doc) {
		if (doc.type === 'book') {
		    emit(null, null);
		}
	    }
	}
    }
};

module.exports = ddoc;
