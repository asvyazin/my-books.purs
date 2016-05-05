'use strict';

var ddoc = {
    _id: '_design/users',
    filters: {
	all: function (doc, req) {
	    return doc.type === 'userInfo';
	}
    }
};

module.exports = ddoc;
