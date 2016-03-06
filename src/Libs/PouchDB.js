"use strict";


// module Libs.PouchDB


var PouchDB = require('pouchdb');


exports.newPouchDB = function (name) {
    return function () {
	return new PouchDB(name);
    };
}


exports.putFFI = function (db) {
    return function (doc) {
	return function (errorCallback) {
	    return function (successCallback) {
		return function () {
		    db.put(doc, function (error, result) {
			if (error) {
			    errorCallback(error)();
			} else {
			    successCallback(result)();
			}
		    });
		    return {};
		};
	    };
	};
    };
}


exports.postFFI = function (db) {
    return function (doc) {
	return function (errorCallback) {
	    return function (successCallback) {
		return function () {
		    db.post(doc, function (error, result) {
			if (error) {
			    errorCallback(error)();
			} else {
			    successCallback(result)();
			}
		    });
		    return {};
		};
	    };
	};
    };
}


exports.getFFI = function (db) {
    return function (docId) {
	return function (errorCallback) {
	    return function (successCallback) {
		return function () {
		    db.get(docId, function (error, result) {
			if (error) {
			    errorCallback(error)();
			} else {
			    successCallback(result)();
			}
		    });
		    return {};
		};
	    };
	};
    };
}


exports.tryGetFFI = function (db) {
    return function (docId) {
	return function (errorCallback) {
	    return function (successCallback) {
		return function () {
		    db.get(docId, function (error, result) {
			if (error) {
			    successCallback(null)();
			} else {
			    successCallback(result)();
			}
		    });
		    return {};
		};
	    };
	};
    };
}
