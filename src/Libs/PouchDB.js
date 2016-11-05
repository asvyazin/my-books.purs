"use strict";


// module Libs.PouchDB


var PouchDB = require('pouchdb');


exports.newPouchDB = function (name) {
    return function () {
	return new PouchDB(name);
    };
};


exports.newPouchDBOpt = function (name) {
    return function (options) {
	return function () {
	    return new PouchDB(name, options);
	};
    };
};


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
		};
	    };
	};
    };
};


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
		};
	    };
	};
    };
};


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
		};
	    };
	};
    };
};


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
		};
	    };
	};
    };
};


exports.sync = function (src) {
    return function (dest) {
	return function (options) {
	    return function () {
		return PouchDB.sync(src, dest, options);
	    };
	};
    };
};


exports.cancel = function (sync) {
    return function () {
	return sync.cancel();
    };
};


exports.debugEnable = function (str) {
    return function () {
	return PouchDB.debugEnable(str);
    };
};


exports.queryFFI = function (db) {
    return function (index) {
	return function (options) {
	    return function (errorCallback) {
		return function (successCallback) {
		    return function () {
			db.query(index, options, function (err, result) {
			    if (err) {
				errorCallback(err)();
			    } else {
				successCallback(result)();
			    }
			});
		    };
		};
	    };
	};
    };
};
