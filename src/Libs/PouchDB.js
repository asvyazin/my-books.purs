"use strict";


// module Libs.PouchDB


import PouchDB from "pouchdb";


function newPouchDB (name) {
    return function () {
	return new PouchDB(name);
    };
}


function putFFI (db) {
    return function (doc) {
	return function (errorCallback) {
	    return function (successCallback) {
		return function () {
		    console.log('put', doc);
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


function postFFI (db) {
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


function getFFI (db) {
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


function tryGetFFI (db) {
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


export { newPouchDB, putFFI, postFFI, getFFI, tryGetFFI };
