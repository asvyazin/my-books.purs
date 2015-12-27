"use strict";


// module Libs.PouchDB


import PouchDB from "pouchdb";


function newPouchDB (name) {
    return new PouchDB(name);
}


function putFFI (db) {
    return function (docId) {
	return function (docRev) {
	    return function (doc) {
		return function (errorCallback) {
		    return function (successCallback) {
			db.put(doc, docId, docRev, undefined, function (error, result) {
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
}


function postFFI (db) {
    return function (doc) {
	return function (errorCallback) {
	    return function (successCallback) {
		db.post(doc, undefined, function (error, result) {
		    if (error) {
			errorCallback(error)();
		    } else {
			successCallback(result)();
		    }
		});
	    };
	};
    };
}


function getFFI (db) {
    return function (docId) {
	return function (errorCallback) {
	    return function (successCallback) {
		db.get(docId, undefined, function (error, result) {
		    if (error) {
			errorCallback(error)();
		    } else {
			successCallback(result)();
		    }
		});
	    };
	};
    };
}


export { newPouchDB, putFFI, postFFI, getFFI };
