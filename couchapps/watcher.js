import PouchDB from 'pouchdb';
import Security from 'pouchdb-security-helper';


async function main() {
    const username = process.env.COUCHDB_ADMIN_USERNAME;
    const password = process.env.COUCHDB_ADMIN_PASSWORD;

    const db = new PouchDB('http://localhost:5984/my-books', {
	auth: { username, password }
    });

    const proxyDb = new PouchDB('http://localhost:5984/my-books-proxy', {
	auth: { username, password }
    });

    try {
	await db.security().reset({
	    members: {
		roles: [ 'users' ]
	    }
	}).save();
	await putAlways(db, {
	    _id: '_design/users',
	    filters: {
		all: function (doc, req) {
		    return doc.type === 'userInfo';
		}.toString()
	    }
	});
	console.log('my-books database initialized');

	await proxyDb.security().reset({
	    members: {
		names: [ 'admin' ]
	    }
	}).save();
	await putAlways(proxyDb, {
	    _id: '_design/users',
	    views: {
		'by-token': {
		    map: function (doc) {
			if (doc.type === 'userToken') {
			    emit(doc.token, null);
			}
		    }.toString()
		}
	    }
	});
	console.log('my-books-proxy database initialized');

	db.changes({
	    filter: 'users/all',
	    live: true
	}).on('change', async function (c) {
	    console.log(c);
	    
	    const userDb = new PouchDB('http://localhost:5984/my-books%2F' + c.id, {
		auth: { username, password }
	    });

	    try {
		await userDb.security().reset({
		    members: {
			names: [ c.id ]
		    }
		}).save();
		await putAlways(userDb, {
		    _id: '_design/books',
		    views: {
			all : {
			    map: function (doc) {
				if (doc.type === 'book') {
				    emit(null, null);
				}
			    }.toString()
			}
		    }
		});		
		console.log(`my-books%2F${c.id} database initialized`);
	    } catch (err) {
		console.error('Error initializing user database:', err, 'userId:', c.id);
	    }
	}).on('error', err => {
	    console.error(err);
	});
    } catch (err) {
	console.error('Error initializing my-books database', err);
    }
}


async function putAlways(db, doc) {
    try {
	const prevDoc = await db.get(doc._id);
	doc._rev = prevDoc._rev;
    } catch (err) {
	if (err.status !== 404) {
	    throw err;
	}
    }

    return await db.put(doc);
}


PouchDB.plugin(Security);
main();
