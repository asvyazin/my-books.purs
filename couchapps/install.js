import { createApp } from 'couchapp/main';
import usersApp from './users/app.users';
import booksApp from './books/app.books';

const myBooksDb = 'http://localhost:5984/my-books';

createApp(usersApp, myBooksDb, app => {
    app.push();
});

// it's temporary :(
const myUserDb = 'http://localhost:5984/my-books%2F24781fb625dc56ba';

createApp(booksApp, myUserDb, app => {
    app.push();
});
