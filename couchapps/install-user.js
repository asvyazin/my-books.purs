import { createApp } from 'couchapp/main';
import booksApp from './books/app.books';

const userId = process.argv.slice(2)[0];
const myUserDb = 'http://localhost:5984/my-books%2F' + userId;

createApp(booksApp, myUserDb, app => {
    app.push();
});
