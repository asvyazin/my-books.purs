import { createApp } from 'couchapp/main';
import usersApp from './users/app.users';

const myBooksDb = 'http://localhost:5984/my-books';

createApp(usersApp, myBooksDb, app => {
    app.push();
});
