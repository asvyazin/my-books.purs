// module Components.Wrappers.Router


var reactRouter = require('react-router');
var Router = reactRouter.Router;
var Route = reactRouter.Route;
var hashHistory = reactRouter.hashHistory;
var browserHistory = reactRouter.browserHistory;
var Link = reactRouter.Link;


exports.routerFFI = Router;
exports.routeFFI = Route;
exports.hashHistory = hashHistory;
exports.browserHistory = browserHistory;
exports.linkFFI = Link;
