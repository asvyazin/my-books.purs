var path = require('path');

var srcs = ['src[]=bower_components/purescript-*/src/**/*.purs', 'src[]=src/**/*.purs'];

var ffis = ['ffi[]=bower_components/purescript-*/src/**/*.js', 'ffi[]=src/**/*.js'];

var output = 'output';

var modulesDirectories = [
    'node_modules',
    'bower_components'
];

module.exports = {
    entry: {
	index: './js/Index.js',
	'index.server': './src/Entries/Index/Server.purs',
	login: './js/Login.js',
	'login.server': './src/Entries/Login/Server.purs'
    },
    output: {
	path: path.resolve(__dirname, "public/js"),
	filename: '[name].bundle.js'
    },
    module: {
	loaders: [{
	    test: /\.purs$/,
	    loader: 'purs-loader?output=' + output + '&' + srcs.concat(ffis).join('&')
	}, {
	    test: /\.js$/,
	    exclude: /(node_modules|bower_components)/,
	    loader: 'babel'
	}]
    },
    resolve: {
	root: 'src',
	modulesDirectories: modulesDirectories,
	extensions: ['', '.js']
    },
    resolveLoader: {
	root: path.join(__dirname, 'node_modules')
    }
};
