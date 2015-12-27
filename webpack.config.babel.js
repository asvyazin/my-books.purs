var BowerWebpackPlugin = require('bower-webpack-plugin');

var path = require('path');

var srcs = ['src[]=bower_components/purescript-*/src/**/*.purs', 'src[]=src/**/*.purs'];

var ffis = ['ffi[]=bower_components/purescript-*/src/**/*.js', 'ffi[]=src/**/*.js'];

var output = 'output';

var modulesDirectories = [
    './output',
    'node_modules',
    'bower_components/purescript-prelude/src'
];

module.exports = {
    context: path.resolve(__dirname, 'src'),
    entry: {
	index: 'Entries/Index.purs',
	'index.server': 'Entries/Index/Server.purs',
	login: 'Entries/Login.purs',
	'login.server': 'Entries/Login/Server.purs'
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
	    loader: 'babel' // 'babel-loader' is also a legal name to reference
	}]
    },
    resolve: {
	root: 'src',
	modulesDirectories: modulesDirectories,
	extensions: ['', '.js']
    },
    resolveLoader: {
	root: path.join(__dirname, 'node_modules')
    },
    plugins: [
	new BowerWebpackPlugin()
    ]
};
