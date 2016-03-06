var PurescriptWebpackPlugin = require('purescript-webpack-plugin');

var path = require('path');

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
	    loader: 'purs-loader'
	}, {
	    test: /\.js$/,
	    exclude: /(node_modules|bower_components)/,
	    loader: 'babel'
	}]
    },
    resolve: {
	modulesDirectories: modulesDirectories
    },
    plugins: [new PurescriptWebpackPlugin()]
};
