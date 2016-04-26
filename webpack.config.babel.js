var PurescriptWebpackPlugin = require('purescript-webpack-plugin');

var path = require('path');

var modulesDirectories = [
  'node_modules',
  'bower_components'
];

module.exports = {
    entry: {
	app: './js/App.js'
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
