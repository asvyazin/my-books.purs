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
	    loader: 'purs-loader',
	    exclude: /node_modules/,
	    query: {
		psc: 'psa',
		src: ['bower_components/purescript-*/src/**/*.purs', 'src/**/*.purs'],
		ffi: ['bower_components/purescript-*/src/**/*.js', 'src/**/*.js']
	    }
	}, {
	    test: /\.js$/,
	    exclude: /(node_modules|bower_components)/,
	    loader: 'babel'
	}]
    },
    resolve: {
	modulesDirectories: modulesDirectories
    }
};
