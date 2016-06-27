import path from 'path';

const modulesDirectories = [
  'node_modules',
  'bower_components'
];

export default {
    entry: {
	app: './js/App.js'
    },
    output: {
	path: path.resolve(__dirname, "public"),
	filename: '[name].bundle.js'
    },
    module: {
	loaders: [{
	    test: /\.purs$/,
	    loader: 'purs-loader',
	    exclude: /node_modules/,
	    query: {
		psc: 'psa',
		pscArgs: { sourceMaps: true },
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
    },
    devtool: 'source-map'
};
