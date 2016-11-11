var path = require('path');
var webpack = require('webpack');

var ROOT = path.resolve(__dirname, 'src/main/webapp');
var SRC = path.resolve(ROOT, 'src');
var DEST = path.resolve(__dirname, 'src/main/webapp/dist');
var VS_ROOT = path.resolve(__dirname, 'node_modules/monaco-editor/min/vs');

//var uglifyPlugin = new webpack.optimize.UglifyJsPlugin({
//	compress: true,
//	mangle: false
//});

module.exports = {

	entry: {
		app: SRC + '/index.ts'
	},

	output: {
		path: DEST,
		filename: 'bundle.js',
		publicPath: '/dist/'
	},

	devtool: 'source-map',

	resolve: {
		root: [
			SRC
		],
		extensions: ['', '.webpack.js', '.web.js', '.ts', '.tsx', '.js']
	},

	module: {
		loaders: [{
			test: /\.tsx?$/,
			loader: 'ts-loader'
		}]
	},

	plugins: [
		new require('copy-webpack-plugin')([{
			from: VS_ROOT,
			to: 'vs',
		}])
//		,uglifyPlugin
	]
};