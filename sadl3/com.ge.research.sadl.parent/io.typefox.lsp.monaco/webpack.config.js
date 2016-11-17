var path = require('path');
var webpack = require('webpack');

var SRC_PATH = path.resolve(__dirname, 'src/main/ts');
var BUILD_PATH = path.resolve(__dirname, 'src/main/webapp/dist');
var VS_ROOT = path.resolve(__dirname, 'node_modules/monaco-editor/min/vs');

//var uglifyPlugin = new webpack.optimize.UglifyJsPlugin({
//	compress: true,
//	mangle: false
//});

module.exports = {

	entry: {
		app: SRC_PATH
	},

	output: {
		path: BUILD_PATH,
		filename: 'bundle.js',
		publicPath: '/dist/'
	},

	devtool: 'source-map',

	resolve: {
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