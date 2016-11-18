var path = require('path');
var webpack = require('webpack');
var CopyWebpackPlugin = new require('copy-webpack-plugin');

var SRC_PATH = path.resolve(__dirname, 'src/main/ts');
var BUILD_PATH = path.resolve(__dirname, 'src/main/webapp/dist');
var VS_ROOT = path.resolve(__dirname, 'node_modules/monaco-editor/min/vs');

// TODO: extract a prod webpack config
/*var uglifyPlugin = new webpack.optimize.UglifyJsPlugin({
	compress: true,
	mangle: false
});*/

module.exports = {
	entry: SRC_PATH,
	output: {
		path: BUILD_PATH,
		filename: 'bundle.js'
	},
	devtool: 'source-map',
	resolve: {
		extensions: ["", ".webpack.js", ".web.js", ".ts", ".tsx", ".js", ".jsx"]
	},
	module: {
		loaders: [
			{ test: /\.css$/, loaders: ['style', 'css'] },
			{ test: /\.tsx?$/, loaders: ['ts-loader'] }
		],
		preLoaders: [
			{ test: /\.js$/, loader: "source-map-loader" }
		]
	},
	plugins: [
		new CopyWebpackPlugin([{
			from: VS_ROOT,
			to: 'vs',
		}])
		/*,
		uglifyPlugin*/
	]
};
