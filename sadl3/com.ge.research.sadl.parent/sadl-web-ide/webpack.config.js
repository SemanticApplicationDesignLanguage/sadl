var path = require('path');
var webpack = require('webpack');
var merge = require('webpack-merge');
var WebpackNotifierPlugin = require('webpack-notifier');
var HtmlwebpackPlugin = require('html-webpack-plugin');

var TARGET = process.env.npm_lifecycle_event;
var ROOT_PATH = path.resolve(__dirname);
var APP_PATH = path.resolve(ROOT_PATH, 'src');
var BUILD_PATH = path.resolve(ROOT_PATH, 'build');

var common = {
    entry: APP_PATH,
    output: {
        path: BUILD_PATH,
        filename: "bundle.js"
    },
    resolve: {
        extensions: ["", ".webpack.js", ".web.js", ".ts", ".tsx", ".js", ".jsx"]
    },
    module: {
        loaders: [
            { test: /\.css$/, loaders: ['style', 'css'] },
            { test: /\.tsx?$/, loaders: ['react-hot-loader/webpack', 'ts-loader'] }
        ]
    },
    plugins: [
        new WebpackNotifierPlugin({ alwaysNotify: true }),
        new HtmlwebpackPlugin({
            title: 'React App!'
        })
    ]
};

if (TARGET === 'start' || !TARGET) {
    module.exports = merge(common, {
        entry: [
            'react-hot-loader/patch',
            'webpack-dev-server/client?http://localhost:3000',
            'webpack/hot/only-dev-server',
            APP_PATH
        ],
        devtool: 'source-map',
        module: {
            preLoaders: [
                { test: /\.js$/, loader: "source-map-loader" }
            ]
        },
        plugins: [
            new webpack.HotModuleReplacementPlugin()
        ]
    });
} else {
    module.exports = common;
}
