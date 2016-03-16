'use strict';

var PurescriptWebpackPlugin = require('purescript-webpack-plugin');

var src = ['bower_components/purescript-*/src/**/*.purs', 'src/**/*.purs'];

var ffi = ['bower_components/purescript-*/src/**/*.js', 'src/**/*FFI.js'];

var modulesDirectories = [
  'node_modules',
  'bower_components'
];

var config
  = { entry: './src/entry'
    , output: { path: __dirname
              , pathinfo: true
              , filename: 'bundle.js'
              }
    , module: { loaders: [ { test: /\.purs$/
                           , loader: 'purs-loader'
                           } ] }
    , resolve: { modulesDirectories: modulesDirectories, extensions: [ '', '.js', '.purs'] }
    , plugins: [ new PurescriptWebpackPlugin({src: src, ffi: ffi}) ]
    , devServer: { historyApiFallback: false }
    }
    ;

module.exports = config;
