var path = require("path");
var webpack = require("webpack");
var fableUtils = require("fable-utils");

function resolve(filePath) {
  return path.join(__dirname, filePath)
}

var babelOptions = fableUtils.resolveBabelOptions({
  presets: [["es2015", { "modules": false }]],
  plugins: ["transform-runtime"]
});

var isProduction = process.argv.indexOf("-w") < 0;
console.log("Bundling for " + (isProduction ? "production" : "development") + "...");

var basicConfig = {
  resolve: {
    modules: [resolve("./node_modules/")]
  },
  node: {
    __dirname: false,
    __filename: false
  },
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: {
          loader: "fable-loader",
          options: {
            babel: babelOptions,
            define: isProduction ? ["DEBUG"] : ["DEBUG", "WATCH"]
          }
        }
      },
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: babelOptions
        },
      }
    ]
  }
};

const CopyWebpackPlugin = require('copy-webpack-plugin');
//const UglifyJSPlugin = require('uglifyjs-webpack-plugin')

var mainConfig = Object.assign({
  target: "electron-main",
  entry: resolve("src/Main/Main.fsproj"),
  output: {
    path: resolve("."),
    filename: "main.js"
  }
}, basicConfig);

var rendererConfig = Object.assign({
  plugins: [

    //new UglifyJSPlugin(),

    new CopyWebpackPlugin([
      {
        from: 'node_modules/monaco-editor/min/vs',
        to: 'vs',
      },
      {
        from: 'node_modules/tippy.js/dist/tippy.all.min.js',
        to: 'app/js/tippy.all.min.js',
      }
    ])
  ],
  target: "electron-renderer",
  devtool: "source-map",
  entry: resolve("src/Renderer/Renderer.fsproj"),
  output: {
    path: resolve("app/js"),
    filename: "renderer.js"
  },
  externals: {
    "monaco": "var monaco",
    "editor": "var editor",
    "fable-repl": "var Fable",
  }
}, basicConfig);

module.exports = [mainConfig, rendererConfig]
