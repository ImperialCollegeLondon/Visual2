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

var isProduction = process.argv.indexOf("-p") >= 0;
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
            define: isProduction ? [] : ["DEBUG"]
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

var mainConfig = Object.assign({
  target: "electron-main",
  entry: resolve("src/Main/Main.fsproj"),
  output: {
    path: resolve("."),
    filename: "main.js"
  }
}, basicConfig);

var rendererConfig = Object.assign({
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