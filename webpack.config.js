const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

const isProduction = process.env['NODE_ENV'] === 'production';

const mode = isProduction ? 'production' : 'development';

console.log({ mode });

module.exports = {
  mode,
  devtool: isProduction ? false : 'eval-source-map',
  entry: './src/App.fsproj',
  output: {
    path: path.join(__dirname, './out'),
    filename: 'bundle.js',
  },
  devServer: {
    contentBase: './public',
    port: 8080,
  },
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: 'fable-loader',
      },
    ],
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: './index.html',
    })
  ],
};
