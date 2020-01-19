const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const outputDir = path.join(__dirname, 'dist/');

const isProd = process.env.NODE_ENV === 'production';
const PUBLIC_PATH = process.env.PUBLIC_PATH || '/';

module.exports = {
  entry: './src/Quotes.bs.js',
  mode: isProd ? 'production' : 'development',
  output: {
    path: outputDir,
    filename: '[name].[contenthash].js',
    libraryTarget: 'var',
    library: 'Quotes',
    publicPath: PUBLIC_PATH
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: 'src/index.html'
    })
  ],
  devServer: {
    compress: true,
    contentBase: outputDir,
    port: process.env.PORT || 8000,
    historyApiFallback: true
  }
};
