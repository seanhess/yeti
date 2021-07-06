const path = require('path');

module.exports = {
  entry: './index.js',
  mode: 'development',
  devtool: 'inline-source-map',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'build.js'
  }
};
