const path = require('path');

module.exports = {
  entry: './js/index.js',
  mode: 'development',
  devtool: 'inline-source-map',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'build.js'
  }
};
