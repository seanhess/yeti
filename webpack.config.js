const path = require('path');

module.exports = {
  entry: './js/deps.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'deps.js'
  }
};
