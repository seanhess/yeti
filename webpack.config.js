 
const path = require('path');

module.exports = {
  entry: "./live/index.js",
  module: {

  },
  resolve: {
    modules: ["node_modules",path.resolve(__dirname, "live")],
  }
}