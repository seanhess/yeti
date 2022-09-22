 
const path = require('path');

module.exports = {
  entry: "./live/index.ts",
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
    ],
  },
  resolve: {
    modules: ["node_modules",path.resolve(__dirname, "live")],
    extensions: ['.tsx', '.ts', '.js'],
  }
}