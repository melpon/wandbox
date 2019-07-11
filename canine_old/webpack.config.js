const path = require('path')
const history = require('connect-history-api-fallback')
const convert = require('koa-connect')
//const webpack = require('webpack')
//const pkg = require('./package')

// const BASE_PLUGINS = [
//   new webpack.DefinePlugin({
//     'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV)
//   })
// ]
module.exports = {
  entry: {
    app: ['./src/main.js']
  },
  mode: process.env.WEBPACK_SERVE ? 'development' : 'production',
  output: {
    filename: '[name].bundle.js',
    path: path.join(__dirname, 'public'),
    publicPath: '/'
  },
  resolve: {
    alias: {
      '~': path.resolve(__dirname, 'src/')
    }
  },
  serve: {
    add: (app, _middleware, _options) => {
      const historyOptions = {
        // https://github.com/bripkens/connect-history-api-fallback#options
      }
      app.use(convert(history(historyOptions)))
    },
    content: [path.resolve(__dirname, 'public')],
    hot: true,
    port: 21635
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        use: 'babel-loader',
        exclude: /node_modules/
      },
      {
        test: /\.css$/,
        use: [
          { loader: 'style-loader' },
          {
            loader: 'css-loader',
            options: {
              modules: false,
              importLoaders: 1
            }
          }
        ]
      }
    ]
  },
  plugins: [],
  devtool: 'inline-source-map'
}
