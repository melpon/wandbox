/* eslint-disable @typescript-eslint/no-var-requires */
const webpack = require("webpack");
const path = require("path");
const CopyPlugin = require("copy-webpack-plugin");
const BundleAnalyzerPlugin = require("webpack-bundle-analyzer")
  .BundleAnalyzerPlugin;

var config = {
  mode:
    process.env.NODE_ENV === undefined || process.env.NODE_ENV === "development"
      ? "development"
      : "production",
  entry: "./src/index.tsx",
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: {
          loader: "ts-loader",
          options: {
            transpileOnly: true,
            happyPackMode: true,
            experimentalWatchApi: true
          }
        },
        include: path.resolve(__dirname, "src")
      },
      {
        test: /\.css$/i,
        exclude: /\.module\.css$/i,
        use: ["style-loader", "css-loader"]
      },
      {
        test: /\.module\.css$/i,
        use: [
          { loader: "style-loader" },
          {
            loader: "css-loader",
            options: {
              modules: true
            }
          }
        ]
      }
    ]
  },
  output: {
    filename: "bundle.js",
    path: path.resolve(__dirname, "dist")
  },
  resolve: {
    alias: {
      "~": path.resolve(__dirname, "src")
    },
    extensions: [".tsx", ".ts", ".js"]
  },
  plugins: (process.env.BUILD_STATS
    ? [
        new BundleAnalyzerPlugin({
          analyzerMode: "static",
          reportFilename: "stats/report.html"
        })
      ]
    : []
  ).concat([
    new CopyPlugin([
      {
        from: path.resolve(__dirname, "src/index.html"),
        to: path.resolve(__dirname, "dist/index.html")
      }
    ]),
    new webpack.EnvironmentPlugin({ NODE_ENV: "develop" })
  ]),
  devServer: {
    historyApiFallback: true
  }
};

module.exports = (_env, argv) => {
  if (argv.mode === "development") {
    config.devtool = "source-map";
  }
  return config;
};
