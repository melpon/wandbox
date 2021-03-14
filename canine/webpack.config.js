/* eslint-disable @typescript-eslint/no-var-requires */
const webpack = require("webpack");
const path = require("path");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const CopyPlugin = require("copy-webpack-plugin");
const BundleAnalyzerPlugin = require("webpack-bundle-analyzer")
  .BundleAnalyzerPlugin;

var config = {
  mode:
    process.env.NODE_ENV === undefined || process.env.NODE_ENV === "development"
      ? "development"
      : "production",
  entry: { wandbox: "./src/index.tsx" },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: {
          loader: "ts-loader",
          options: {
            transpileOnly: true,
            happyPackMode: true,
            experimentalWatchApi: true,
          },
        },
        include: path.resolve(__dirname, "src"),
      },
      {
        test: /\.(c|s[ac])ss$/i,
        use: [MiniCssExtractPlugin.loader, "css-loader", "sass-loader"],
      },
    ],
  },
  optimization: {
    splitChunks: {
      name: "vendor",
      chunks: "initial",
    },
  },
  output: {
    filename: "static/js/[name].js",
    chunkFilename: "static/js/[name].bundle.js",
    path: path.resolve(__dirname, "dist"),
  },
  resolve: {
    alias: {
      "~": path.resolve(__dirname, "src"),
    },
    extensions: [".tsx", ".ts", ".js"],
  },
  plugins: (process.env.BUILD_STATS
    ? [
        new BundleAnalyzerPlugin({
          analyzerMode: "static",
          reportFilename: "stats/report.html",
        }),
      ]
    : []
  ).concat([
    new MiniCssExtractPlugin({
      filename: "static/css/[name].css",
    }),
    new CopyPlugin({
      patterns: [
        {
          from: path.resolve(__dirname, "src/index.html"),
          to: path.resolve(__dirname, "dist/index.html"),
        },
      ],
    }),
    new webpack.DefinePlugin({
      "process.env.NODE_ENV": JSON.stringify(
        process.env.NODE_ENV === undefined ||
          process.env.NODE_ENV === "development"
          ? "development"
          : "production"
      ),
      "process.env.WANDBOX_URL_PREFIX": JSON.stringify(
        process.env.NODE_ENV === undefined ||
          process.env.NODE_ENV === "development"
          ? "https://staging.wandbox.org"
          : // 空文字にしておけば /api/list.json みたいになる
            ""
      ),
    }),
  ]),
  devServer: {
    historyApiFallback: true,
  },
};

module.exports = (_env, argv) => {
  if (argv.mode === "development") {
    config.devtool = "source-map";
  }
  return config;
};
