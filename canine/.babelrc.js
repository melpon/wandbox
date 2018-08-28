module.exports = {
  plugins: [
    '@babel/plugin-proposal-object-rest-spread'
  ],
  presets: [
    ['@babel/preset-env', { useBuiltIns: 'usage' }],
    '@babel/preset-react',
    '@babel/preset-flow'
  ]
}