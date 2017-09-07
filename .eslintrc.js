module.exports = {
  "extends": "prettier",
  "env": {
      "browser": true,
      "es6": true,
      "node": true
  },
  "parserOptions": {
      "ecmaVersion": 6,
      "sourceType": "module",
      "ecmaFeatures": {
        "experimentalObjectRestSpread": true,
        "impliedStrict": true,
        "modules": true
    }
  },
  "plugins": [
      "prettier"
  ],
  "rules": {
      "prettier/prettier": [
        "error", { "singleQuote": true }
      ],
      "no-unused-vars": [1, { "vars": "local", "args": "after-used" }],
      "no-undef": 2,
      "no-unused-expressions": 2,
  }
};
