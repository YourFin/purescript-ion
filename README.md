# Purescript ion

WIP unofficial purescript parser for the [Amazon Ion](https://amzn.github.io/ion-docs/) data serialization format.

Note that this library does not make use of, nor is designed to integrate with, the [official Amazon Ion javascript implementation](https://github.com/amzn/ion-js).

## Use

For now, you'll have to git clone it manually. Note that this library depends on [purescript-bigints](https://github.com/purescript-contrib/purescript-bigints/blob/master/README.md), and therefore transitively requires [BigInteger.js](https://github.com/peterolson/BigInteger.js)
by [Peter Olson](https://github.com/peterolson). 

[BigInteger.js](https://github.com/peterolson/BigInteger.js) can be installed via `npm`:
```
bower install purescript-bigints
npm install big-integer
```

For the browser, remember to bundle `BigInteger.min.js` with your code.
