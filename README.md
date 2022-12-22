# swc-plugin-dev-expression [![npm](https://img.shields.io/npm/v/swc-plugin-dev-expression)]((https://img.shields.io/npm/v/swc-plugin-dev-expression)) [![Crates.io](https://img.shields.io/crates/v/swc-plugin-dev-expression)]((https://img.shields.io/npm/v/swc-plugin-dev-expression)) [![GitHub](https://img.shields.io/github/license/twnk/swc-plugin-dev-expression/blob/main/LICENCE)]()

A port of Facebook's [dev-expression Babel plugin](https://github.com/4Catalyzer/babel-plugin-dev-expression).

This plugin gates calls to `warning()` behind `process.env.NODE_ENV !== 'production'`, modifies calls to `invariant()` to hoist the condition outside of the function call, and replaces `__DEV__` expressions with a `NODE_ENV` check. Most bundlers will then strip these calls out of production code entirely. The transform applied to `warning()` and `invariant()` is identical to the behaviour of `babel-plugin-dev-expression`. The transformation of `__DEV__` differs and is only done in more constrained conditions, for ease of implementation. See note below.

## `invariant`

Replaces

```js
invariant(condition, argument, argument);
```

with

```js
if (!condition) {
  if ("production" !== process.env.NODE_ENV) {
    invariant(false, argument, argument);
  } else {
    invariant(false);
  }
}
```

Recommended for use with https://github.com/zertosh/invariant or smaller https://github.com/alexreardon/tiny-invariant.

## `warning`

Replaces

```js
warning(condition, argument, argument);
```

with

```js
if ("production" !== process.env.NODE_ENV) {
  warning(condition, argument, argument);
}
```

Recommended for use with https://github.com/r3dm/warning or smaller https://github.com/alexreardon/tiny-warning.

## `__DEV__`

Replaces

```js
if (__DEV__) {}
(__DEV__ ? yes() : no());
!__DEV__;
(__DEV__ && true);
(__DEV__ || false);
```

with

```js
if(process.env.NODE_ENV !== 'production') {}
(process.env.NODE_ENV !== 'production' ? yes() : no());
!(process.env.NODE_ENV !== 'production');
(process.env.NODE_ENV !== 'production') && true;
(process.env.NODE_ENV !== 'production') || false;
```

**Note:** The plugin does not transform every single reference to `__DEV__`. It will only make this substitution if `__DEV__` is the entire condition in an `if` statement or `?` conditional expression, or if `__DEV__` is an argument to a logical expression (`!`, `&&` or `||`). If this limitation poses any problems for your usage, please get in touch. 

## License
*This project is Copyright of Angel Wells, licenced under GPL 3.0 or later.*