# CHANGES IN arrayr from VERSION 0.1.8 to VERSION 0.1.9

## NEW FEATURES

- `HybridArray`
- `...` input and numeric `id` in `HybridArray$add()` is interpreted as position index
- Argument `sorting` in `Array$get()`
- `...` input and numeric `id` in `Array$exists()` is interpreted as position index
- `...` input and numeric `id` in `Array$index()` is interpreted as position index
- `...` input and numeric `id` in `Array$rm()` is interpreted as position index; argument `sorting`.
- `Array$copy()`: 
  - numeric `from` and `to` now possible
  - argument `char` introduced to force `from` and `to` to be interpreted as `character` values
  - argument `sorting` introduced
  
## BUG FIXES

- # 7
  `Array$get()` for alternative default
- # 8
  Refactor `Array()`

## MAJOR CHANGES

- M: `Array` --> `Array`
- M: `Array$initialize()`
- A: `Array$.set()`

## MINOR CHANGES

## MISC

- `Array` refactored until `$add()`

