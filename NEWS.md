# CHANGES IN arrayr VERSION 0.1.8

## NEW FEATURES

- private method: `ArrayEnvironment$.getNumericKeys()`
  Retrieve numeric key values.
  
- private method: `ArrayEnvironment$.indexNumericKeys()`
  Retrieve numeric key values.  
  
- private method: `ArrayEnvironment$.autoadjustNumericKeys()`
  Automatical sequence adjustment for `numeric` keys.
  Mimicks the respective behavior when manipulating the structure of **unnamed** `list`s via `numeric` indexes (e.g. when removing the *5th* list element of a list of length `6`, the labels/keys are automatically adjusted so that there is a new 5th element)
  
- argument `numonly` in `ArrayEnvironment$rm()`:
  when using a `numeric` index, should only `numeric` keys or also 
  `character` keys be considered? Has to do with auto-adjusting numerical 
  keys

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN arrayr VERSION 0.1.7

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

- M: argument `id` to `...` in `ArrayEnviroment$rm()`
  Allows the specification via `c()` or `<1>, <2>, ..., <n>`  

## MINOR CHANGES

## MISC

-----

# CHANGES IN arrayr VERSION 0.1.6

## NEW FEATURES

- argument `intnum` in `.set()` of `ArrayEnvironment`:
  Converts `integer` to `numeric` values to ensure numerical consistency
- `ArrayEnvironment$exists()` can handle `character` and `numeric` indexes;
  the latter is interpreted as a position index
- argument `char` in `ArrayEnvironment$exists()`
  Coerces `numeric` index to `character` index
- argument `all_names` in `ArrayEnvironment$index()`
  Controls if hidden elements should also be considered or not
- arguments `all_names`, `char`, `sorted` in `ArrayEnvironment$copy()`

## BUG FIXES

## MAJOR CHANGES

- M: argument `id` in `ArrayEnvironment$get()` substituted by `...`
  Allows the specification via `c()` or `<1>, <2>, ..., <n>`
- M: argument `id` in `ArrayEnvironment$exists()` substituted by `...`
  Allows the specification via `c()` or `<1>, <2>, ..., <n>`
- M: argument `id` in `ArrayEnvironment$index()` substituted by `...`
  Allows the specification via `c()` or `<1>, <2>, ..., <n>`  
- M: numerical indexes possible in `ArrayEnvironment$copy()`

## MINOR CHANGES

- M: return value of `add()` when `overwrite = FALSE` in `ArrayEnvironment`:
  names are now included
- M: return value of `.set()`:
  names are now included

## MISC

-----

# CHANGES IN arrayr VERSION 0.1.5

## NEW FEATURES

- numeric indexes for `ArrayEnvironment()` in `get()`

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN arrayr VERSION 0.1.4

## NEW FEATURES

- argument `sorted`
  - currently only for `ArrayEnvironment`
  - currently affects: `get()`, `rm()`
- argument `all_names`
  - currently only for `ArrayEnvironment`
  - currently affects: `get()`, `rm()`

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN arrayr VERSION 0.1.3

## NEW FEATURES

## BUG FIXES

- fixed: #3
  inconsistency for atomic input in `add()`

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN arrayr VERSION 0.1.2

## NEW FEATURES

## BUG FIXES

- fixed: #2
  inconsistency with respect to numerical input of certain methods

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN arrayr VERSION 0.1.1

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

- modified: initialization method in `Array` and `ArrayEnvironment` in order to account for the specification of atomic elements
- modified: README

## MISC

-----

# CHANGES IN arrayr VERSION 0.1

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

- initial version

-----


