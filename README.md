arrayr
======

Manage array structures

## Installation

```
require("devtools")
devtools::install_github("Rappster/conditionr")
devtools::install_github("Rappster/arrayr")
require("arrayr")
```
## Purpose

The package provides classes and methods for managing array structures.

## Vignettes

- [Class `Array`](https://github.com/Rappster/arrayr/blob/master/vignettes/class_array.Rmd)
- [Class `ArrayEnvironment`](https://github.com/Rappster/arrayr/blob/master/vignettes/class_array_environment.Rmd)

----------

## Class `ArrayEnvironment` (environment based)

### Initialize

```
inst <- ArrayEnvironment$new(a = 1, b = 1)
inst$get(list = TRUE)
```

### Add 

```
inst$add(c = 1, d = 1)
inst$get(list = TRUE)
inst$add(new.env())
inst$get(list = TRUE)
```

## Set

```
inst$set(a = 10)
inst$get(list = TRUE)
inst$set(e = 1)
inst$get(list = TRUE)
inst$set(e = 1, must_exist = FALSE)
inst$get(list = TRUE)
```

## Get

```
inst$get("a")
inst$get("a", "b")
inst$get(1, 4)
```

## Exists

```
inst$exists("a")
inst$exists("a", "b")
inst$exists(1, 4)
```

## Index

```
inst$index("a")
inst$index("a", "b")
inst$index("1")
inst$index(1, 2)
```

## Clear 

```
inst$clear()
inst$get(list = TRUE)
```

## Remove 

```
inst <- ArrayEnvironment$new(list(1, a = 1, b = 1))
inst$get(list = TRUE)
inst$rm("a")
inst$exists("a")
inst$rm(c("b", "c"))
inst$get(list = TRUE)

inst$rm("a")
try(inst$rm("a", strict = 1))
try(inst$rm("a", strict = 2))

inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1, d = 1))
inst$rm(1)
inst$rm(1, 3)
inst$get(list = TRUE)
```

## Remove first 

```
inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rmFirst()
inst$get(list = TRUE)
inst$rmFirst(2)
inst$get(list = TRUE)
```

## Remove last

```
inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rmLast()
inst$get(list = TRUE)
inst$rmLast(2)
inst$get(list = TRUE)
```

## Copy

### Single/character

```
inst <- ArrayEnvironment$new(list(a = 1))
inst$copy("a", "b")
inst$get(list = TRUE)

inst$copy("c", "d")
inst$get(list = TRUE)
try(inst$copy("c", "d", strict = 1))
try(inst$copy("c", "d", strict = 2))
```

### Single/numeric

```
inst <- ArrayEnvironment$new(list(a = 1))
inst$copy(1, 2)
inst$get(list = TRUE)

inst$copy(3, 4)
inst$get(list = TRUE)
try(inst$copy(3, 4, strict = 1))
try(inst$copy(3, 4, strict = 2))
```

### Multiple/character

```
inst <- ArrayEnvironment$new(list(a = 1, b = 1))
inst$copy(c("a", "b"), c("c", "d"))
inst$get(list = TRUE)
inst$copy(c("a", "b"), c("c", "d"))
inst$get(list = TRUE)
```
----------

## Class `Array` (list based)

### Initialize

#### Default

```
inst <- Array$new()
inst$.array
```

#### Explicit

```
inst <- Array$new(list(a = 1, b = 2))
inst$.array
inst <- Array$new(list(c = 1), list(d = 2))
inst$.array
```

#### Atomic and mixed 

```
inst <- Array$new(a = 1, b = 2)
inst$.array
inst <- Array$new(a = 1, list(b = 2))
inst$.array
```

### Add 

#### Single

```
inst <- Array$new()
inst$add(list(a = 1))
inst$.array
inst$add(list(b = 2))
inst$.array

inst$add(list(b = 2), dups = FALSE)
inst$.array
try(inst$add(list(b = 2), dups = FALSE, strict = 1))
inst$.array
try(inst$add(list(b = 2), dups = FALSE, strict = 2))
inst$.array

inst$add(list(b = 2))
inst$.array
```
#### Single atomic 

```
## Single atomic //
inst <- Array$new()
inst$add(1)
inst$.array
inst$add(new.env())
inst$.array
inst$add(a = new.env())
inst$.array
inst$add(b = 1)
inst$.array
```

#### Multiple

```
inst <- Array$new()
inst$add(list(a = 1), list(b = 2))
inst$.array

inst$add(1, 1, 1, id = c("c", "d", "e"))
inst$.array

inst$add(1, 1, 1, id = c("e", "f"), dups = FALSE)
inst$.array

inst <- Array$new()
inst$add(1, 1, 1, id = c("a", "b"))
try(inst$add(1, 1, 1, id = c("a", "b"), strict = 1))
try(inst$add(1, 1, 1, id = c("a", "b"), strict = 2))
```

### Set 

#### Single

```
inst <- Array$new(list(a = 1))
inst$set(list(a = 2))
inst$.array
inst$set(list(b = 2))
try(inst$set(list(b = 2), strict = 1))
try(inst$set(list(b = 2), strict = 2))
inst$.array
```

#### Multiple

```
inst <- Array$new(list(a = 1, b = 1))
inst$set(list(a = 2), list(b = 2))
inst$.array

inst <- Array$new(list(a = 1, b = 1, c = 1))
inst$set(2, 2, 2, id = c("a", "b", "c"))
inst$.array

inst$set(2, 2, 2, id = c("a", "b", "d"))
inst$.array
try(inst$set(2, 2, 2, id = c("a", "b", "d"), strict = 1))
try(inst$set(2, 2, 2, id = c("a", "b", "d"), strict = 2))
```

#### Different lengths

```
inst$set(2, 2, 2, id = c("b", "c"))
try(inst$set(2, 2, 2, id = c("b", "c"), strict = 1))
try(inst$set(2, 2, 2, id = c("b", "c"), strict = 2))
```

#### New (or does not have to exist)

```
inst <- Array$new(list(a = 1))
inst$set(list(b = 1), must_exist = FALSE)
inst$.array
inst$set(list(c = 1, d = 1), must_exist = FALSE)
inst$.array
```

### Get

```
inst <- Array$new()
inst$get()

inst <- Array$new(array = list(a = 1, b = 1))
inst$.array, list(a = 1, b = 1))
inst$get()

inst$get("a")
inst$get("a", inner = FALSE)
inst$get(c("a", "b"))
inst$get(c("a", "b"), inner = FALSE)
inst$get("c")
inst$get("c", inner = FALSE)
inst$get(c("a", "c"))
inst$get(c("a", "c"), inner = FALSE)

inst <- Array$new(list(a = 1))
inst$get("b")
try(inst$get("b", strict = 1))
try(inst$get("b", strict = 2))
inst$get(c("a", "b"))
try(inst$get(c("a", "b"), strict = 1))
try(inst$get(c("a", "b"), strict = 2))
```

### Exists

```
inst <- Array$new(array = list(a = 1, b = 1))
inst$exists("a")
inst$exists(c("a", "b"))
inst$exists("c")
```

### Index 

```
inst <- Array$new(array = list(a = 1, b = 1))
inst$index("a")
inst$index(c("a", "b"))
inst$index("c")
try(inst$index("c", strict = 1))
try(inst$index("c", strict = 2))
inst$index(c("a", "c"))

inst$index("c", simplify = TRUE)
try(inst$index("c", simplify = TRUE, strict = 1))
try(inst$index("c", simplify = TRUE, strict = 2))
inst$index(c("a", "c"), simplify = TRUE)
```

### Clear

```
inst <- Array$new(array = list(a = 1, b = 1))
inst$clear()
inst$.array
```

### Remove

```
inst <- Array$new(array = list(a = 1, b = 1, c = 1))
inst$rm("a")
inst$exists("a")
inst$rm(c("b", "c"))
inst$rm("a")
inst$rm(c("a", "b"))
inst$add(list(a = 1))
inst$rm(c("a", "b"))
inst$rm("a")
try(inst$rm("a", strict = 1))
try(inst$rm("a", strict = 2))
```

### Remove first

```
inst <- Array$new(array = list(a = 1, b = 1, c = 1))
inst$rmFirst()
inst$get()
inst$rmFirst(2)
inst$get()

inst <- Array$new(array = list(a = 1, b = 1, c = 1))
inst$rmFirst(4)
inst$get()
inst <- Array$new(array = list(a = 1, b = 1, c = 1))
try(inst$rmFirst(4, strict = 1))
inst <- Array$new(array = list(a = 1, b = 1, c = 1))
try(inst$rmFirst(4, strict = 2))
inst <- Array$new(array = list(a = 1, b = 1, c = 1))
inst$rmFirst(4, simplify = TRUE)
inst <- Array$new(array = list(a = 1, b = 1, c = 1))
inst$rmFirst(3, simplify = TRUE)
inst$get()

inst$rmFirst()
inst$get()
```

### Remove last

```
inst <- Array$new(array = list(a = 1, b = 1, c = 1))
inst$rmLast()
inst$get()
inst$rmLast(2)
inst$get()

## Simplify //
inst <- Array$new(array = list(a = 1, b = 1, c = 1))
inst$rmLast(simplify = TRUE)
inst$get()
inst$rmLast(2, simplify = TRUE)
inst$get()

inst <- Array$new(array = list(a = 1, b = 1, c = 1))
inst$rmLast(4)
inst$get()
inst <- Array$new(array = list(a = 1, b = 1, c = 1))
try(inst$rmLast(4, strict = 1))
inst <- Array$new(array = list(a = 1, b = 1, c = 1))
try(inst$rmLast(4, strict = 2))

## Simplify //
inst <- Array$new(array = list(a = 1, b = 1, c = 1))
inst$rmLast(4, simplify = TRUE)
inst$get()

inst <- Array$new()
inst$rmLast()
try(inst$rmLast(strict = 1))
try(inst$rmLast(strict = 2))
inst$rmLast(2)
try(inst$rmLast(strict = 1))
try(inst$rmLast(strict = 2))
inst$get()

## Simplify //
inst <- Array$new()
inst$rmLast(simplify = TRUE)
try(inst$rmLast(strict = 1, simplify = TRUE))
inst$rmLast(2, simplify = TRUE)
try(inst$rmLast(strict = 1, simplify = TRUE))
```

### Copy 

```
inst <- Array$new(array = list(a = 1))
inst$copy("a", "b")
inst$get()

inst$copy("c", "d")
inst$get()
try(inst$copy("c", "d", strict = 1))
try(inst$copy("c", "d", strict = 2))

inst$copy(c("a", "b"), c("c", "d"))
inst$get()
inst$copy(c("a", "b"), c("c", "d"), dups = FALSE)
inst$get()
try(inst$copy(c("a", "b"), c("c", "d"), dups = FALSE, strict = 1))
try(inst$copy(c("a", "b"), c("c", "d"), dups = FALSE, strict = 2))
inst$copy(c("a", "b"), c("c", "d"))
inst$get()

inst$copy(c("a", "b"), "d")
try(inst$copy(c("a", "b"), "d", strict = 1))
try(inst$copy(c("a", "b"), "d", strict = 2))
```
