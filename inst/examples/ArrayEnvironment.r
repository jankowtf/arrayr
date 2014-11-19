\dontrun{

##------------------------------------------------------------------------------
## Initialize
##------------------------------------------------------------------------------

## Default //
inst <- ArrayEnvironment$new()
inst$get()
inst$get(list = TRUE)

## List/named //
inst <- ArrayEnvironment$new(list(a = 1, b = 2))
inst$get(list = TRUE)

inst <- ArrayEnvironment$new(list(a = 1), list(b = 2))
inst$get(list = TRUE)

inst <- ArrayEnvironment$new(a = list(a = 1, b = 2))
inst$get(list = TRUE)

inst <- ArrayEnvironment$new(list(a = 1, b = new.env()))
inst$get(list = TRUE)

## List/unnamed //
inst <- ArrayEnvironment$new(list(1, 2))
inst$get(list = TRUE)

inst <- ArrayEnvironment$new(list(1), list(2))
inst$get(list = TRUE)

inst <- ArrayEnvironment$new(list(1, new.env()))
inst$get(list = TRUE)

## List/mixed //
inst <- ArrayEnvironment$new(list(1), list(a = 2))
inst$get(list = TRUE)

inst <- ArrayEnvironment$new(list(1, 2, a = 3))
inst$get(list = TRUE)

inst <- ArrayEnvironment$new(list(list(a = 1)), list(b = 2))
inst$get(list = TRUE)

inst <- ArrayEnvironment$new(list(list(1, a = 1)))
inst$get(list = TRUE)

inst <- ArrayEnvironment$new(list(list(1, 2, a = 3)))
inst$get(list = TRUE)

## Explicit/atomic/named //
inst <- ArrayEnvironment$new(a = 1, b = 2)
inst$get(list = TRUE)

inst <- ArrayEnvironment$new(a = 1, b = new.env())
inst$get(list = TRUE)

inst <- ArrayEnvironment$new(a = 1, b = list(a = 1, b = 2))
inst$get(list = TRUE)

## Explicit/atomic/unnamed //
inst <- ArrayEnvironment$new(1, 2)
inst$get(list = TRUE)

inst <- ArrayEnvironment$new(1, new.env())
inst$get(list = TRUE)

## Explicit/mixed //
inst <- ArrayEnvironment$new(a = 1, 2)
inst$get(list = TRUE)

inst <- ArrayEnvironment$new(a = 1, list(b = 2), new.env())
inst$get(list = TRUE)

##------------------------------------------------------------------------------
## Add //
##------------------------------------------------------------------------------

## NOTE
## Method `add()` is "overwrite eager"; see method `set()` for an alternative
## that is "overwrite hesitant"

## List/single/named //
inst <- ArrayEnvironment$new()
inst$add(list(a = 1))
inst$get(list = TRUE)

inst$add(list(b = 2))
inst$get(list = TRUE)

inst$add(list(b = 3), overwrite = FALSE)
## --> overwrite blocked
inst$get(list = TRUE)
try(inst$add(list(b = 2), overwrite = FALSE, strict = 1))
inst$get(list = TRUE)
try(inst$add(list(b = 3), overwrite = FALSE, strict = 2))
inst$get(list = TRUE)

inst$add(list(b = 3))
## --> overwrite
inst$get(list = TRUE)

## List/single/unnamed //
inst <- ArrayEnvironment$new()
inst$add(list(1))
inst$get(list = TRUE)
inst$add(2)
inst$get(list = TRUE)
inst$add(2)
inst$get(list = TRUE)

inst$add(list("1" = 2), overwrite = FALSE)
## --> overwrite blocked
inst$get(list = TRUE)
try(inst$add(list("1" = 2), overwrite = FALSE, strict = 1))
inst$get(list = TRUE)
try(inst$add(list("1" = 2), overwrite = FALSE, strict = 2))
inst$get(list = TRUE)

inst$add(list("1" = 3))
## --> overwrite
inst$get(list = TRUE)

## List/multiple/named //
inst <- ArrayEnvironment$new()
all(inst$add(list(a = 1), list(b = 2))))
inst$get(list = TRUE)

## List/multiple/unnamed //
inst <- ArrayEnvironment$new()
inst$add(list(1), list(2))
inst$get(list = TRUE)

## Atomic/single/named //
inst <- ArrayEnvironment$new()
inst$add(a = new.env())
inst$get(list = TRUE)
inst$add(b = 1)
inst$get(list = TRUE)

## Atomic/single/unnamed //
inst <- ArrayEnvironment$new()
inst$add(1)
inst$get(list = TRUE)
inst$add(1)
inst$get(list = TRUE)

inst <- ArrayEnvironment$new()
inst$add(new.env())
inst$get(list = TRUE)

## Atomic/single/id //
inst <- ArrayEnvironment$new()
inst$add(1, id = "a")
inst$get(list = TRUE)
inst$add(1, id = "b")
inst$get(list = TRUE)

inst$add(10, id = "b", overwrite = FALSE)
inst$get(list = TRUE)
## --> overwrite blocked
inst$add(10, id = "b")
inst$get(list = TRUE)
## --> overwrite

## Wrong dimensions //
inst <- ArrayEnvironment$new()
inst$add(1, id = c("a", "b"))
try(inst$add(1, id = c("a", "b"), strict = 1))
try(inst$add(1, id = c("a", "b"), strict = 2))

## Atomic/multiple/named //
inst <- ArrayEnvironment$new()
inst$add(a = 1, b = 2)
inst$get(list = TRUE)

inst <- ArrayEnvironment$new()
inst$add(a = new.env(), b = new.env())
inst$get(list = TRUE)

## Atomic/multiple/unnamed //
inst <- ArrayEnvironment$new()
inst$add(1:3)
inst$get(list = TRUE)

inst <- ArrayEnvironment$new()
inst$add(envir, new.env())
inst$get(list = TRUE)

## Atomic/multiple/id //
inst <- ArrayEnvironment$new()
inst$add(1, 1, 1, id = c("c", "d", "e"))
inst$get(list = TRUE)

inst$add(1, 1, 1, id = c("e", "f"), overwrite = FALSE)
inst$get(list = TRUE)

inst <- ArrayEnvironment$new()
inst$add(1, 1, 1, id = c("a", "b"))
try(inst$add(1, 1, 1, id = c("a", "b"), strict = 1))
try(inst$add(1, 1, 1, id = c("a", "b"), strict = 2))

##------------------------------------------------------------------------------
## Set //
##------------------------------------------------------------------------------

## List/single //
inst <- ArrayEnvironment$new(list(a = 1))
inst$set(list(a = 2))
inst$get(list = TRUE)
inst$set(list(b = 2))
try(inst$set(list(b = 2), strict = 1))
try(inst$set(list(b = 2), strict = 2))
inst$get(list = TRUE)

## List/multiple //
inst <- ArrayEnvironment$new(list(a = 1, b = 1))
inst$set(list(a = 2), list(b = 2))
inst$get(list = TRUE)

inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$set(2, 2, 2, id = c("a", "b", "c"))
inst$get(list = TRUE)

## List/new //
inst <- ArrayEnvironment$new(list(a = 1))
inst$set(list(b = 1), must_exist = FALSE)
inst$get(list = TRUE)
inst$set(list(c = 1, d = 1), must_exist = FALSE)

## Atomic/single/named //
inst <- ArrayEnvironment$new(a = 1)
inst$set(a = 10)
inst$get(list = TRUE)

inst <- ArrayEnvironment$new(a = 1)
inst$set(b = 1, must_exist = FALSE)
inst$get(list = TRUE)

## Atomic/single/unnamed //
inst <- ArrayEnvironment$new(1)
inst$set(10)
inst$get(list = TRUE)

## Atomic/multiple/named //
inst <- ArrayEnvironment$new(a = 1, b = 1)
inst$set(a = 10, b = 10)
inst$get(list = TRUE)

inst <- ArrayEnvironment$new(a = 1, b = 1)
inst$set(a = 10, b = 10, c = 1, must_exist = FALSE)
inst$get(list = TRUE)

## Atomic/id //
inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$set(2, 2, 2, id = c("a", "b", "d"))
inst$get(list = TRUE)

## Not yet existing //
try(inst$set(2, 2, 2, id = c("a", "b", "d"), strict = 1))
try(inst$set(2, 2, 2, id = c("a", "b", "d"), strict = 2))

## Different lengths //
inst$set(2, 2, 2, id = c("b", "c")))
try(inst$set(2, 2, 2, id = c("b", "c"), strict = 1))
try(inst$set(2, 2, 2, id = c("b", "c"), strict = 2))

##------------------------------------------------------------------------------
## Get //
##------------------------------------------------------------------------------

## All //
inst <- ArrayEnvironment$new()
inst$get()
inst$get(list = TRUE)

inst <- ArrayEnvironment$new(list(a = 1, b = 1))
inst$get(list = TRUE)

## Character index //
inst <- ArrayEnvironment$new(list(a = 1, b = 1))
inst$get("a")
inst$get("a", inner = FALSE)
inst$get(c("a", "b"))
inst$get("a", "b")
inst$get(c("a", "b"), inner = FALSE)
inst$get("a", "b", inner = FALSE)

inst$get("c")
inst$get("c", inner = FALSE)
inst$get("c", strict = 1)
inst$get("c", strict = 2)

inst$get(c("a", "c"))
inst$get(c("a", "c"), inner = FALSE)

## Order //
inst <- ArrayEnvironment$new(
  list(a = 1, b = 1, "1" = 1, "10" = 1, "2" = 1, "20" = 1, .a = 1)
)
as.list(inst$.array, all.names = TRUE)
## --> note how the default order of `as.list(<environment>)` is very 
## different from what you might expect when looking at 
## `ls(<environment>, all.names = TRUE)`
ls(inst$.array, all.names = TRUE)
## --> I decided to rather use the order returned by `ls()` when `sorted = FALSE`:
inst$get(list = TRUE, all_names = TRUE, sorted = FALSE)
## As for an array management context a sorted arrangement makes most sense,
## the default of `sorted` is `TRUE`
inst$get(list = TRUE)
## --> numbers before characters, hidden before non-hidden

## Numerical index //
## Numerical indexes are treated as **position** indexes unless `char = TRUE`
inst <- ArrayEnvironment$new(
  list(a = 1, b = 1, "1" = 1, "10" = 1, "2" = 1, "20" = 1, .a = 1)
)
inst$get(list = TRUE)
inst$get(1:5)
inst$get(1:2, char = TRUE)
## --> elements with character names `"1"` and `"2"`
inst$get(1:3, char = TRUE)
## --> no element with character name `"3"`

##------------------------------------------------------------------------------
## Exists //
##------------------------------------------------------------------------------

## Character index //
inst <- ArrayEnvironment$new(list(a = 1, b = 1))
inst$exists("a")
inst$exists(c("a", "b"))
inst$exists("a", "b")
inst$exists("c")

## Numeric index //
## Interpreted as position index
inst <- ArrayEnvironment$new(list(a = 1, b = 1))
inst$exists(1)
inst$exists(c(1, 2))
inst$exists(1, 2)
inst$exists(3)

inst <- ArrayEnvironment$new(list("1" = 1, b = 1))
inst$exists(1, 2, char = TRUE)

##------------------------------------------------------------------------------
## Index //
##------------------------------------------------------------------------------

inst <- ArrayEnvironment$new(list(a = 1, b = 1, "1" = 1))
inst$index("a")
inst$index(c("a", "b"))
inst$index("a", "b")
inst$index("c")
try(inst$index("c", strict = 1))
try(inst$index("c", strict = 2))
inst$index(c("a", "c"))

inst$index("c", simplify = TRUE)
try(inst$index("c", simplify = TRUE, strict = 1))
try(inst$index("c", simplify = TRUE, strict = 2))
inst$index(c("a", "c"), simplify = TRUE)

##------------------------------------------------------------------------------
## Clear //
##------------------------------------------------------------------------------

inst <- ArrayEnvironment$new(list(a = 1, b = 1))
inst$get(list = TRUE)
inst$clear()
inst$get(list = TRUE)

##------------------------------------------------------------------------------
## Remove //
##------------------------------------------------------------------------------

inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
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

inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rm("a", "c")
inst$get(list = TRUE)

## Numerical index //
## Sorted:
inst <- ArrayEnvironment$new(list(a = 1, b = 1, 
  "1" = 1, "10" = 1, "2" = 1, "20" = 1, .a = 1))
inst$get(list = TRUE)
inst$rm(id = 1:2)
inst$get(list = TRUE)
## --> first two elements of **ordered** array have been removed

## Unsorted:
inst <- ArrayEnvironment$new(list(a = 1, b = 1, 
  "1" = 1, "10" = 1, "2" = 1, "20" = 1, .a = 1))
inst$get(list = TRUE, sorted = FALSE, all_names = TRUE)
inst$rm(id = 1:3, sorted = FALSE, all_names = TRUE)
inst$get(list = TRUE, sorted = FALSE, all_names = TRUE)

inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rm(1, 3)
inst$get(list = TRUE)

##############################
## Numeric keys/auto-adjust ##
##############################

inst <- ArrayEnvironment$new("1" = 1, a = 1, "2" = 2)
inst$rm(1:2)
inst$get(list = TRUE)

inst <- ArrayEnvironment$new("1" = 1, a = 1, "2" = 2, 
  b = 1, "3" = 3, "4" = 4, "5" = 5)
inst$rm(2, 4)
inst$get(list = TRUE)

## Numonly
inst <- ArrayEnvironment$new("1" = 1, a = 1, "2" = 2, 
  b = 1, "3" = 3, "4" = 4, "5" = 5)
inst$rm(2, 6)
inst$get(list = TRUE)
## --> 6th position was `a` --> not removed due to `numonly = TRUE`

inst <- ArrayEnvironment$new("1" = 1, a = 1, "2" = 2, 
  b = 1, "3" = 3, "4" = 4, "5" = 5)
inst$rm(2, 6, numonly = FALSE)
inst$get(list = TRUE)
## --> 6th position was `a` --> not removed due to `numonly = FALSE`

inst <- ArrayEnvironment$new("1" = 1, a = 1, "2" = 2, 
  b = 1, "3" = 3, "4" = 4, "5" = 5)
inst$rm(1:5)
inst$get(list = TRUE)

inst <- ArrayEnvironment$new("1" = 1, a = 1, "2" = 2, 
  b = 1, "3" = 3, "4" = 4, "5" = 5)
inst$rm(1:6)
inst$get(list = TRUE)

inst <- ArrayEnvironment$new("1" = 1, a = 1, "2" = 2, 
  b = 1, "3" = 3, "4" = 4, "5" = 5)
inst$rm(1:6, numonly = FALSE)
inst$get(list = TRUE)
  
##------------------------------------------------------------------------------
## Remove first //
##------------------------------------------------------------------------------

inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rmFirst()
inst$get(list = TRUE)
inst$rmFirst(2)
inst$get(list = TRUE)

inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rmFirst(4)
inst$get(list = TRUE)
inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
try(inst$rmFirst(4, strict = 1))
inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
try(inst$rmFirst(4, strict = 2))
inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rmFirst(4, simplify = TRUE)
inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rmFirst(3, simplify = TRUE)
inst$get(list = TRUE)

inst$rmFirst()
inst$get(list = TRUE)
  
##------------------------------------------------------------------------------
## Remove last //
##------------------------------------------------------------------------------

inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rmLast()
inst$get(list = TRUE)
inst$rmLast(2)
inst$get(list = TRUE)

## Simplify //
inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rmLast(simplify = TRUE)
inst$get(list = TRUE)
inst$rmLast(2, simplify = TRUE)
inst$get(list = TRUE)

inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rmLast(4)
inst$get(list = TRUE)
inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
try(inst$rmLast(4, strict = 1))
inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
try(inst$rmLast(4, strict = 2))

## Simplify //
inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rmLast(4, simplify = TRUE)
inst$get(list = TRUE)

inst <- ArrayEnvironment$new()
inst$rmLast()
try(inst$rmLast(strict = 1))
try(inst$rmLast(strict = 2))
inst$rmLast(2)
try(inst$rmLast(strict = 1))
try(inst$rmLast(strict = 2))
inst$get(list = TRUE)

## Simplify //
inst <- ArrayEnvironment$new()
inst$rmLast(simplify = TRUE)
try(inst$rmLast(strict = 1, simplify = TRUE))
inst$rmLast(2, simplify = TRUE)
try(inst$rmLast(strict = 1, simplify = TRUE))

##------------------------------------------------------------------------------
## Copy //
##------------------------------------------------------------------------------

## Single/character //
inst <- ArrayEnvironment$new(list(a = 1))
inst$copy("a", "b")
inst$get(list = TRUE)

inst$copy("c", "d")
inst$get(list = TRUE)
try(inst$copy("c", "d", strict = 1))
try(inst$copy("c", "d", strict = 2))

## Single/numeric //
inst <- ArrayEnvironment$new(list(a = 1))
inst$copy(1, 2)
inst$get(list = TRUE)

inst$copy(3, 4)
inst$get(list = TRUE)
try(inst$copy(3, 4, strict = 1))
try(inst$copy(3, 4, strict = 2))

## Multiple/character //
inst <- ArrayEnvironment$new(list(a = 1, b = 1))
inst$copy(c("a", "b"), c("c", "d"))
inst$get(list = TRUE)
inst$copy(c("a", "b"), c("c", "d"))
inst$get(list = TRUE)
try(inst$copy(c("a", "b"), c("c", "d"), strict = 1))
try(inst$copy(c("a", "b"), c("c", "d"), strict = 2))
inst$copy(c("a", "b"), c("c", "d"), overwrite = TRUE)
inst$get(list = TRUE)

all(inst$copy(c("a", "b"), "d")))  
try(inst$copy(c("a", "b"), "d", strict = 1))
try(inst$copy(c("a", "b"), "d", strict = 2))

## Multiple/numeric //
inst <- ArrayEnvironment$new(list(a = 1, b = 1))
inst$copy(c(1, 2), c(3, 4))
inst$get(list = TRUE)
inst$copy(c(1, 2), c(3, 4))
inst$get(list = TRUE)
try(inst$copy(c(1, 2), c(3, 4), strict = 1))
try(inst$copy(c(1, 2), c(3, 4), strict = 2))
inst$copy(c(1, 2), c(3, 4), overwrite = TRUE)
inst$get(list = TRUE)

inst$copy(c(1, 2), 4)
try(inst$copy(c(1, 2), 4, strict = 1))
try(inst$copy(c(1, 2), 4, strict = 2))
  
}
