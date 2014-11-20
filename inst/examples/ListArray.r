\dontrun{

##------------------------------------------------------------------------------
## Initialize
##------------------------------------------------------------------------------

## Default //
inst <- ListArray$new()
inst$.array

## Explicit: list/named //
inst <- ListArray$new(list(a = 1, b = 2))
inst$.array
inst <- ListArray$new(list(c = 1), list(d = 2))
inst$.array

## Explicit: list/unnamed //
inst <- ListArray$new(list(1, 2))
inst$.array
inst <- ListArray$new(list(1), list(2))
inst$.array

## Explicit: list/mixed //
inst <- ListArray$new(list(1, a = 2))
inst$.array
inst <- ListArray$new(list(1), list(a = 2))
inst$.array

## Explicit: atomic/named //
inst <- ListArray$new(a = 1, b = 2)
inst$.array

## Explicit: atomic/unnamed //
inst <- ListArray$new(1, 2)
inst$.array

## Explicit: atomic/mixed //
inst <- ListArray$new(1, a = 2)
inst$.array

## Explicit: mixed //
inst <- ListArray$new(list(1), list(a = 1), 1, b = 1)
inst$.array

##------------------------------------------------------------------------------
## Add //
##------------------------------------------------------------------------------

##########
## List ##
##########

## Single: unnamed //
inst <- ListArray$new()
inst$add(list(1))
inst$.array
inst$add(list(2))
inst$.array
inst$add(list(2))
inst$.array

## Single: named //
inst <- ListArray$new()
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

## Multiple //
inst <- ListArray$new()
inst$add(list(a = 1, b = 2))
inst$.array
inst$add(list(a = 1), list(b = 2))
inst$.array
inst$add(list(1, 2))
inst$.array
inst$add(list(1), list(2))
inst$.array

############
## Atomic ##
############

## Unnamed //
inst <- ListArray$new()
inst$add(1)
inst$.array
inst$add(new.env())
inst$.array
inst$add(1, 2, 3)
inst$.array
inst$add(1:3)
inst$.array

## Named //
inst <- ListArray$new()
inst$add(a = 1)
inst$.array
inst$add(b = 1)
inst$.array
inst$add(a = 1, b = 1, c = 1)
inst$.array

##############
## With IDs ##
##############

inst <- ListArray$new()
inst$add(1, 1, 1, id = c("a", "b", "c"))
inst$.array

inst$add(1, 1, 1, id = 5:7)
inst$.array
## --> note how position 4 is `NULL`; numeric `id` is 
## interpeted as position index

inst$add(1, 1, 1, id = c("c", "d"), dups = FALSE)
inst$.array
## --> duplicates detected

inst$add(1, 1, 1, id = c("a", "b"))
## --> wrong dimensions
try(inst$add(1, 1, 1, id = c("a", "b"), strict = 1))
try(inst$add(1, 1, 1, id = c("a", "b"), strict = 2))

##------------------------------------------------------------------------------
## Set //
##------------------------------------------------------------------------------

## Single //
inst <- ListArray$new(list(a = 1))
inst$set(list(a = 2))
inst$.array
inst$set(list(b = 2))
try(inst$set(list(b = 2), strict = 1))
try(inst$set(list(b = 2), strict = 2))
inst$.array

## Multiple //
inst <- ListArray$new(list(a = 1, b = 1))
inst$set(list(a = 2), list(b = 2))
inst$.array

inst <- ListArray$new(list(a = 1, b = 1, c = 1))
inst$set(2, 2, 2, id = c("a", "b", "c"))
inst$.array

inst$set(2, 2, 2, id = c("a", "b", "d"))
inst$.array
try(inst$set(2, 2, 2, id = c("a", "b", "d"), strict = 1))
try(inst$set(2, 2, 2, id = c("a", "b", "d"), strict = 2))

## Different lengths //
inst$set(2, 2, 2, id = c("b", "c"))
try(inst$set(2, 2, 2, id = c("b", "c"), strict = 1))
try(inst$set(2, 2, 2, id = c("b", "c"), strict = 2))

## New (or does not have to exist) //
inst <- ListArray$new(list(a = 1))
inst$set(list(b = 1), must_exist = FALSE)
inst$.array
inst$set(list(c = 1, d = 1), must_exist = FALSE)
inst$.array

##------------------------------------------------------------------------------
## Get //
##------------------------------------------------------------------------------

inst <- ListArray$new()
inst$get()

inst <- ListArray$new(array = list(a = 1, b = 1))
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

inst <- ListArray$new(list(a = 1))
inst$get("b")
try(inst$get("b", strict = 1))
try(inst$get("b", strict = 2))
inst$get(c("a", "b"))
try(inst$get(c("a", "b"), strict = 1))
try(inst$get(c("a", "b"), strict = 2))

##------------------------------------------------------------------------------
## Exists //
##------------------------------------------------------------------------------

inst <- ListArray$new(array = list(a = 1, b = 1))
inst$exists("a")
inst$exists(c("a", "b"))
inst$exists("c")

##------------------------------------------------------------------------------
## Index //
##------------------------------------------------------------------------------

inst <- ListArray$new(array = list(a = 1, b = 1))
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

##------------------------------------------------------------------------------
## Clear //
##------------------------------------------------------------------------------

inst <- ListArray$new(array = list(a = 1, b = 1))
inst$clear()
inst$.array

##------------------------------------------------------------------------------
## Remove //
##------------------------------------------------------------------------------

inst <- ListArray$new(array = list(a = 1, b = 1, c = 1))
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

##------------------------------------------------------------------------------
## Remove first //
##------------------------------------------------------------------------------

inst <- ListArray$new(array = list(a = 1, b = 1, c = 1))
inst$rmFirst()
inst$get()
inst$rmFirst(2)
inst$get()

inst <- ListArray$new(array = list(a = 1, b = 1, c = 1))
inst$rmFirst(4)
inst$get()
inst <- ListArray$new(array = list(a = 1, b = 1, c = 1))
try(inst$rmFirst(4, strict = 1))
inst <- ListArray$new(array = list(a = 1, b = 1, c = 1))
try(inst$rmFirst(4, strict = 2))
inst <- ListArray$new(array = list(a = 1, b = 1, c = 1))
inst$rmFirst(4, simplify = TRUE)
inst <- ListArray$new(array = list(a = 1, b = 1, c = 1))
inst$rmFirst(3, simplify = TRUE)
inst$get()

inst$rmFirst()
inst$get()
  
##------------------------------------------------------------------------------
## Remove last //
##------------------------------------------------------------------------------

inst <- ListArray$new(array = list(a = 1, b = 1, c = 1))
inst$rmLast()
inst$get()
inst$rmLast(2)
inst$get()

## Simplify //
inst <- ListArray$new(array = list(a = 1, b = 1, c = 1))
inst$rmLast(simplify = TRUE)
inst$get()
inst$rmLast(2, simplify = TRUE)
inst$get()

inst <- ListArray$new(array = list(a = 1, b = 1, c = 1))
inst$rmLast(4)
inst$get()
inst <- ListArray$new(array = list(a = 1, b = 1, c = 1))
try(inst$rmLast(4, strict = 1))
inst <- ListArray$new(array = list(a = 1, b = 1, c = 1))
try(inst$rmLast(4, strict = 2))

## Simplify //
inst <- ListArray$new(array = list(a = 1, b = 1, c = 1))
inst$rmLast(4, simplify = TRUE)
inst$get()

inst <- ListArray$new()
inst$rmLast()
try(inst$rmLast(strict = 1))
try(inst$rmLast(strict = 2))
inst$rmLast(2)
try(inst$rmLast(strict = 1))
try(inst$rmLast(strict = 2))
inst$get()

## Simplify //
inst <- ListArray$new()
inst$rmLast(simplify = TRUE)
try(inst$rmLast(strict = 1, simplify = TRUE))
inst$rmLast(2, simplify = TRUE)
try(inst$rmLast(strict = 1, simplify = TRUE))

##------------------------------------------------------------------------------
## Copy //
##------------------------------------------------------------------------------

inst <- ListArray$new(array = list(a = 1))
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

}
