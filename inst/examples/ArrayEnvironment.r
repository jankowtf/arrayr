\dontrun{

##------------------------------------------------------------------------------
## Initialize
##------------------------------------------------------------------------------

## Default //
inst <- ArrayEnvironment$new()
inst$.array
inst$get()

as.list(inst$.array)
inst$get(as_list = TRUE)

## Explicit //
inst <- ArrayEnvironment$new(list(a = 1, b = 2))
inst <- ArrayEnvironment$new(array = list(a = 1, b = 2))
inst$get(as_list = TRUE)
inst <- ArrayEnvironment$new(array = list(a = 1, b = 2))
inst$get(as_list = TRUE)
inst <- ArrayEnvironment$new(list(c = 1), list(d = 2))
inst$get(as_list = TRUE)

##------------------------------------------------------------------------------
## Add //
##------------------------------------------------------------------------------

## Single //
inst <- ArrayEnvironment$new()
inst$add(list(a = 1))
inst$get(as_list = TRUE)
inst$add(list(b = 2))
inst$get(as_list = TRUE)

inst$add(list(b = 2), dups = FALSE)
inst$get(as_list = TRUE)
try(inst$add(list(b = 2), dups = FALSE, strict = 1))
inst$get(as_list = TRUE)
try(inst$add(list(b = 2), dups = FALSE, strict = 2))
inst$get(as_list = TRUE)

inst$add(list(b = 2))
inst$get(as_list = TRUE)
## --> note that the implementation of `add()` differs from that of 
## class `Array`

## Multiple //
inst <- ArrayEnvironment$new()
inst$add(list(a = 1), list(b = 2))
inst$get(as_list = TRUE)

inst$add(1, 1, 1, id = c("c", "d", "e"))
inst$get(as_list = TRUE)

inst$add(1, 1, 1, id = c("e", "f"), dups = FALSE)
inst$get(as_list = TRUE)

inst <- ArrayEnvironment$new()
inst$add(1, 1, 1, id = c("a", "b"))
try(inst$add(1, 1, 1, id = c("a", "b"), strict = 1))
try(inst$add(1, 1, 1, id = c("a", "b"), strict = 2))

##------------------------------------------------------------------------------
## Set //
##------------------------------------------------------------------------------

## Single //
inst <- ArrayEnvironment$new(list(a = 1))
inst$set(list(a = 2))
inst$get(as_list = TRUE)
inst$set(list(b = 2))
try(inst$set(list(b = 2), strict = 1))
try(inst$set(list(b = 2), strict = 2))
inst$get(as_list = TRUE)

## Multiple //
inst <- ArrayEnvironment$new(list(a = 1, b = 1))
inst$set(list(a = 2), list(b = 2))
inst$get(as_list = TRUE)

inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$set(2, 2, 2, id = c("a", "b", "c"))
inst$get(as_list = TRUE)

inst$set(3, 3, 3, id = c("a", "b", "d"))
inst$get(as_list = TRUE)
try(inst$set(2, 2, 2, id = c("a", "b", "d"), strict = 1))
try(inst$set(2, 2, 2, id = c("a", "b", "d"), strict = 2))

## Different lengths //
inst$set(2, 2, 2, id = c("b", "c"))
try(inst$set(2, 2, 2, id = c("b", "c"), strict = 1))
try(inst$set(2, 2, 2, id = c("b", "c"), strict = 2))

## New (or does not have to exist) //
inst <- ArrayEnvironment$new(list(a = 1))
inst$set(list(b = 1), must_exist = FALSE)
inst$get(as_list = TRUE)
inst$set(list(c = 1, d = 1), must_exist = FALSE)
inst$.array

##------------------------------------------------------------------------------
## Get //
##------------------------------------------------------------------------------

inst <- ArrayEnvironment$new()
inst$get()
inst$get(as_list = TRUE)

inst <- ArrayEnvironment$new(list(a = 1, b = 1))
inst$get()
inst$get(as_list = TRUE)

inst$get("a")
inst$get("a", inner = FALSE)
inst$get(c("a", "b"))
inst$get(c("a", "b"), inner = FALSE)
inst$get("c")
inst$get("c", inner = FALSE)
inst$get(c("a", "c"))
inst$get(c("a", "c"), inner = FALSE)

inst <- ArrayEnvironment$new(list(a = 1))
inst$get("b")
try(inst$get("b", strict = 1))
try(inst$get("b", strict = 2))
inst$get(c("a", "b"))
try(inst$get(c("a", "b"), strict = 1))
try(inst$get(c("a", "b"), strict = 2))

##------------------------------------------------------------------------------
## Exists //
##------------------------------------------------------------------------------

inst <- ArrayEnvironment$new(list(a = 1, b = 1))
inst$exists("a")
inst$exists(c("a", "b"))
inst$exists("c")

##------------------------------------------------------------------------------
## Index //
##------------------------------------------------------------------------------

inst <- ArrayEnvironment$new(list(a = 1, b = 1, "1" = 1))
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

inst <- ArrayEnvironment$new(list(a = 1, b = 1))
inst$get(as_list = TRUE)
inst$clear()
inst$get(as_list = TRUE)

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

##------------------------------------------------------------------------------
## Remove first //
##------------------------------------------------------------------------------

inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rmFirst()
inst$get(as_list = TRUE)
inst$rmFirst(2)
inst$get(as_list = TRUE)

inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rmFirst(4)
inst$get(as_list = TRUE)
inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
try(inst$rmFirst(4, strict = 1))
inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
try(inst$rmFirst(4, strict = 2))
inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rmFirst(4, simplify = TRUE)
inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rmFirst(3, simplify = TRUE)
inst$get(as_list = TRUE)

inst$rmFirst()
inst$get(as_list = TRUE)
  
##------------------------------------------------------------------------------
## Remove last //
##------------------------------------------------------------------------------

inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rmLast()
inst$get(as_list = TRUE)
inst$rmLast(2)
inst$get(as_list = TRUE)

## Simplify //
inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rmLast(simplify = TRUE)
inst$get(as_list = TRUE)
inst$rmLast(2, simplify = TRUE)
inst$get(as_list = TRUE)

inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rmLast(4)
inst$get(as_list = TRUE)
inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
try(inst$rmLast(4, strict = 1))
inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
try(inst$rmLast(4, strict = 2))

## Simplify //
inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1))
inst$rmLast(4, simplify = TRUE)
inst$get(as_list = TRUE)

inst <- ArrayEnvironment$new()
inst$rmLast()
try(inst$rmLast(strict = 1))
try(inst$rmLast(strict = 2))
inst$rmLast(2)
try(inst$rmLast(strict = 1))
try(inst$rmLast(strict = 2))
inst$get(as_list = TRUE)

## Simplify //
inst <- ArrayEnvironment$new()
inst$rmLast(simplify = TRUE)
try(inst$rmLast(strict = 1, simplify = TRUE))
inst$rmLast(2, simplify = TRUE)
try(inst$rmLast(strict = 1, simplify = TRUE))

##------------------------------------------------------------------------------
## Copy //
##------------------------------------------------------------------------------

inst <- ArrayEnvironment$new(list(a = 1))
inst$copy("a", "b")
inst$get(as_list = TRUE)

inst$copy("c", "d")
inst$get(as_list = TRUE)
try(inst$copy("c", "d", strict = 1))
try(inst$copy("c", "d", strict = 2))

## Multiple //
inst$copy(c("a", "b"), c("c", "d"))
inst$get(as_list = TRUE)
inst$copy(c("a", "b"), c("c", "d"), dups = FALSE)
inst$get(as_list = TRUE)
try(inst$copy(c("a", "b"), c("c", "d"), dups = FALSE, strict = 1))
try(inst$copy(c("a", "b"), c("c", "d"), dups = FALSE, strict = 2))
inst$copy(c("a", "b"), c("c", "d"))
inst$get(as_list = TRUE)

inst$copy(c("a", "b"), "d")
try(inst$copy(c("a", "b"), "d", strict = 1))
try(inst$copy(c("a", "b"), "d", strict = 2))

}
