\dontrun{

##------------------------------------------------------------------------------
## Initialize
##------------------------------------------------------------------------------

## Default //
inst <- Array$new()
inst$.array

## Explicit: list/named //
inst <- Array$new(list(a = 1, b = 2))
inst$.array
inst <- Array$new(list(c = 1), list(d = 2))
inst$.array

## Explicit: list/unnamed //
inst <- Array$new(list(1, 2))
inst$.array
inst <- Array$new(list(1), list(2))
inst$.array

## Explicit: list/mixed //
inst <- Array$new(list(1, a = 2))
inst$.array
inst <- Array$new(list(1), list(a = 2))
inst$.array

## Explicit: atomic/named //
inst <- Array$new(a = 1, b = 2)
inst$.array

## Explicit: atomic/unnamed //
inst <- Array$new(1, 2)
inst$.array

## Explicit: atomic/mixed //
inst <- Array$new(1, a = 2)
inst$.array

## Explicit: mixed //
inst <- Array$new(list(1), list(a = 1), 1, b = 1)
inst$.array

##------------------------------------------------------------------------------
## Add //
##------------------------------------------------------------------------------

##########
## List ##
##########

## Single: unnamed //
inst <- Array$new()
inst$add(list(1))
inst$.array
inst$add(list(2))
inst$.array
inst$add(list(2))
inst$.array

## Single: named //
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

## Multiple //
inst <- Array$new()
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
inst <- Array$new()
inst$add(1)
inst$.array
inst$add(new.env())
inst$.array
inst$add(1, 2, 3)
inst$.array
inst$add(1:3)
inst$.array

## Named //
inst <- Array$new()
inst$add(a = 1)
inst$.array
inst$add(b = 1)
inst$.array
inst$add(a = 1, b = 1, c = 1)
inst$.array

##############
## With IDs ##
##############

inst <- Array$new()
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

##########
## List ##
##########

## Single //
inst <- Array$new(list(a = 1))
inst$set(list(a = 2))
inst$.array
inst$set(list(b = 2))
try(inst$set(list(b = 2), strict = 1))
try(inst$set(list(b = 2), strict = 2))
inst$.array

## Multiple //
inst <- Array$new(list(a = 1, b = 1))
inst$set(list(a = 2), list(b = 2))
inst$.array

## New //
inst <- Array$new(list(a = 1))
inst$set(list(b = 1))
inst$.array
inst$set(list(b = 1), must_exist = FALSE)
inst$.array
inst$set(list(c = 1, d = 1), must_exist = FALSE)
inst$.array

## Unnamed //  
inst <- Array$new(list(a = 1))
inst$set(list(2))
try(inst$set(list(2), strict = 1))
try(inst$set(list(2), strict = 2))
inst$.array

############
## Atomic ##
############

## Single //
inst <- Array$new(a = 1)
inst$set(a = 2)
inst$.array
inst$set(b = 2)
try(inst$set(b = 2, strict = 1))
try(inst$set(b = 2, strict = 2))
inst$.array

## Multiple //
inst <- Array$new(a = 1, b = 1)
inst$set(a = 2, b = 2)
inst$.array

## New //
inst <- Array$new(a = 1)
inst$set(b = 1, must_exist = FALSE)
inst$.array
inst$set(c = 1, d = 1, must_exist = FALSE)
inst$.array

## Unnamed //  
inst <- Array$new(1)
inst$set(2)
try(inst$set(2, strict = 1))
try(inst$set(2, strict = 2))
inst$.array

##############
## With IDs ##
##############

## Single //
inst <- Array$new(list(a = 1))
inst$set(2, id = "a")
inst$.array

inst$set(1, id = c("a", "b"))
try(inst$set(1, id = c("a", "b"), strict = 1))
try(inst$set(1, id = c("a", "b"), strict = 2))
inst$.array

## Multiple //
inst <- Array$new(list(a = 1, b = 1, c = 1))
inst$set(2, 2, 2, id = c("a", "b", "c"))
inst$.array

inst$set(2, 2, 2, id = c("a", "b", "d"))
inst$.array
try(inst$set(2, 2, 2, id = c("a", "b", "d"), strict = 1))
try(inst$set(2, 2, 2, id = c("a", "b", "d"), strict = 2))
inst$.array

## Invalid //
inst <- Array$new(list(a = 1, b = 1))
inst$set(2, 2, 2, id = c("b", "c"))
try(inst$set(2, 2, 2, id = c("b", "c"), strict = 1))
try(inst$set(2, 2, 2, id = c("b", "c"), strict = 2))
  
##------------------------------------------------------------------------------
## Get //
##------------------------------------------------------------------------------

#########
## All ##
#########

inst <- Array$new()
inst$get()

inst <- Array$new(list(a = 1, "1" = 1, "10" = 1, b = 1, 
  "20" = 1, "2" = 1, .a = 1))
inst$get()
## --> order "as-is"

inst$get(sorting = 1)
## --> sorted (increasing, numbers before hidden before regular)
inst$get(sorting = 2)
## --> sorted (decreasing, regular before hidden before numbers)

##################
## Character ID ##
##################

## Single //
inst <- Array$new(list(a = 1, "1" = 1, "10" = 1, b = 1, 
  "20" = 1, "2" = 1, .a = 1))
inst$get("a")
inst$get("a", inner = FALSE)

inst$get("c")
inst$get("c", inner = FALSE)
try(inst$get("c", strict = 1))
try(inst$get("c", strict = 2))
inst$get("c", default = list())
inst$get("c", default = list(), inner = FALSE)

## Multiple //
inst <- Array$new(list(a = 1, "1" = 1, "10" = 1, b = 1, 
  "20" = 1, "2" = 1, .a = 1))
inst$get(c("a", "b"))
inst$get(c("a", "b"), inner = FALSE)
inst$get("a", "b")
inst$get(id = c("a", "b"))

inst$get(c("a", "c"))
inst$get(c("a", "c"), inner = FALSE)
inst$get(c("a", "c"), simplify = TRUE)
try(inst$get(c("a", "c"), strict = 1))
try(inst$get(c("a", "c"), strict = 2))
inst$get(c("a", "c"), default = list(NA, list()))
## --> first value of `default` is for element-wise values, 
## the second for the overall value in case the entire result set is empty
inst$get(c("a", "c"), default = list(NA, list()), inner = FALSE)

inst$get("c", "d")
inst$get("c", "d", simplify = TRUE)
inst$get("c", "d", simplify = TRUE, default = list(NA, "empty result set"))

## Sorting //
inst <- Array$new(list(a = 1, "1" = 1, "10" = 1, b = 1, 
  "20" = 1, "2" = 1, .a = 1))
inst$get("a", "2", "b")
inst$get(c("a", "2", "b"), sorting = 1)
inst$get("a", "2", "b", sorting = 2)

################
## Numeric ID ##
################

## Single / regular //
inst <- Array$new(list(a = 1, "1" = 2, "10" = 3, b = 4, "20" = 5, "2" = 6, .a = 7))
inst$get(2)
inst$get(2, inner = FALSE)

## Single / sorting //
inst <- Array$new(list(a = 1, "1" = 2, "10" = 3, b = 4, "20" = 5, "2" = 6, .a = 7))
inst$get(2, sorting = 1)
## --> note how the array has been sorted before the get operation was 
## carried out:
inst$.order()
inst$get(2, inner = FALSE, sorting = 1)

inst$get(2, sorting = 2)
## --> sorted in decreasing order, named before hidden-named before numeric:
inst$.order(decreasing = TRUE)
inst$get(2, inner = FALSE, sorting = 2)

## Single / non-existing //
inst <- Array$new(list(a = 1, b = 1))
inst$get(3)
inst$get(3, inner = FALSE)
try(inst$get("c", strict = 1))
try(inst$get("c", strict = 2))
inst$get(3, default = list(NA, list()))

## Multiple / regular //
inst <- Array$new(list(a = 1, "1" = 2, "10" = 3, b = 4, "20" = 5, "2" = 6, .a = 7))
inst$get(1, 2)
inst$get(1, 2, inner = FALSE)

inst$get(2, 1)
## --> note that order matters as long as `sorting = 0`
inst$get(2, 1, inner = FALSE)

## Multiple / sorted //
inst <- Array$new(list(a = 1, "1" = 2, "10" = 3, b = 4, "20" = 5, "2" = 6, .a = 7))
inst$get(1, 2, sorting = 1)
inst$get(1, 2, inner = FALSE, sorting = 1)

inst$get(2, 1, sorting = 1)
inst$get(2, 1, inner = FALSE, sorting = 1)

inst <- Array$new(list(a = 1, "1" = 2, "10" = 3, b = 4, "20" = 5, "2" = 6, .a = 7))
inst$get(1, 2, sorting = 2)
inst$get(1, 2, inner = FALSE, sorting = 2)

inst$get(2, 1, sorting = 2)
inst$get(2, 1, inner = FALSE, sorting = 2)

## Multiple / non-existing //
inst <- Array$new(list(a = 1, b = 1))
inst$get(1, 3)
inst$get(1, 3, inner = FALSE)
try(inst$get(1, 3, strict = 1))
try(inst$get(1, 3, strict = 2))
inst$get(1, 3, simplify = TRUE)

inst$get(3, 4)
inst$get(3, 4, simplify = TRUE)

##------------------------------------------------------------------------------
## Exists //
##------------------------------------------------------------------------------

## Character //
inst <- Array$new(list(a = 1, b = 1))
inst$exists("a")
inst$exists(c("a", "b"))
inst$exists("a", "b")
inst$exists("c")
inst$exists("a", "c")

## Numeric //
inst <- Array$new(list(a = 1, b = 1))
inst$exists(1)
inst$exists(c(1, 2))
inst$exists(1, 2)
inst$exists(3)
inst$exists(1, 3)

## Empty //
inst <- Array$new()
inst$exists()

##------------------------------------------------------------------------------
## Index //
##------------------------------------------------------------------------------

##################
## Character ID ##
##################
  
## Single //
inst <- Array$new(list(a = 1, b = 1))
inst$index("a")

## Single / non-existing //
inst <- Array$new(list(a = 1, b = 1))
inst$index("c")
try(inst$index("c", strict = 1))
try(inst$index("c", strict = 2))
inst$index("c", simplify = TRUE)

## Single / sorting //
inst <- Array$new(list(a = 1, "1" = 2, "10" = 1, b = 1, "20" = 1, "2" = 1, .a = 1))
inst$index("a", sorting = 0)
inst$index("a", sorting = 1)
inst$index("a", sorting = 2)

## Multiple //
inst <- Array$new(list(a = 1, b = 1))
inst$index(c("a", "b"))

## Multiple / non-existing //
inst <- Array$new(list(a = 1, b = 1))
inst$index(c("a", "c"))
inst$index(c("a", "c"), simplify = TRUE)

## Multiple / sorted //

inst <- Array$new(list(a = 1, "1" = 2, "10" = 1, b = 1, "20" = 1, "2" = 1, .a = 1))
inst$index("a", ".a", sorting = 0)
inst$index("a", ".a", sorting = 1)
inst$index("a", ".a", sorting = 2)

##------------------------------------------------------------------------------
## Clear //
##------------------------------------------------------------------------------

inst <- Array$new(array = list(a = 1, b = 1))
inst$clear()
inst$get()

##------------------------------------------------------------------------------
## Remove //
##------------------------------------------------------------------------------

##################
## Character ID ##
##################

## Single //
inst <- Array$new(list(a = 1, b = 1, c = 1))
inst$rm("a")
inst$exists("a")

## Single / fast //
inst <- Array$new(list(a = 1, b = 1, c = 1))
inst$rm("a", fast = TRUE)
## --> note that when `fast = TRUE` no TRUE/FALSE status information can be 
## given (thus `NA`)

## Single / non-existing //
inst <- Array$new(list())
inst$rm("a")
try(inst$rm("a", strict = 1))
try(inst$rm("a", strict = 2))

## Multiple //
inst <- Array$new(list(a = 1, b = 1, c = 1))
inst$rm("b", "c")
inst$rm("b", "c")

################
## Numeric ID ##
################

## Single //
inst <- Array$new(list(a = 1, b = 1, c = 1))
inst$rm(1)
inst$exists("a")
inst$rm(3)

## Single / fast //
inst <- Array$new(list(a = 1, b = 1, c = 1))
inst$rm(1, fast = TRUE)
inst$exists("a")
inst$rm(3, fast = TRUE)
inst$exists("c")

## Single / non-existing //
inst <- Array$new(list())
inst$rm(1)
try(inst$rm(1, strict = 1))
try(inst$rm(1, strict = 2))

## Multiple //
inst <- Array$new(list(a = 1, b = 1, c = 1))
inst$rm(1, 3)
inst$exists("a", "c")
inst$rm(1, 2)

## Multiple / fast //
inst <- Array$new(list(a = 1, b = 1, c = 1))
inst$rm(1, 2, fast = TRUE)
inst$exists("a", "b")

##------------------------------------------------------------------------------
## Remove first //
##------------------------------------------------------------------------------

## Default `n` //
inst <- Array$new(list(a = 1, b = 1, c = 1))
inst$rmFirst()
inst$get()

## Explicit `n` //  
inst <- Array$new(list(a = 1, b = 1, c = 1))
inst$rmFirst(3)
inst$get()

## Exceeds scope //
inst <- Array$new(list(a = 1, b = 1, c = 1))
inst$rmFirst(4)
inst <- Array$new(list(a = 1, b = 1, c = 1))
try(inst$rmFirst(4, strict = 1))
inst <- Array$new(list(a = 1, b = 1, c = 1))
try(inst$rmFirst(4, strict = 2))

## Simplify //
inst <- Array$new(list(a = 1, b = 1, c = 1))
inst$rmFirst(3, simplify = TRUE)
inst <- Array$new(list(a = 1, b = 1, c = 1))
inst$rmFirst(4, simplify = TRUE)
inst$get()
  
##------------------------------------------------------------------------------
## Remove last //
##------------------------------------------------------------------------------
  
## Default `n` //
inst <- Array$new(list(a = 1, b = 1, c = 1))
inst$rmLast()
inst$get()

## Explicit `n` //
inst <- Array$new(list(a = 1, b = 1, c = 1))
inst$rmLast(3)
inst$get()

## Exceeds scope //  
inst <- Array$new(list(a = 1, b = 1, c = 1))
inst$rmLast(4)
inst$get()

inst <- Array$new(list(a = 1, b = 1, c = 1))
try(inst$rmLast(4, strict = 1))
inst <- Array$new(list(a = 1, b = 1, c = 1))
try(inst$rmLast(4, strict = 2))

## Simplify //
inst <- Array$new(list(a = 1, b = 1, c = 1))
inst$rmLast(simplify = TRUE)
inst$rmLast(2, simplify = TRUE)
inst$rmLast(simplify = TRUE)

inst <- Array$new(list(a = 1, b = 1, c = 1))
try(inst$rmLast(strict = 1, simplify = TRUE))
inst <- Array$new(list(a = 1, b = 1, c = 1))
try(inst$rmLast(strict = 2, simplify = TRUE))

}
