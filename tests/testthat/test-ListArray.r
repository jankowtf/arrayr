##------------------------------------------------------------------------------
context("ListArray/initialize")
##------------------------------------------------------------------------------

test_that("ListArray/initialize/default", {
  
  expect_is(inst <- ListArray$new(), "ListArray")
  expect_identical(inst$.array, structure(list(), names = character()))
  
})

test_that("ListArray/initialize/explicit/list/named", {
  
  expect_is(inst <- ListArray$new(list(a = 1, b = 2)), "ListArray")
  expect_identical(inst$.array, list(a = 1, b = 2))
  expect_is(inst <- ListArray$new(list(a = 1), list(b = 2)), "ListArray")
  expect_identical(inst$.array, list(a = 1, b = 2))

})

test_that("ListArray/initialize/explicit/list/unnamed", {
  
  expect_is(inst <- ListArray$new(list(1, 2)), "ListArray")
  expect_equivalent(inst$.array, list(1, 2))
  expect_is(inst <- ListArray$new(list(1), list(2)), "ListArray")
  expect_equivalent(inst$.array, list(1, 2))

})

test_that("ListArray/initialize/explicit/list/mixed", {
  
  expect_is(inst <- ListArray$new(list(1, a = 2)), "ListArray")
  expect_equivalent(inst$.array, list(1, a = 2))
  expect_is(inst <- ListArray$new(list(1), list(a = 2)), "ListArray")
  expect_equivalent(inst$.array, list(1, a = 2))

})

test_that("ListArray/initialize/explicit/atomic/named", {
  
  expect_is(inst <- ListArray$new(a = 1, b = 2), "ListArray")
  expect_identical(inst$.array, list(a = 1, b = 2))
  
})

test_that("ListArray/initialize/explicit/atomic/unnamed", {
  
  expect_is(inst <- ListArray$new(1, 2), "ListArray")
  expect_equivalent(inst$.array, list(1, 2))
  
})

test_that("ListArray/initialize/explicit/atomic/mixed", {
  
  expect_is(inst <- ListArray$new(1, a = 2), "ListArray")
  expect_equivalent(inst$.array, list(1, a = 2))
  
})

test_that("ListArray/initialize/explicit/mixed", {
  
  expect_is(inst <- ListArray$new(list(1), list(a = 1), 1, b = 1), "ListArray")
  expect_identical(inst$.array, list(1, a = 1, 1, b = 1))
  
})

##------------------------------------------------------------------------------
context("ListArray/add")
##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
context("ListArray/add/list")
##------------------------------------------------------------------------------

test_that("ListArray/add/list/single/named", {
  
  inst <- ListArray$new()
  expect_true(inst$add(list(a = 1)))
  expect_identical(inst$.array, list(a = 1))
  expect_true(inst$add(list(b = 2)))
  expect_identical(inst$.array, list(a = 1, b = 2))
  
  expect_false(inst$add(list(b = 2), dups = FALSE))
  expect_identical(inst$.array, list(a = 1, b = 2))
  expect_warning(expect_false(inst$add(list(b = 2), dups = FALSE, strict = 1)))
  expect_identical(inst$.array, list(a = 1, b = 2))
  expect_error(inst$add(list(b = 2), dups = FALSE, strict = 2))
  expect_identical(inst$.array, list(a = 1, b = 2))
  
  expect_true(inst$add(list(b = 2)))
  expect_identical(inst$.array, list(a = 1, b = 2, b = 2))
  
  inst <- ListArray$new()
  value <- list(new.env())
  expect_true(inst$add(a = value))
  expect_identical(inst$.array, list(a = value))
  
  inst <- ListArray$new()
  value <- list(data.frame(1:3))
  expect_true(inst$add(a = value))
  expect_identical(inst$.array, list(a = value))
  
  inst <- ListArray$new()
  value <- list(list(1:3))
  expect_true(inst$add(b = value))
  expect_identical(inst$.array, list(b = value))
  
})

test_that("ListArray/add/list/single/unnamed", {
  
  inst <- ListArray$new()
  expect_true(inst$add(list(1)))
  expect_equivalent(inst$.array, list(1))
  expect_true(inst$add(list(2)))
  expect_equivalent(inst$.array, list(1, 2))
  
  expect_true(inst$add(list(2)))
  expect_equivalent(inst$.array, list(1, 2, 2))
  
  inst <- ListArray$new()
  value <- list(new.env())
  expect_true(inst$add(value))
  expect_equivalent(inst$.array, value)
  
  inst <- ListArray$new()
  value <- list(data.frame(1:3))
  expect_true(inst$add(value))
  expect_equivalent(inst$.array, value)
  
  inst <- ListArray$new()
  value <- list(list(1:3))
  expect_true(inst$add(value))
  expect_equivalent(inst$.array, value[[1]])
  
})

test_that("ListArray/add/list/multiple", {
  
  inst <- ListArray$new()
  expect_true(all(inst$add(list(a = 1), list(b = 2))))
  expect_identical(inst$.array, list(a = 1, b = 2))
  
})

test_that("ListArray/add/list/multiple/id/character", {
  
  inst <- ListArray$new()
  expect_true(all(inst$add(list(1), list(1), list(1), id = letters[1:3])))
  expect_identical(inst$.array, list(a = list(1), b = list(1), c = list(1)))
  
  expect_false(all(inst$add(list(1), list(1), list(1), id = c("e", "f"), 
    dups = FALSE)))
  expect_identical(inst$.array, list(a = list(1), b = list(1), c = list(1)))
  
  inst <- ListArray$new()
  expect_false(inst$add(list(1), list(1), list(1), id = c("a", "b")))
  expect_warning(expect_false(inst$add(list(1), list(1), list(1), 
    id = c("a", "b"), strict = 1)))
  expect_error(inst$add(list(1), list(1), list(1), id = c("a", "b"), strict = 2))
  
})

test_that("ListArray/add/list/multiple/id/numeric", {
  
  inst <- ListArray$new()
  expect_true(all(inst$add(list(1), list(1), list(1), id = 2:4)))
  expect_equivalent(inst$.array, list(NULL, 1, 1, 1))
  
})

##------------------------------------------------------------------------------
context("ListArray/add/atomic")
##------------------------------------------------------------------------------

test_that("ListArray/add/atomic/single/named", {
  
  inst <- ListArray$new()
  expect_true(inst$add(a = 1))
  expect_equivalent(inst$.array, list(a = 1))
  
  envir <- new.env()
  inst <- ListArray$new()
  expect_true(inst$add(a = envir))
  expect_equivalent(inst$.array, list(a = envir))
  
  df <- data.frame(1:3)
  inst <- ListArray$new()
  expect_true(inst$add(a = df))
  expect_equivalent(inst$.array, list(a = df))
  
  lst <- list(list(1:3))
  inst <- ListArray$new()
  expect_true(inst$add(a = lst))
  expect_equivalent(inst$.array, list(a = lst[[1]]))
  
})

test_that("ListArray/add/atomic/single/unnamed", {
  
  inst <- ListArray$new()
  expect_true(inst$add(1))
  expect_equivalent(inst$.array, list(1))
  
  envir <- new.env()
  inst <- ListArray$new()
  expect_true(inst$add(envir))
  expect_equivalent(inst$.array, list(envir))
  
  df <- data.frame(1:3)
  inst <- ListArray$new()
  expect_true(inst$add(df))
  expect_equivalent(inst$.array, list(df))
  
  lst <- list(list(1:3))
  inst <- ListArray$new()
  expect_true(inst$add(lst))
  expect_equivalent(inst$.array, lst[[1]])
  
})

test_that("ListArray/add/atomic/single/id/character", {
  
  inst <- ListArray$new()
  expect_true(inst$add(1, id = "a"))
  expect_equivalent(inst$.array, list(a = 1))
  
  inst <- ListArray$new()
  expect_true(inst$add(1, id = "1"))
  expect_equivalent(inst$.array, list("1" = 1))
  
  envir <- new.env()
  inst <- ListArray$new()
  expect_true(inst$add(envir, id = "a"))
  expect_equivalent(inst$.array, list(a = envir))
  
  df <- data.frame(1:3)
  inst <- ListArray$new()
  expect_true(inst$add(df, id = "a"))
  expect_equivalent(inst$.array, list(a = df))
  
  lst <- list(list(1:3))
  inst <- ListArray$new()
  expect_true(inst$add(lst, id = "a"))
  expect_equivalent(inst$.array, list(a = list(lst[[1]])))
  
})

test_that("ListArray/add/atomic/single/id/numeric", {
  
  inst <- ListArray$new()
  expect_true(inst$add(1, id = 1))
  expect_equivalent(inst$.array, list(1))
  
  inst <- ListArray$new()
  expect_true(inst$add(1, id = 2))
  expect_equivalent(inst$.array, list(NULL, 1))
  
  envir <- new.env()
  inst <- ListArray$new()
  expect_true(inst$add(envir, id = 2))
  expect_equivalent(inst$.array, list(NULL, envir))
  
  df <- data.frame(1:3)
  inst <- ListArray$new()
  expect_true(inst$add(df, id = 2))
  expect_equivalent(inst$.array, list(NULL, df))
  
  lst <- list(list(1:3))
  inst <- ListArray$new()
  expect_true(inst$add(lst, id = 2))
  expect_equivalent(inst$.array, list(NULL, lst[[1]]))
  
})

test_that("ListArray/add/atomic/single/mixed", {
  
  envir <- new.env()
  df <- data.frame(1:3)
  lst <- list(list(1:3))
  
  inst <- ListArray$new()
  expect_true(all(inst$add(1, a = 1, envir, b = envir, 
    df, c = df, lst, d = lst)))
  expect_equivalent(inst$.array, 
    list(1, a = 1, envir, b = envir, df, c = df, lst[[1]], d = lst))
  
})

test_that("ListArray/add/atomic/multiple", {
  
  inst <- ListArray$new()
  expect_true(all(inst$add(a = 1, b = 2, c = 3)))
  expect_equivalent(inst$.array, list(a = 1, b = 2, c = 3))
  
  inst <- ListArray$new()
  expect_true(all(inst$add(1, 2, 3)))
  expect_equivalent(inst$.array, list(1, 2, 3))
  
})

test_that("ListArray/add/atomic/multiple/id/character", {
  
  inst <- ListArray$new()
  expect_true(all(inst$add(1, 2, 3, id = letters[1:3])))
  expect_equivalent(inst$.array, list(a = 1, b = 2, c = 3))
  
})

test_that("ListArray/add/atomic/multiple/id/numeric", {
  
  inst <- ListArray$new()
  expect_true(all(inst$add(1, 2, 3, id = 2:4)))
  expect_equivalent(inst$.array, list(NULL, 1, 2, 3))
  
})

##------------------------------------------------------------------------------
context("ListArray/set")
##------------------------------------------------------------------------------

test_that("ListArray/set/single", {
  
  expect_is(inst <- ListArray$new(list(a = 1)), "ListArray")
  expect_true(inst$set(list(a = 2)))
  expect_identical(inst$.array, list(a = 2))
  expect_false(inst$set(list(b = 2)))
  expect_warning(expect_false(inst$set(list(b = 2), strict = 1)))
  expect_error(inst$set(list(b = 2), strict = 2))
  expect_identical(inst$.array, list(a = 2))
  
})

test_that("ListArray/set/multiple", {
  
  expect_is(inst <- ListArray$new(list(a = 1, b = 1)), "ListArray")
  expect_identical(inst$set(list(a = 2), list(b = 2)), c(a = TRUE, b = TRUE))
  expect_identical(inst$.array, list(a = 2, b = 2))
  
  expect_is(inst <- ListArray$new(list(a = 1, b = 1, c = 1)), "ListArray")
  expect_true(all(inst$set(2, 2, 2, id = c("a", "b", "c"))))
  expect_identical(inst$.array, list(a = 2, b = 2, c = 2))
  
  expect_identical(inst$set(2, 2, 2, id = c("a", "b", "d")), 
    c(a = TRUE, b = TRUE, d = FALSE))
  expect_identical(inst$.array, list(a = 2, b = 2, c = 2))
  expect_warning(expect_identical(inst$set(2, 2, 2, id = c("a", "b", "d"), strict = 1), 
    c(a = TRUE, b = TRUE, d = FALSE)))
  expect_error(inst$set(2, 2, 2, id = c("a", "b", "d"), strict = 2))
  
  ## Different lengths //
  expect_false(inst$set(2, 2, 2, id = c("b", "c")))
  expect_warning(expect_false(inst$set(2, 2, 2, id = c("b", "c"), strict = 1)))
  expect_error(inst$set(2, 2, 2, id = c("b", "c"), strict = 2))
  
})

test_that("ListArray/set/new", {
  
  expect_is(inst <- ListArray$new(list(a = 1)), "ListArray")
  expect_true(inst$set(list(b = 1), must_exist = FALSE))
  expect_identical(inst$.array, list(a = 1, b = 1))
  expect_identical(inst$set(list(c = 1, d = 1), must_exist = FALSE), 
    c(c = TRUE, d = TRUE))

})

##------------------------------------------------------------------------------
context("ListArray/get")
##------------------------------------------------------------------------------

test_that("ListArray/get", {  

  expect_is(inst <- ListArray$new(list(a = 1, b = 1)), "ListArray")
  expect_equal(inst$.array, list(a = 1, b = 1))
  expect_equal(inst$get(), list(a = 1, b = 1))
  
  expect_identical(inst$get("a"), 1)
  expect_identical(inst$get("a", inner = FALSE), list(a = 1))
  expect_identical(inst$get(c("a", "b")), list(a = 1, b = 1))
  expect_identical(inst$get(c("a", "b"), inner = FALSE), 
    list(a = list(a = 1), b = list(b = 1)))
  expect_identical(inst$get("c"), NULL)
  expect_identical(inst$get("c", inner = FALSE), NULL)
  expect_identical(inst$get(c("a", "c")), list(a = 1, c = NULL))
  expect_identical(inst$get(c("a", "c"), inner = FALSE), 
    list(a = list(a = 1), c = NULL))
  
  inst <- ListArray$new()
  expect_equal(inst$get(), structure(list(), names = character()))
  
  expect_is(inst <- ListArray$new(list(a = 1)), "ListArray")
  expect_identical(inst$get("b"), NULL)
  expect_warning(expect_identical(inst$get("b", strict = 1), NULL))
  expect_error(inst$get("b", strict = 2))
  expect_identical(inst$get(c("a", "b")), list(a = 1, b = NULL))
  expect_warning(expect_identical(inst$get(c("a", "b"), strict = 1), list(a = 1, b = NULL)))
  expect_error(inst$get(c("a", "b"), strict = 2))
  
})

##------------------------------------------------------------------------------
context("ListArray/exists")
##------------------------------------------------------------------------------

test_that("ListArray/exists", {  
  
  expect_is(inst <- ListArray$new(list(a = 1, b = 1)), "ListArray")
  expect_true(inst$exists("a"))
  expect_true(all(inst$exists(c("a", "b"))))
  expect_false(inst$exists("c"))
  
})

##------------------------------------------------------------------------------
context("ListArray/index")
##------------------------------------------------------------------------------

test_that("ListArray/index", {  
  
  expect_is(inst <- ListArray$new(list(a = 1, b = 1)), "ListArray")
  expect_identical(inst$index("a"), structure(1, names = "a"))
  expect_identical(inst$index(c("a", "b")), 
    structure(as.numeric(1:2), names = c("a", "b")))
  expect_identical(inst$index("c"), structure(NA, names = "c"))
  expect_warning(expect_identical(inst$index("c", strict = 1), 
    structure(NA, names = "c")))
  expect_error(inst$index("c", strict = 2))
  expect_identical(inst$index(c("a", "c")), 
    structure(c(1, NA), names = c("a", "c")))
  
  expect_identical(inst$index("c", simplify = TRUE), numeric())
  expect_warning(expect_identical(inst$index("c", 
    simplify = TRUE, strict = 1), numeric()))
  expect_error(inst$index("c", simplify = TRUE, strict = 2))
  expect_identical(inst$index(c("a", "c"), simplify = TRUE), as.numeric(1))
  
})
  
##------------------------------------------------------------------------------
context("ListArray/clear")
##------------------------------------------------------------------------------

test_that("ListArray/clear", {  
  
  expect_is(inst <- ListArray$new(list(a = 1, b = 1)), "ListArray")
  expect_true(inst$clear())
  expect_identical(inst$.array, structure(list(), names = character()))
  
})  
  
##------------------------------------------------------------------------------
context("ListArray/remove")
##------------------------------------------------------------------------------

test_that("ListArray/remove", {  
  
  expect_is(inst <- ListArray$new(list(a = 1, b = 1, c = 1)), "ListArray")
  expect_true(inst$rm("a"))
  expect_false(inst$exists("a"))
  expect_true(all(inst$rm(c("b", "c"))))
  expect_false(inst$rm("a"))
  expect_false(all(inst$rm(c("a", "b"))))
  expect_true(inst$add(list(a = 1)))
  expect_equivalent(inst$rm(c("a", "b")), c(TRUE, FALSE))
  expect_false(inst$rm("a"))
  expect_warning(expect_false(inst$rm("a", strict = 1)))
  expect_error(inst$rm("a", strict = 2))
  
})

##------------------------------------------------------------------------------
context("ListArray/remove first")
##------------------------------------------------------------------------------

test_that("ListArray/remove first", {  
  
  expect_is(inst <- ListArray$new(list(a = 1, b = 1, c = 1)), "ListArray")
  expect_true(inst$rmFirst())
  expect_identical(inst$.array, list(b = 1, c = 1))
  expect_true(all(inst$rmFirst(2)))
  expect_identical(inst$.array, structure(list(), names = character()))
  
  expect_is(inst <- ListArray$new(list(a = 1, b = 1, c = 1)), "ListArray")
  expect_identical(inst$rmFirst(4), structure(c(rep(TRUE, 3), FALSE), names = 1:4))
  expect_is(inst <- ListArray$new(list(a = 1, b = 1, c = 1)), "ListArray")
  expect_warning(expect_identical(inst$rmFirst(4, strict = 1), 
    structure(c(rep(TRUE, 3), FALSE), names = 1:4)))
  expect_is(inst <- ListArray$new(list(a = 1, b = 1, c = 1)), "ListArray")
  expect_error(inst$rmFirst(4, strict = 2))
  expect_is(inst <- ListArray$new(list(a = 1, b = 1, c = 1)), "ListArray")
  expect_false(inst$rmFirst(4, simplify = TRUE))
  expect_is(inst <- ListArray$new(list(a = 1, b = 1, c = 1)), "ListArray")
  expect_true(inst$rmFirst(3, simplify = TRUE))
  expect_identical(inst$.array, structure(list(), names = character()))
  
  expect_true(inst$rmFirst())
  expect_identical(inst$.array, structure(list(), names = character()))
    
})  

##------------------------------------------------------------------------------
context("ListArray/remove last")
##------------------------------------------------------------------------------

test_that("ListArray/remove last", {  
  
  expect_is(inst <- ListArray$new(list(a = 1, b = 1, c = 1)), "ListArray")
  expect_identical(inst$rmLast(), c("3" = TRUE))
  expect_identical(inst$.array, list(a = 1, b = 1))
  expect_identical(inst$rmLast(2), c("2" = TRUE, "1" = TRUE))
  expect_identical(inst$.array, structure(list(), names = character()))
  
  ## Simplify //
  expect_is(inst <- ListArray$new(list(a = 1, b = 1, c = 1)), "ListArray")
  expect_true(inst$rmLast(simplify = TRUE))
  expect_true(inst$rmLast(2, simplify = TRUE))
  
  expect_is(inst <- ListArray$new(list(a = 1, b = 1, c = 1)), "ListArray")
  expect_identical(inst$rmLast(4), 
    structure(c(rep(TRUE, 3), FALSE), names = 3:0))
  expect_identical(inst$.array, structure(list(), names = character()))
  expect_is(inst <- ListArray$new(list(a = 1, b = 1, c = 1)), "ListArray")
  expect_warning(expect_identical(inst$rmLast(4, strict = 1), 
    structure(c(rep(TRUE, 3), FALSE), names = 3:0)))
  expect_is(inst <- ListArray$new(list(a = 1, b = 1, c = 1)), "ListArray")
  expect_error(inst$rmLast(4, strict = 2))
  
  ## Simplify //
  expect_is(inst <- ListArray$new(list(a = 1, b = 1, c = 1)), "ListArray")
  expect_false(inst$rmLast(4, simplify = TRUE))
  expect_identical(inst$.array, structure(list(), names = character()))
  
  inst <- ListArray$new()
  expect_false(inst$rmLast())
  expect_warning(expect_false(inst$rmLast(strict = 1)))
  expect_error(inst$rmLast(strict = 2))
  expect_false(all(inst$rmLast(2)))
  expect_warning(expect_false(all(inst$rmLast(strict = 1))))
  expect_error(inst$rmLast(strict = 2))
  expect_identical(inst$.array, structure(list(), names = character()))
  
  ## Simplify //
  inst <- ListArray$new()
  expect_false(inst$rmLast(simplify = TRUE))
  expect_warning(expect_false(inst$rmLast(strict = 1, simplify = TRUE)))
  expect_false(inst$rmLast(2, simplify = TRUE))
  expect_warning(expect_false(inst$rmLast(strict = 1, simplify = TRUE)))
  
})

##------------------------------------------------------------------------------
context("ListArray/copy")
##------------------------------------------------------------------------------

test_that("ListArray/copy", { 
  
  expect_is(inst <- ListArray$new(list(a = 1)), "ListArray")
  expect_true(inst$copy("a", "b"))
  expect_identical(inst$.array, list(a = 1, b = 1))
  
  expect_false(inst$copy("c", "d"))
  expect_identical(inst$.array, list(a = 1, b = 1))
  expect_warning(expect_false(inst$copy("c", "d", strict = 1)))
  expect_error(inst$copy("c", "d", strict = 2))

  expect_true(all(inst$copy(c("a", "b"), c("c", "d"))))
  expect_identical(inst$.array, list(a = 1, b = 1, c = 1, d = 1))
  expect_false(all(inst$copy(c("a", "b"), c("c", "d"), dups = FALSE)))
  expect_identical(inst$.array, list(a = 1, b = 1, c = 1, d = 1))
  expect_warning(expect_false(all(inst$copy(c("a", "b"), c("c", "d"), 
    dups = FALSE, strict = 1))))
  expect_error(all(inst$copy(c("a", "b"), c("c", "d"), 
    dups = FALSE, strict = 2)))
  expect_true(all(inst$copy(c("a", "b"), c("c", "d"))))
  expect_identical(inst$get(), list(a = 1, b = 1, c = 1, d = 1, c = 1, d = 1))

  expect_false(all(inst$copy(c("a", "b"), "d")))  
  expect_warning(expect_false(all(inst$copy(c("a", "b"), "d", strict = 1))))
  expect_error(all(inst$copy(c("a", "b"), "d", strict = 2)))
  
})
