##------------------------------------------------------------------------------
context("ArrayEnvironment/initialize")
##------------------------------------------------------------------------------

test_that("ArrayEnvironment/initialize/default", {
  
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_is(inst$.array, "environment")
  expect_identical(ls(inst$.array), character())
  
})

test_that("ArrayEnvironment/initialize/explicit", {
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 2)), "ArrayEnvironment")
  expect_identical(ls(inst$.array), c("a", "b"))
  expect_identical(inst$.array$a, 1)
  expect_identical(inst$.array$b, 2)
  expect_is(inst <- ArrayEnvironment$new(list(a = 1), list(b = 2)), "ArrayEnvironment")
  expect_identical(ls(inst$.array), c("a", "b"))
  expect_identical(inst$.array$a, 1)
  expect_identical(inst$.array$b, 2)
  
  ## Atomic //
  expect_is(inst <- ArrayEnvironment$new(a = 1, b = 2), "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  expect_is(inst <- ArrayEnvironment$new(a = 1, list(b = 2)), "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list(a = 1, "1" = list(b = 2)))
  expect_is(inst <- ArrayEnvironment$new(list(a = 1), list(b = 2)), "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  
})

test_that("ArrayEnvironment/initialize/explicit/unnamed", {
  
  expect_is(inst <- ArrayEnvironment$new(list(1, 2, a = 3)), "ArrayEnvironment")
  expect_identical(ls(inst$.array), c("1", "2", "a"))
  expect_identical(inst$.array[["1"]], 1)
  expect_identical(inst$.array[["2"]], 2)
  expect_identical(inst$.array$a, 3)
  
  expect_is(inst <- ArrayEnvironment$new(list(1), list(a = 2)), "ArrayEnvironment")
  expect_identical(ls(inst$.array), c("1", "a"))
  expect_identical(inst$.array[["1"]], 1)
  expect_identical(inst$.array$a, 2)
  
})

##------------------------------------------------------------------------------
context("ArrayEnvironment/add")
##------------------------------------------------------------------------------

test_that("ArrayEnvironment/add/single/named", {
  
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_true(inst$add(list(a = 1)))
  expect_identical(ls(inst$.array), "a")
  expect_identical(inst$.array$a, 1)
  expect_true(inst$add(list(b = 2)))
  expect_identical(inst$.array$b, 2)
  expect_identical(ls(inst$.array), c("a", "b"))
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  
  expect_false(inst$add(list(b = 3), dups = FALSE))
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  expect_warning(expect_false(inst$add(list(b = 2), dups = FALSE, strict = 1)))
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  expect_error(inst$add(list(b = 3), dups = FALSE, strict = 2))
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  
  expect_true(inst$add(list(b = 3)))
  expect_identical(as.list(inst$.array), list(a = 1, b = 3))
  
})

test_that("ArrayEnvironment/add/single/unnamed", {
  
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_true(inst$add(list(1)))
  expect_identical(as.list(inst$.array), list("1" = 1))
  expect_true(inst$add(2))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2))
  expect_true(inst$add(2))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2, "3" = 2))
  
  expect_false(inst$add(list("1" = 2), dups = FALSE))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2, "3" = 2))
  expect_warning(expect_false(inst$add(list("1" = 2), dups = FALSE, strict = 1)))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2, "3" = 2))
  expect_error(inst$add(list("1" = 2), dups = FALSE, strict = 2))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2, "3" = 2))
  
  expect_true(inst$add(list("1" = 3)))
  expect_identical(as.list(inst$.array), list("1" = 3, "2" = 2, "3" = 2))
  
  ## Atomic //
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_true(inst$add(1))
  expect_identical(as.list(inst$.array), list("1" = 1))
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_true(inst$add(new.env()))
  expect_is(res <- as.list(inst$.array), "list")
  expect_is(res[[1]], "environment")
  expect_true(inst$add(a = new.env()))
  expect_is(res <- as.list(inst$.array), "list")
  expect_is(res$a, "environment")
  expect_true(inst$add(b = 1))
  expect_is(res <- as.list(inst$.array), "list")
  expect_identical(res$b, 1)
  
})

test_that("ArrayEnvironment/add/multiple", {
  
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_true(all(inst$add(list(a = 1), list(b = 2))))
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  
  expect_true(all(inst$add(1, 1, 1, id = c("c", "d", "e"))))
  expect_identical(as.list(inst$.array), list(a = 1, b = 2, c = 1, d = 1, e = 1))
  
  expect_false(all(inst$add(1, 1, 1, id = c("e", "f"), dups = FALSE)))
  expect_identical(as.list(inst$.array), list(a = 1, b = 2, c = 1, d = 1, e = 1))
  
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_false(inst$add(1, 1, 1, id = c("a", "b")))
  expect_warning(expect_false(inst$add(1, 1, 1, id = c("a", "b"), strict = 1)))
  expect_error(inst$add(1, 1, 1, id = c("a", "b"), strict = 2))
  
})

##------------------------------------------------------------------------------
context("ArrayEnvironment/set")
##------------------------------------------------------------------------------

test_that("ArrayEnvironment/set/single", {
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1)), "ArrayEnvironment")
  expect_true(inst$set(list(a = 2)))
  expect_identical(as.list(inst$.array), list(a = 2))
  expect_false(inst$set(list(b = 2)))
  expect_warning(expect_false(inst$set(list(b = 2), strict = 1)))
  expect_error(inst$set(list(b = 2), strict = 2))
  expect_identical(as.list(inst$.array), list(a = 2))
  
})

test_that("ArrayEnvironment/set/multiple", {
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1)), "ArrayEnvironment")
  expect_identical(inst$set(list(a = 2), list(b = 2)), c(a = TRUE, b = TRUE))
  expect_identical(as.list(inst$.array), list(a = 2, b = 2))
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1)), "ArrayEnvironment")
  expect_true(all(inst$set(2, 2, 2, id = c("a", "b", "c"))))
  expect_identical(as.list(inst$.array), list(a = 2, b = 2, c = 2))
  
  expect_identical(inst$set(2, 2, 2, id = c("a", "b", "d")), 
    c(a = TRUE, b = TRUE, d = FALSE))
  expect_identical(as.list(inst$.array), list(a = 2, b = 2, c = 2))
  expect_warning(expect_identical(inst$set(2, 2, 2, id = c("a", "b", "d"), strict = 1), 
    c(a = TRUE, b = TRUE, d = FALSE)))
  expect_error(inst$set(2, 2, 2, id = c("a", "b", "d"), strict = 2))
  
  ## Different lengths //
  expect_false(inst$set(2, 2, 2, id = c("b", "c")))
  expect_warning(expect_false(inst$set(2, 2, 2, id = c("b", "c"), strict = 1)))
  expect_error(inst$set(2, 2, 2, id = c("b", "c"), strict = 2))
  
})

test_that("ArrayEnvironment/set/new", {
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1)), "ArrayEnvironment")
  expect_true(inst$set(list(b = 1), must_exist = FALSE))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1))
  expect_identical(inst$set(list(c = 1, d = 1), must_exist = FALSE), 
    c(c = TRUE, d = TRUE))

})

##------------------------------------------------------------------------------
context("ArrayEnvironment/get")
##------------------------------------------------------------------------------

test_that("ArrayEnvironment/get", {  

  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1)), 
    "ArrayEnvironment")
  expect_equal(as.list(inst$.array), list(a = 1, b = 1))
  expect_equal(as.list(inst$get()), list(a = 1, b = 1))
  expect_equal(inst$get(list = TRUE), list(a = 1, b = 1))
  
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
  
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_equal(as.list(inst$get()), structure(list(), names = character()))
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1)), "ArrayEnvironment")
  expect_identical(inst$get("b"), NULL)
  expect_warning(expect_identical(inst$get("b", strict = 1), NULL))
  expect_error(inst$get("b", strict = 2))
  expect_identical(inst$get(c("a", "b")), list(a = 1, b = NULL))
  expect_warning(expect_identical(inst$get(c("a", "b"), strict = 1), list(a = 1, b = NULL)))
  expect_error(inst$get(c("a", "b"), strict = 2))
  
})

test_that("ArrayEnvironment/get/order", {  

  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, 
    "1" = 1, "10" = 1, "2" = 1, "20" = 1, .a = 1)), 
    "ArrayEnvironment")

  expect_equal(as.list(inst$.array, all.names = TRUE), 
    list("20" = 1, a = 1, b = 1, "10" = 1, "1" = 1, .a = 1, "2" = 1))
  expect_equal(as.list(inst$get(), all.names = TRUE), 
    list("20" = 1, a = 1, b = 1, "10" = 1, "1" = 1, .a = 1, "2" = 1))
  expect_equal(inst$get(list = TRUE), 
    list("1" = 1, "2" = 1, "10" = 1, "20" = 1, .a = 1, a = 1, b = 1))
  expect_equal(inst$get(list = TRUE, all_names = TRUE, sorted = FALSE), 
    list(.a = 1, "1" = 1, "10" = 1, "2" = 1, "20" = 1, a = 1, b = 1))
  
})

##------------------------------------------------------------------------------
context("ArrayEnvironment/exists")
##------------------------------------------------------------------------------

test_that("ArrayEnvironment/exists", {  
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1)), "ArrayEnvironment")
  expect_true(inst$exists("a"))
  expect_true(all(inst$exists(c("a", "b"))))
  expect_false(inst$exists("c"))
  
})

##------------------------------------------------------------------------------
context("ArrayEnvironment/index")
##------------------------------------------------------------------------------

test_that("ArrayEnvironment/index", {  
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1)), "ArrayEnvironment")
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
context("ArrayEnvironment/clear")
##------------------------------------------------------------------------------

test_that("ArrayEnvironment/clear", {  
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1)), "ArrayEnvironment")
  expect_true(inst$clear())
  expect_is(res <- inst$.array, "environment")
  expect_identical(ls(res, all.names = TRUE), character())
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
})  
  
##------------------------------------------------------------------------------
context("ArrayEnvironment/remove")
##------------------------------------------------------------------------------

test_that("ArrayEnvironment/remove", {  
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1)), "ArrayEnvironment")
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

test_that("ArrayEnvironment/remove/index", {  
  
  ## Sorted //
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, 
    "1" = 1, "10" = 1, "2" = 1, "20" = 1, .a = 1)), 
    "ArrayEnvironment")
  inst$get(list = TRUE)
  expect_true(all(inst$rm(id = 1:2)))
  inst$get(list = TRUE)
  
  ## Unsorted //
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, 
    "1" = 1, "10" = 1, "2" = 1, "20" = 1, .a = 1)), 
    "ArrayEnvironment")
  inst$get(list = TRUE, sorted = FALSE, all_names = TRUE)
  expect_true(all(inst$rm(id = 1:2, sorted = FALSE, all_names = TRUE)))
  inst$get(list = TRUE, sorted = FALSE, all_names = TRUE)
  
})

##------------------------------------------------------------------------------
context("ArrayEnvironment/remove first")
##------------------------------------------------------------------------------

test_that("ArrayEnvironment/remove first", {  
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1)), "ArrayEnvironment")
  expect_true(inst$rmFirst())
  expect_identical(as.list(inst$.array), list(b = 1, c = 1))
  expect_true(all(inst$rmFirst(2)))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1)), "ArrayEnvironment")
  expect_identical(inst$rmFirst(4), structure(c(rep(TRUE, 3), FALSE), names = 1:4))
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1)), "ArrayEnvironment")
  expect_warning(expect_identical(inst$rmFirst(4, strict = 1), 
    structure(c(rep(TRUE, 3), FALSE), names = 1:4)))
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1)), "ArrayEnvironment")
  expect_error(inst$rmFirst(4, strict = 2))
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1)), "ArrayEnvironment")
  expect_false(inst$rmFirst(4, simplify = TRUE))
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1)), "ArrayEnvironment")
  expect_true(inst$rmFirst(3, simplify = TRUE))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
  expect_true(inst$rmFirst())
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
    
})  

##------------------------------------------------------------------------------
context("ArrayEnvironment/remove last")
##------------------------------------------------------------------------------

test_that("ArrayEnvironment/remove last", {  
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1)), "ArrayEnvironment")
  expect_identical(inst$rmLast(), c("3" = TRUE))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1))
  expect_identical(inst$rmLast(2), c("2" = TRUE, "1" = TRUE))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
  ## Simplify //
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1)), "ArrayEnvironment")
  expect_true(inst$rmLast(simplify = TRUE))
  expect_true(inst$rmLast(2, simplify = TRUE))
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1)), "ArrayEnvironment")
  expect_identical(inst$rmLast(4), 
    structure(c(rep(TRUE, 3), FALSE), names = 3:0))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1)), "ArrayEnvironment")
  expect_warning(expect_identical(inst$rmLast(4, strict = 1), 
    structure(c(rep(TRUE, 3), FALSE), names = 3:0)))
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1)), "ArrayEnvironment")
  expect_error(inst$rmLast(4, strict = 2))
  
  ## Simplify //
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1)), "ArrayEnvironment")
  expect_false(inst$rmLast(4, simplify = TRUE))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_false(inst$rmLast())
  expect_warning(expect_false(inst$rmLast(strict = 1)))
  expect_error(inst$rmLast(strict = 2))
  expect_false(all(inst$rmLast(2)))
  expect_warning(expect_false(all(inst$rmLast(strict = 1))))
  expect_error(inst$rmLast(strict = 2))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
  ## Simplify //
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_false(inst$rmLast(simplify = TRUE))
  expect_warning(expect_false(inst$rmLast(strict = 1, simplify = TRUE)))
  expect_false(inst$rmLast(2, simplify = TRUE))
  expect_warning(expect_false(inst$rmLast(strict = 1, simplify = TRUE)))
  
})

##------------------------------------------------------------------------------
context("ArrayEnvironment/copy")
##------------------------------------------------------------------------------

test_that("ArrayEnvironment/copy", { 
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1)), "ArrayEnvironment")
  expect_true(inst$copy("a", "b"))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1))
  
  expect_false(inst$copy("c", "d"))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1))
  expect_warning(expect_false(inst$copy("c", "d", strict = 1)))
  expect_error(inst$copy("c", "d", strict = 2))

  expect_true(all(inst$copy(c("a", "b"), c("c", "d"))))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1, c = 1, d = 1))
  expect_false(all(inst$copy(c("a", "b"), c("c", "d"), dups = FALSE)))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1, c = 1, d = 1))
  expect_warning(expect_false(all(inst$copy(c("a", "b"), c("c", "d"), 
    dups = FALSE, strict = 1))))
  expect_error(all(inst$copy(c("a", "b"), c("c", "d"), 
    dups = FALSE, strict = 2)))
  expect_true(all(inst$copy(c("a", "b"), c("c", "d"))))
  expect_identical(as.list(inst$get()), 
    list(a = 1, b = 1, c = 1, d = 1))

  expect_false(all(inst$copy(c("a", "b"), "d")))  
  expect_warning(expect_false(all(inst$copy(c("a", "b"), "d", strict = 1))))
  expect_error(all(inst$copy(c("a", "b"), "d", strict = 2)))
  
})
