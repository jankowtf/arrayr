##------------------------------------------------------------------------------
context("ArrayEnvironment/initialize")
##------------------------------------------------------------------------------

test_that("ArrayEnvironment/initialize/default", {
  
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_is(inst$.array, "environment")
  expect_identical(ls(inst$.array), character())
  
})

test_that("ArrayEnvironment/initialize/explicit/list/named", {
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 2)), "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1), list(b = 2)), 
    "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  
  expect_is(inst <- ArrayEnvironment$new(a = list(a = 1, b = 2)), 
    "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list(a = list(a = 1, b = 2)))
  
  envir <- new.env()
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = envir)), 
    "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list(a = 1, b = envir))
  
})

test_that("ArrayEnvironment/initialize/explicit/list/unnamed", {
  
  expect_is(inst <- ArrayEnvironment$new(list(1, 2)), 
    "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2))
  
  expect_is(inst <- ArrayEnvironment$new(list(1), list(2)), 
    "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2))
  
  envir <- new.env()
  expect_is(inst <- ArrayEnvironment$new(list(1, envir)), 
    "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = envir))
  
})

test_that("ArrayEnvironment/initialize/explicit/list/mixed", {
  
  expect_is(inst <- ArrayEnvironment$new(list(1), list(a = 2)), 
    "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list(a = 2, "1" = 1))
  
  expect_is(inst <- ArrayEnvironment$new(list(1, 2, a = 3)), 
    "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list(a = 3, "1" = 1, "2" = 2))
  
  expect_is(inst <- ArrayEnvironment$new(list(list(a = 1)), list(b = 2)), 
    "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list(b = 2, "1" = list(a = 1)))
  
  expect_is(inst <- ArrayEnvironment$new(list(list(1, a = 1))), 
    "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list("1" = list(1, a = 1)))
  
  expect_is(inst <- ArrayEnvironment$new(list(list(1, 2, a = 3))), 
    "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list("1" = list(1, 2, a = 3)))
  
})

test_that("ArrayEnvironment/initialize/explicit/atomic/named", {
  
  expect_is(inst <- ArrayEnvironment$new(a = 1, b = 2), "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  
  envir <- new.env()
  expect_is(inst <- ArrayEnvironment$new(a = 1, b = envir), "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list(a = 1, b = envir))
  
  expect_is(inst <- ArrayEnvironment$new(a = 1, b = list(a = 1, b = 2)), 
    "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list(a = 1, b = list(a = 1, b = 2)))
  
})

test_that("ArrayEnvironment/initialize/explicit/atomic/unnamed", {
  
  expect_is(inst <- ArrayEnvironment$new(1, 2), "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2))
  
  envir <- new.env()
  expect_is(inst <- ArrayEnvironment$new(1, envir), "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = envir))
  
})

test_that("ArrayEnvironment/initialize/explicit/mixed", {
  
  expect_is(inst <- ArrayEnvironment$new(a = 1, 2), "ArrayEnvironment")
  expect_identical(as.list(inst$.array), list(a = 1, "1" = 2))

  envir <- new.env()
  expect_is(inst <- ArrayEnvironment$new(a = 1, list(b = 2), envir),
    "ArrayEnvironment")
  expect_identical(as.list(inst$.array), 
    list(a = 1, "1" = list(b = 2), "2" = envir))
  
})

##------------------------------------------------------------------------------
context("ArrayEnvironment/add")
##------------------------------------------------------------------------------

test_that("ArrayEnvironment/add/list/single/named", {
  
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_true(inst$add(list(a = 1)))
  expect_identical(as.list(inst$.array), list(a = 1))
  
  expect_true(inst$add(list(b = 2)))
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  
  expect_false(inst$add(list(b = 3), overwrite = FALSE))
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  expect_warning(expect_false(inst$add(list(b = 2), overwrite = FALSE, strict = 1)))
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  expect_error(inst$add(list(b = 3), overwrite = FALSE, strict = 2))
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  
  expect_true(inst$add(list(b = 3)))
  expect_identical(as.list(inst$.array), list(a = 1, b = 3))
  
})

test_that("ArrayEnvironment/add/list/single/unnamed", {
  
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_true(inst$add(list(1)))
  expect_identical(as.list(inst$.array), list("1" = 1))
  expect_true(inst$add(2))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2))
  expect_true(inst$add(2))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2, "3" = 2))
  
  expect_false(inst$add(list("1" = 2), overwrite = FALSE))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2, "3" = 2))
  expect_warning(expect_false(inst$add(list("1" = 2), overwrite = FALSE, strict = 1)))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2, "3" = 2))
  expect_error(inst$add(list("1" = 2), overwrite = FALSE, strict = 2))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2, "3" = 2))
  
  expect_true(inst$add(list("1" = 3)))
  expect_identical(as.list(inst$.array), list("1" = 3, "2" = 2, "3" = 2))
  
})

test_that("ArrayEnvironment/add/list/multiple/named", {
  
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_true(all(inst$add(list(a = 1), list(b = 2))))
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))

})

test_that("ArrayEnvironment/add/list/multiple/unnamed", {
  
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_true(all(inst$add(list(1), list(2))))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2))

})

test_that("ArrayEnvironment/add/atomic/single/named", {

  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  envir <- new.env()
  expect_true(inst$add(a = envir))
  expect_identical(as.list(inst$.array), list(a = envir))
  expect_true(inst$add(b = 1))
  expect_identical(as.list(inst$.array), list(a = envir, b = 1))
  
})

test_that("ArrayEnvironment/add/atomic/single/unnamed", {
  
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_true(inst$add(1))
  expect_identical(as.list(inst$.array), list("1" = 1))
  expect_true(inst$add(1))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 1))
  
  envir <- new.env()
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_true(inst$add(envir))
  expect_identical(as.list(inst$.array), list("1" = envir))
  
})

test_that("ArrayEnvironment/add/atomic/single/id", {  
  
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_true(all(inst$add(1, id = "a")))
  expect_identical(as.list(inst$.array), list(a = 1))
  expect_true(all(inst$add(1, id = "b")))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1))
  
  expect_false(all(inst$add(1, id = "b", overwrite = FALSE)))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1))
  expect_true(all(inst$add(2, id = "b")))
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_false(inst$add(1, id = c("a", "b")))
  expect_warning(expect_false(inst$add(1, id = c("a", "b"), strict = 1)))
  expect_error(inst$add(1, id = c("a", "b"), strict = 2))
  
})

test_that("ArrayEnvironment/add/atomic/multiple/named", {
  
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_true(all(inst$add(a = 1, b = 2)))
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  
  envir <- new.env()
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_true(all(inst$add(a = envir, b = envir)))
  expect_identical(as.list(inst$.array), list(a = envir, b = envir))
  
})

test_that("ArrayEnvironment/add/atomic/multiple/unnamed", {
  
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_true(all(inst$add(1:3)))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2, "3" = 3))
  
  envir <- new.env()
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_true(all(inst$add(envir, envir)))
  expect_identical(as.list(inst$.array), list("1" = envir, "2" = envir))
  
})

test_that("ArrayEnvironment/add/atomic/multiple/id", {  
  
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_true(all(inst$add(1, 1, 1, id = c("c", "d", "e"))))
  expect_identical(as.list(inst$.array), list(c = 1, d = 1, e = 1))
  
  expect_false(all(inst$add(1, 1, 1, id = c("e", "f"), overwrite = FALSE)))
  expect_identical(as.list(inst$.array), list(c = 1, d = 1, e = 1))
  
  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_false(inst$add(1, 1, 1, id = c("a", "b")))
  expect_warning(expect_false(inst$add(1, 1, 1, id = c("a", "b"), strict = 1)))
  expect_error(inst$add(1, 1, 1, id = c("a", "b"), strict = 2))
  
})

##------------------------------------------------------------------------------
context("ArrayEnvironment/set")
##------------------------------------------------------------------------------

test_that("ArrayEnvironment/set/list/single", {
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1)), "ArrayEnvironment")
  expect_true(inst$set(list(a = 2)))
  expect_identical(as.list(inst$.array), list(a = 2))
  expect_false(inst$set(list(b = 2)))
  expect_warning(expect_false(inst$set(list(b = 2), strict = 1)))
  expect_error(inst$set(list(b = 2), strict = 2))
  expect_identical(as.list(inst$.array), list(a = 2))
  
})

test_that("ArrayEnvironment/set/list/multiple", {
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1)), "ArrayEnvironment")
  expect_identical(inst$set(list(a = 2), list(b = 2)), c(a = TRUE, b = TRUE))
  expect_identical(as.list(inst$.array), list(a = 2, b = 2))
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1)), 
    "ArrayEnvironment")
  expect_true(all(inst$set(2, 2, 2, id = c("a", "b", "c"))))
  expect_identical(as.list(inst$.array), list(a = 2, b = 2, c = 2))
  
})

test_that("ArrayEnvironment/set/list/new", {
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1)), "ArrayEnvironment")
  expect_true(inst$set(list(b = 1), must_exist = FALSE))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1))
  expect_identical(inst$set(list(c = 1, d = 1), must_exist = FALSE), 
    c(c = TRUE, d = TRUE))

})

test_that("ArrayEnvironment/set/atomic/single/named", {
  
  expect_is(inst <- ArrayEnvironment$new(a = 1), "ArrayEnvironment")
  expect_true(inst$set(a = 10))
  expect_identical(as.list(inst$.array), list(a = 10))
  
  expect_is(inst <- ArrayEnvironment$new(a = 1), "ArrayEnvironment")
  expect_true(inst$set(b = 1, must_exist = FALSE))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1))
  
})

test_that("ArrayEnvironment/set/atomic/single/unnamed", {
  
  expect_is(inst <- ArrayEnvironment$new(1), "ArrayEnvironment")
  expect_true(inst$set(10))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 10))
  
})

test_that("ArrayEnvironment/set/atomic/multiple/named", {
  
  expect_is(inst <- ArrayEnvironment$new(a = 1, b = 1), "ArrayEnvironment")
  expect_true(all(inst$set(a = 10, b = 10)))
  expect_identical(as.list(inst$.array), list(a = 10, b = 10))
  
  expect_is(inst <- ArrayEnvironment$new(a = 1, b = 1), "ArrayEnvironment")
  expect_true(all(inst$set(a = 10, b = 10, c = 1, must_exist = FALSE)))
  expect_identical(as.list(inst$.array), list(a = 10, b = 10, c = 1))
  
})

test_that("ArrayEnvironment/set/atomic/id", {
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, c = 1)), 
    "ArrayEnvironment")
  expect_identical(inst$set(2, 2, 2, id = c("a", "b", "d")), 
    c(a = TRUE, b = TRUE, d = FALSE))
  expect_identical(as.list(inst$.array), list(a = 2, b = 2, c = 1))
  
  ## Not yet existing //
  expect_warning(expect_identical(inst$set(2, 2, 2, id = c("a", "b", "d"), 
    strict = 1), c(a = TRUE, b = TRUE, d = FALSE)))
  expect_error(inst$set(2, 2, 2, id = c("a", "b", "d"), strict = 2))
  
  ## Different lengths //
  expect_false(inst$set(2, 2, 2, id = c("b", "c")))
  expect_warning(expect_false(inst$set(2, 2, 2, id = c("b", "c"), strict = 1)))
  expect_error(inst$set(2, 2, 2, id = c("b", "c"), strict = 2))
  
})

##------------------------------------------------------------------------------
context("ArrayEnvironment/get")
##------------------------------------------------------------------------------

test_that("ArrayEnvironment/get/all", {  

  expect_is(inst <- ArrayEnvironment$new(), "ArrayEnvironment")
  expect_equal(as.list(inst$get()), structure(list(), names = character()))
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1)), 
    "ArrayEnvironment")
  expect_equal(as.list(inst$get()), list(a = 1, b = 1))
  expect_equal(inst$get(list = TRUE), list(a = 1, b = 1))

})

test_that("ArrayEnvironment/get/character", {  
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1)), 
    "ArrayEnvironment")
  expect_identical(inst$get("a"), 1)
  expect_identical(inst$get("a", inner = FALSE), list(a = 1))
  expect_identical(inst$get(c("a", "b")), list(a = 1, b = 1))
  expect_identical(inst$get(c("a", "b"), inner = FALSE), 
    list(a = list(a = 1), b = list(b = 1)))
  
  expect_identical(inst$get("c"), NULL)
  expect_identical(inst$get("c", inner = FALSE), list(c = NULL))
  expect_warning(expect_identical(inst$get("c", strict = 1), NULL))
  expect_error(inst$get("c", strict = 2))
  
  expect_identical(inst$get(c("a", "c")), list(a = 1, c = NULL))
  expect_identical(inst$get(c("a", "c"), inner = FALSE), 
    list(a = list(a = 1), c = list(c = NULL)))
  
})

test_that("ArrayEnvironment/get/order", {  

  expect_is(inst <<- ArrayEnvironment$new(list(a = 1, b = 1, 
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

test_that("ArrayEnvironment/get/numeric", {  

  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1, 
    "1" = 1, "10" = 1, "2" = 1, "20" = 1, .a = 1)), 
    "ArrayEnvironment")

  expect_equal(inst$get(1:5), list("1" = 1, "2" = 1, "10" = 1, "20" = 1, a = 1))
  expect_equal(inst$get(1:3, char = TRUE), list("1" = 1, "2" = 1, "3" = NULL))
  
})

##------------------------------------------------------------------------------
context("ArrayEnvironment/exists")
##------------------------------------------------------------------------------

test_that("ArrayEnvironment/exists/character", {  
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1)), "ArrayEnvironment")
  expect_true(inst$exists("a"))
  expect_true(all(inst$exists(c("a", "b"))))
  expect_true(all(inst$exists("a", "b")))
  expect_false(inst$exists("c"))
  
})

test_that("ArrayEnvironment/exists/numerical", {  
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1)), "ArrayEnvironment")
  expect_true(inst$exists(1))
  expect_true(all(inst$exists(c(1, 2))))
  expect_true(all(inst$exists(1, 2)))
  expect_false(inst$exists(3))
  
  expect_is(inst <- ArrayEnvironment$new(list("1" = 1, b = 1)), "ArrayEnvironment")
  expect_equivalent(inst$exists(1, 2, char = TRUE), c(TRUE, FALSE))
  
})

##------------------------------------------------------------------------------
context("ArrayEnvironment/index")
##------------------------------------------------------------------------------

test_that("ArrayEnvironment/index", {  
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1)), "ArrayEnvironment")
  expect_identical(inst$index("a"), structure(1, names = "a"))
  expect_identical(inst$index(c("a", "b")), 
    structure(as.numeric(1:2), names = c("a", "b")))
  expect_identical(inst$index("a", "b"), 
    structure(as.numeric(1:2), names = c("a", "b")))
  
  expect_identical(inst$index("c"), structure(NA_integer_, names = "c"))
  expect_warning(expect_identical(inst$index("c", strict = 1), 
    structure(NA_integer_, names = "c")))
  expect_error(inst$index("c", strict = 2))
  expect_identical(inst$index(c("a", "c")), 
    structure(c(1, NA_integer_), names = c("a", "c")))
  
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

test_that("ArrayEnvironment/remove/character", {  
  
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

test_that("ArrayEnvironment/remove/numeric", {  
  
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

test_that("ArrayEnvironment/copy/single/character", { 
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1)), "ArrayEnvironment")
  expect_true(inst$copy("a", "b"))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1))
  
  expect_false(inst$copy("c", "d"))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1))
  expect_warning(expect_false(inst$copy("c", "d", strict = 1)))
  expect_error(inst$copy("c", "d", strict = 2))

})

test_that("ArrayEnvironment/copy/single/numeric", { 
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1)), "ArrayEnvironment")
  expect_true(inst$copy(1, 2))
  expect_identical(as.list(inst$.array), list(a = 1, "2" = 1))
  
  expect_false(inst$copy(3, 4))
  expect_identical(as.list(inst$.array), list(a = 1, "2" = 1))
  expect_warning(expect_false(inst$copy(3, 4, strict = 1)))
  expect_error(inst$copy(3, 4, strict = 2))

})

test_that("ArrayEnvironment/copy/multiple/character", { 
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1)), 
    "ArrayEnvironment")
  expect_true(all(inst$copy(c("a", "b"), c("c", "d"))))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1, c = 1, d = 1))
  expect_false(all(inst$copy(c("a", "b"), c("c", "d"))))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1, c = 1, d = 1))
  expect_warning(expect_false(all(inst$copy(c("a", "b"), c("c", "d"), 
    strict = 1))))
  expect_error(all(inst$copy(c("a", "b"), c("c", "d"), strict = 2)))
  expect_true(all(inst$copy(c("a", "b"), c("c", "d"), overwrite = TRUE)))
  expect_identical(as.list(inst$get()), list(a = 1, b = 1, c = 1, d = 1))

  expect_false(all(inst$copy(c("a", "b"), "d")))  
  expect_warning(expect_false(all(inst$copy(c("a", "b"), "d", strict = 1))))
  expect_error(all(inst$copy(c("a", "b"), "d", strict = 2)))
  
})

test_that("ArrayEnvironment/copy/multiple/numeric", { 
  
  expect_is(inst <- ArrayEnvironment$new(list(a = 1, b = 1)), 
    "ArrayEnvironment")
  expect_true(all(inst$copy(c(1, 2), c(3, 4))))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1, "3" = 1, "4" = 1))
  expect_false(all(inst$copy(c(1, 2), c(3, 4))))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1, c = 1, d = 1))
  expect_warning(expect_false(all(inst$copy(c(1, 2), c(3, 4), strict = 1))))
  expect_error(all(inst$copy(c(1, 2), c(3, 4), strict = 2)))
  expect_true(all(inst$copy(c(1, 2), c(3, 4), overwrite = TRUE)))
  expect_identical(as.list(inst$get()), list(a = 1, b = 1, "3" = 1, "4" = 1))

  expect_false(all(inst$copy(c(1, 2), 4)))  
  expect_warning(expect_false(all(inst$copy(c(1, 2), 4, strict = 1))))
  expect_error(all(inst$copy(c(1, 2), 4, strict = 2)))
  
})
