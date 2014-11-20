##------------------------------------------------------------------------------
context("HybridArray/initialize")
##------------------------------------------------------------------------------

test_that("HybridArray/initialize/default", {
  
  expect_is(inst <- HybridArray$new(), "HybridArray")
  expect_is(inst$.array, "environment")
  expect_identical(ls(inst$.array), character())
  
})

test_that("HybridArray/initialize/explicit/list/named", {
  
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 2)), "HybridArray")
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  
  expect_is(inst <<- HybridArray$new(list(a = 1), list(b = 2)), 
    "HybridArray")
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  
  expect_is(inst <<- HybridArray$new(a = list(a = 1, b = 2)), 
    "HybridArray")
  expect_identical(as.list(inst$.array), list(a = list(a = 1, b = 2)))
  
  envir <- new.env()
  expect_is(inst <<- HybridArray$new(list(a = 1, b = envir)), 
    "HybridArray")
  expect_identical(as.list(inst$.array), list(a = 1, b = envir))
  
})

test_that("HybridArray/initialize/explicit/list/unnamed", {
  
  expect_is(inst <<- HybridArray$new(list(1, 2)), "HybridArray")
  expect_identical(inst$.array$._list, list(1, 2))
  
  expect_is(inst <<- HybridArray$new(list(1), list(2)), "HybridArray")
  expect_identical(inst$.array$._list, list(1, 2))
  
  envir <- new.env()
  expect_is(inst <<- HybridArray$new(list(1, envir)), "HybridArray")
  expect_identical(inst$.array$._list, list(1, envir))
  
})

test_that("HybridArray/initialize/explicit/list/unnamed/force", {
  
  expect_is(inst <<- HybridArray$new(list(1, 2), force = TRUE), "HybridArray")
  expect_identical(inst$.array$._list, list())
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2))
  
  expect_is(inst <<- HybridArray$new(list(1), list(2), force = TRUE), 
    "HybridArray")
  expect_identical(inst$.array$._list, list())
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2))
  
  envir <- new.env()
  expect_is(inst <<- HybridArray$new(list(1, envir), force = TRUE), 
    "HybridArray")
  expect_identical(inst$.array$._list, list())
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = envir))
  
})

test_that("HybridArray/initialize/explicit/list/mixed", {
  
  expect_is(inst <<- HybridArray$new(list(1), list(a = 2)), "HybridArray")
  expect_identical(inst$.array$._list, list(1))
  expect_identical(as.list(inst$.array), list(a = 2))
  
  expect_is(inst <<- HybridArray$new(list(1, 2, a = 3)), "HybridArray")
  expect_identical(inst$.array$._list, list(1, 2))
  expect_identical(as.list(inst$.array), list(a = 3))
  
  expect_is(inst <<- HybridArray$new(list(list(a = 1)), list(b = 2)), 
    "HybridArray")
  expect_identical(inst$.array$._list, list(list(a = 1)))
  expect_identical(as.list(inst$.array), list(b = 2))
  
  expect_is(inst <<- HybridArray$new(list(list(1, a = 1))), "HybridArray")
  expect_identical(inst$.array$._list, list(list(1, a = 1)))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
  expect_is(inst <<- HybridArray$new(list(list(1, 2, a = 3))), "HybridArray")
  expect_identical(inst$.array$._list, list(list(1, 2, a = 3)))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
})

test_that("HybridArray/initialize/explicit/list/mixed/force", {
  
  expect_is(inst <<- HybridArray$new(list(1), list(a = 2), force = TRUE), 
    "HybridArray")
  expect_identical(as.list(inst$.array), list(a = 2, "1" = 1))
  
  expect_is(inst <<- HybridArray$new(list(1, 2, a = 3), force = TRUE), 
    "HybridArray")
  expect_identical(as.list(inst$.array), list(a = 3, "1" = 1, "2" = 2))
  
  expect_is(inst <<- HybridArray$new(list(list(a = 1)), list(b = 2), force = TRUE), 
    "HybridArray")
  expect_identical(as.list(inst$.array), list(b = 2, "1" = list(a = 1)))
  
  expect_is(inst <<- HybridArray$new(list(list(1, a = 1)), force = TRUE), 
    "HybridArray")
  expect_identical(as.list(inst$.array), list("1" = list(1, a = 1)))
  
  expect_is(inst <<- HybridArray$new(list(list(1, 2, a = 3)), force = TRUE), 
    "HybridArray")
  expect_identical(as.list(inst$.array), list("1" = list(1, 2, a = 3)))
  
})

test_that("HybridArray/initialize/explicit/atomic/named", {
  
  expect_is(inst <<- HybridArray$new(a = 1, b = 2), "HybridArray")
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  
  envir <- new.env()
  expect_is(inst <<- HybridArray$new(a = 1, b = envir), "HybridArray")
  expect_identical(as.list(inst$.array), list(a = 1, b = envir))
  
  expect_is(inst <<- HybridArray$new(a = 1, b = list(a = 1, b = 2)), 
    "HybridArray")
  expect_identical(as.list(inst$.array), list(a = 1, b = list(a = 1, b = 2)))
  
})

test_that("HybridArray/initialize/explicit/atomic/unnamed", {
  
  expect_is(inst <<- HybridArray$new(1, 2), "HybridArray")
  expect_identical(inst$.array$._list, list(1, 2))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
  envir <- new.env()
  expect_is(inst <<- HybridArray$new(1, envir), "HybridArray")
  expect_identical(inst$.array$._list, list(1, envir))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
})

test_that("HybridArray/initialize/explicit/atomic/unnamed/force", {
  
  expect_is(inst <<- HybridArray$new(1, 2, force = TRUE), "HybridArray")
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2))
  
  envir <- new.env()
  expect_is(inst <<- HybridArray$new(1, envir, force = TRUE), "HybridArray")
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = envir))
  
})

test_that("HybridArray/initialize/explicit/mixed", {
  
  expect_is(inst <<- HybridArray$new(a = 1, 2), "HybridArray")
  expect_identical(inst$.array$._list, list(2))
  expect_identical(as.list(inst$.array), list(a = 1))

  envir <- new.env()
  expect_is(inst <<- HybridArray$new(a = 1, list(b = 2), envir),
    "HybridArray")
  expect_identical(inst$.array$._list, list(list(b = 2), envir))
  expect_identical(as.list(inst$.array), list(a = 1))
  
})

test_that("HybridArray/initialize/explicit/mixed/force", {
  
  expect_is(inst <<- HybridArray$new(a = 1, 2, force = TRUE), "HybridArray")
  expect_identical(as.list(inst$.array), list(a = 1, "1" = 2))

  envir <- new.env()
  expect_is(inst <<- HybridArray$new(a = 1, list(b = 2), envir, force = TRUE),
    "HybridArray")
  expect_identical(as.list(inst$.array), 
    list(a = 1, "1" = list(b = 2), "2" = envir))
  
})


##------------------------------------------------------------------------------
context("HybridArray/add")
##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
context("HybridArray/add/list/single")
##------------------------------------------------------------------------------

test_that("HybridArray/add/list/single/named", {
  
  inst <- HybridArray$new()
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

test_that("HybridArray/add/list/single/unnamed", {
  
  inst <- HybridArray$new()
  expect_true(inst$add(list(1)))
  expect_identical(inst$.array$._list, list(1))
  
  expect_true(inst$add(2))
  expect_identical(inst$.array$._list, list(1, 2))
  expect_true(inst$add(2))
  expect_identical(inst$.array$._list, list(1, 2, 2))
  
  expect_true(inst$add(list(3)))
  expect_identical(inst$.array$._list, list(1, 2, 2, 3))
  
})

test_that("HybridArray/add/list/single/unnamed/force", {
  
  inst <- HybridArray$new()
  expect_true(inst$add(list(1), force = TRUE))
  expect_identical(as.list(inst$.array), list("1" = 1))
  expect_true(inst$add(2, force = TRUE))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2))
  expect_true(inst$add(2, force = TRUE))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2, "3" = 2))  
  expect_true(inst$add(list(3), force = TRUE))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2, "3" = 2, "4" = 3))
  
})

test_that("HybridArray/add/list/single/unnamed/data frame", {
  
  inst <- HybridArray$new()
  expect_true(inst$add(data.frame(1:3)))
  expect_identical(inst$.array$._list, list(data.frame(1:3)))
  
})

test_that("HybridArray/add/list/single/unnamed/data frame/force", {
  
  inst <- HybridArray$new()
  expect_true(inst$add(data.frame(1:3), force = TRUE))
  expect_identical(as.list(inst$.array), list("1" = data.frame(1:3)))
  
})

test_that("HybridArray/add/list/single/mixed", {
  
  inst <- HybridArray$new()
  expect_true(inst$add(list(1, a = 1)))
  expect_identical(inst$.array$._list, list(1))
  expect_identical(as.list(inst$.array), list(a = 1))
  
})

##------------------------------------------------------------------------------
context("HybridArray/add/list/multiple")
##------------------------------------------------------------------------------


test_that("HybridArray/add/list/multiple/named", {
  
  inst <- HybridArray$new()
  expect_true(all(inst$add(list(a = 1), list(b = 2))))
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))

})

test_that("HybridArray/add/list/multiple/unnamed", {
  
  inst <- HybridArray$new()
  expect_true(all(inst$add(list(1), list(2))))
  expect_identical(inst$.array$._list, list(1, 2))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))

})

test_that("HybridArray/add/list/multiple/unnamed/force", {
  
  inst <- HybridArray$new()
  expect_true(all(inst$add(list(1), list(2), force = TRUE)))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2))

})

test_that("HybridArray/add/list/multiple/mixed", {
  
  inst <- HybridArray$new()
  expect_true(all(inst$add(list(1), list(a = 2))))
  expect_identical(inst$.array$._list, list(1))
  expect_identical(as.list(inst$.array), list(a = 2))

})

##------------------------------------------------------------------------------
context("HybridArray/add/atomic/single")
##------------------------------------------------------------------------------

test_that("HybridArray/add/atomic/single/named", {

  inst <- HybridArray$new()
  envir <- new.env()
  expect_true(inst$add(a = envir))
  expect_identical(as.list(inst$.array), list(a = envir))
  expect_true(inst$add(b = 1))
  expect_identical(as.list(inst$.array), list(a = envir, b = 1))
  
})

test_that("HybridArray/add/atomic/single/unnamed", {
  
  inst <- HybridArray$new()
  expect_true(inst$add(1))
  expect_identical(inst$.array$._list, list(1))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  expect_true(inst$add(1))
  expect_identical(inst$.array$._list, list(1, 1))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
  envir <- new.env()
  inst <- HybridArray$new()
  expect_true(inst$add(envir))
  expect_identical(inst$.array$._list, list(envir))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
})

test_that("HybridArray/add/atomic/single/unnamed/force", {
  
  inst <- HybridArray$new()
  expect_true(inst$add(1, force = TRUE))
  expect_identical(as.list(inst$.array), list("1" = 1))
  expect_true(inst$add(1, force = TRUE))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 1))
  
  envir <- new.env()
  inst <- HybridArray$new()
  expect_true(inst$add(envir, force = TRUE))
  expect_identical(as.list(inst$.array), list("1" = envir))
  
})

test_that("HybridArray/add/atomic/single/id", {  
  
  inst <- HybridArray$new()
  expect_true(all(inst$add(1, id = "a")))
  expect_identical(as.list(inst$.array), list(a = 1))
  expect_true(all(inst$add(1, id = "b")))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1))
  
  expect_false(all(inst$add(10, id = "b", overwrite = FALSE)))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1))
  expect_true(all(inst$add(2, id = "b")))
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  
  inst <- HybridArray$new()
  expect_false(inst$add(1, id = c("a", "b")))
  expect_warning(expect_false(inst$add(1, id = c("a", "b"), strict = 1)))
  expect_error(inst$add(1, id = c("a", "b"), strict = 2))
  
})

##------------------------------------------------------------------------------
context("HybridArray/add/atomic/multiple")
##------------------------------------------------------------------------------

test_that("HybridArray/add/atomic/multiple/named", {
  
  inst <- HybridArray$new()
  expect_true(all(inst$add(a = 1, b = 2)))
  expect_identical(as.list(inst$.array), list(a = 1, b = 2))
  
  envir <- new.env()
  inst <- HybridArray$new()
  expect_true(all(inst$add(a = envir, b = envir)))
  expect_identical(as.list(inst$.array), list(a = envir, b = envir))
  
})

test_that("HybridArray/add/atomic/multiple/unnamed", {
  
  inst <- HybridArray$new()
  expect_true(all(inst$add(1:3)))
  expect_identical(inst$.array$._list, list(1, 2, 3))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
  envir <- new.env()
  inst <- HybridArray$new()
  expect_true(all(inst$add(envir, envir)))
  expect_identical(inst$.array$._list, list(envir, envir))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
})

test_that("HybridArray/add/atomic/multiple/unnamed/force", {
  
  inst <- HybridArray$new()
  expect_true(all(inst$add(1:3, force = TRUE)))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2, "3" = 3))
  
  envir <- new.env()
  inst <- HybridArray$new()
  expect_true(all(inst$add(envir, envir, force = TRUE)))
  expect_identical(as.list(inst$.array), list("1" = envir, "2" = envir))
  
})

test_that("HybridArray/add/atomic/multiple/mixed", {
  
  inst <- HybridArray$new()
  expect_true(all(inst$add(1:3, a = 1, b = 1)))
  expect_identical(inst$.array$._list, list(1, 2, 3))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1))
  
  envir <- new.env()
  inst <- HybridArray$new()
  expect_true(all(inst$add(envir, envir, a = envir, b = envir)))
  expect_identical(inst$.array$._list, list(envir, envir))
  expect_identical(as.list(inst$.array), list(a = envir, b = envir))
  
  df <- data.frame(1:3)
  inst <- HybridArray$new()
  expect_true(all(inst$add(df, df, a = df, b = df)))
  expect_identical(inst$.array$._list, list(df, df))
  expect_identical(as.list(inst$.array), list(a = df, b = df))
  
})

test_that("HybridArray/add/atomic/multiple/id", {  
  
  inst <- HybridArray$new()
  expect_true(all(inst$add(1, 1, 1, id = c("c", "d", "e"))))
  expect_identical(as.list(inst$.array), list(c = 1, d = 1, e = 1))
  
  expect_false(all(inst$add(1, 1, 1, id = c("e", "f"), overwrite = FALSE)))
  expect_identical(as.list(inst$.array), list(c = 1, d = 1, e = 1))
  
  inst <- HybridArray$new()
  expect_false(inst$add(1, 1, 1, id = c("a", "b")))
  expect_warning(expect_false(inst$add(1, 1, 1, id = c("a", "b"), strict = 1)))
  expect_error(inst$add(1, 1, 1, id = c("a", "b"), strict = 2))
  
})

##------------------------------------------------------------------------------
context("HybridArray/set")
##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
context("HybridArray/set/list")
##------------------------------------------------------------------------------

test_that("HybridArray/set/list/single/named", {
  
  inst <- HybridArray$new(list(a = 1))
  expect_true(inst$set(list(a = 2)))
  expect_identical(as.list(inst$.array), list(a = 2))
  expect_false(inst$set(list(b = 2)))
  expect_warning(expect_false(inst$set(list(b = 2), strict = 1)))
  expect_error(inst$set(list(b = 2), strict = 2))
  expect_identical(as.list(inst$.array), list(a = 2))
  
})

test_that("HybridArray/set/list/single/unnamed", {
  
  inst <- HybridArray$new(list(1))
  expect_true(inst$set(list(2)))
  expect_identical(inst$.array$._list, list(1, 2))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
})

test_that("HybridArray/set/list/single/unnamed/force", {
  
  inst <- HybridArray$new(list(1), force = TRUE)
  expect_true(inst$set(list(2), force = TRUE))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2))
  
})

test_that("HybridArray/set/list/multiple/named", {
  
  inst <- HybridArray$new(list(a = 1, b = 1))
  expect_identical(inst$set(list(a = 2), list(b = 2)), c(a = TRUE, b = TRUE))
  expect_identical(as.list(inst$.array), list(a = 2, b = 2))
  
  inst <- HybridArray$new(list(a = 1, b = 1, c = 1))
  expect_true(all(inst$set(2, 2, 2, id = c("a", "b", "c"))))
  expect_identical(as.list(inst$.array), list(a = 2, b = 2, c = 2))
  
})

test_that("HybridArray/set/list/multiple/unnamed", {
  
  inst <- HybridArray$new(list(1, 1))
  expect_identical(inst$set(list(2), list(2)), 
    c(TRUE, TRUE))
  expect_identical(inst$.array$._list, list(1, 1, 2, 2))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
  inst <- HybridArray$new(list(1, 1, 1))
  expect_true(all(inst$set(2, 2, 2, id = c("a", "b", "c"))))
  expect_identical(as.list(inst$.array), list(a = 2, b = 2, c = 2))
  
})


test_that("HybridArray/set/list/multiple/unnamed/force", {
  
  inst <- HybridArray$new(list(1, 2), force = TRUE)
  expect_identical(inst$set(list(2), list(2), force = TRUE), 
    c("3" = TRUE, "4" = TRUE))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 2, "3" = 2, "4" = 2))
  
  inst <- HybridArray$new(list(a = 1, b = 1, c = 1), force = TRUE)
  expect_true(all(inst$set(2, 2, 2, id = c("a", "b", "c")), force = TRUE))
  expect_identical(as.list(inst$.array), list(a = 2, b = 2, c = 2))
  
})

test_that("HybridArray/set/list/new", {
  
  expect_is(inst <<- HybridArray$new(list(a = 1)), "HybridArray")
  expect_true(inst$set(list(b = 1), must_exist = FALSE))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1))
  expect_identical(inst$set(list(c = 1, d = 1), must_exist = FALSE), 
    c(c = TRUE, d = TRUE))

})

test_that("HybridArray/set/atomic/single/named", {
  
  expect_is(inst <<- HybridArray$new(a = 1), "HybridArray")
  expect_true(inst$set(a = 10))
  expect_identical(as.list(inst$.array), list(a = 10))
  
  expect_is(inst <<- HybridArray$new(a = 1), "HybridArray")
  expect_true(inst$set(b = 1, must_exist = FALSE))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1))
  
})

test_that("HybridArray/set/atomic/single/unnamed", {
  
  expect_is(inst <<- HybridArray$new(1), "HybridArray")
  expect_true(inst$set(10))
  expect_identical(as.list(inst$.array), list("1" = 1, "2" = 10))
  
})

test_that("HybridArray/set/atomic/multiple/named", {
  
  expect_is(inst <<- HybridArray$new(a = 1, b = 1), "HybridArray")
  expect_true(all(inst$set(a = 10, b = 10)))
  expect_identical(as.list(inst$.array), list(a = 10, b = 10))
  
  expect_is(inst <<- HybridArray$new(a = 1, b = 1), "HybridArray")
  expect_true(all(inst$set(a = 10, b = 10, c = 1, must_exist = FALSE)))
  expect_identical(as.list(inst$.array), list(a = 10, b = 10, c = 1))
  
})

test_that("HybridArray/set/atomic/id", {
  
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1, c = 1)), 
    "HybridArray")
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
context("HybridArray/get")
##------------------------------------------------------------------------------

test_that("HybridArray/get/all", {  

  inst <- HybridArray$new()
  expect_equal(as.list(inst$get()), structure(list(), names = character()))
  
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1)), 
    "HybridArray")
  expect_equal(as.list(inst$get()), list(a = 1, b = 1))
  expect_equal(inst$get(list = TRUE), list(a = 1, b = 1))
  
})

test_that("HybridArray/get/character", {  
  
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1)), 
    "HybridArray")
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

test_that("HybridArray/get/order", {  

  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1, 
    "1" = 1, "10" = 1, "2" = 1, "20" = 1, .a = 1)), 
    "HybridArray")
  expect_equal(as.list(inst$.array, all.names = TRUE), 
    list("20" = 1, a = 1, b = 1, "10" = 1, "1" = 1, .a = 1, "2" = 1))
  expect_equal(as.list(inst$get(), all.names = TRUE), 
    list("20" = 1, a = 1, b = 1, "10" = 1, "1" = 1, .a = 1, "2" = 1))
  expect_equal(inst$get(list = TRUE), 
    list("1" = 1, "2" = 1, "10" = 1, "20" = 1, .a = 1, a = 1, b = 1))
  expect_equal(inst$get(list = TRUE, all_names = TRUE, sorted = FALSE), 
    list(.a = 1, "1" = 1, "10" = 1, "2" = 1, "20" = 1, a = 1, b = 1))
  
})

test_that("HybridArray/get/numeric", {  

  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1, 
    "1" = 1, "10" = 1, "2" = 1, "20" = 1, .a = 1)), 
    "HybridArray")

  expect_equal(inst$get(1:5), list("1" = 1, "2" = 1, "10" = 1, "20" = 1, a = 1))
  expect_equal(inst$get(1:3, char = TRUE), list("1" = 1, "2" = 1, "3" = NULL))
  
})

##------------------------------------------------------------------------------
context("HybridArray/exists")
##------------------------------------------------------------------------------

test_that("HybridArray/exists/character", {  
  
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1)), "HybridArray")
  expect_true(inst$exists("a"))
  expect_true(all(inst$exists(c("a", "b"))))
  expect_true(all(inst$exists("a", "b")))
  expect_false(inst$exists("c"))
  
})

test_that("HybridArray/exists/numerical", {  
  
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1)), "HybridArray")
  expect_true(inst$exists(1))
  expect_true(all(inst$exists(c(1, 2))))
  expect_true(all(inst$exists(1, 2)))
  expect_false(inst$exists(3))
  
  expect_is(inst <<- HybridArray$new(list("1" = 1, b = 1)), "HybridArray")
  expect_equivalent(inst$exists(1, 2, char = TRUE), c(TRUE, FALSE))
  
})

##------------------------------------------------------------------------------
context("HybridArray/index")
##------------------------------------------------------------------------------

test_that("HybridArray/index", {  
  
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1)), "HybridArray")
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
context("HybridArray/clear")
##------------------------------------------------------------------------------

test_that("HybridArray/clear", {  
  
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1)), "HybridArray")
  expect_true(inst$clear())
  expect_is(res <- inst$.array, "environment")
  expect_identical(ls(res, all.names = TRUE), character())
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
})  
  
##------------------------------------------------------------------------------
context("HybridArray/remove")
##------------------------------------------------------------------------------

test_that("HybridArray/remove/character", {  
  
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1, c = 1)), "HybridArray")
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
  
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1, c = 1)), 
    "HybridArray")
  expect_true(all(inst$rm("a", "c")))
  expect_identical(inst$get(list = TRUE), list(b = 1))
  
})

test_that("HybridArray/remove/numeric", {  
  
  ## Sorted //
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1, 
    "1" = 1, "10" = 1, "2" = 1, "20" = 1, .a = 1)), 
    "HybridArray")
  expect_identical(inst$get(list = TRUE), 
    list("1" = 1, "2" = 1, "10" = 1, "20" = 1, .a = 1, a = 1, b = 1))
  expect_true(all(inst$rm(id = 1:2)))
#   inst$get(list = TRUE)

  ## Unsorted //
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1, 
    "1" = 1, "10" = 1, "2" = 1, "20" = 1, .a = 1)), 
    "HybridArray")
#   inst$get(list = TRUE, sorted = FALSE, all_names = TRUE)
  expect_true(all(inst$rm(id = 1:2, sorted = FALSE, all_names = TRUE)))
#   inst$get(list = TRUE, sorted = FALSE, all_names = TRUE)
  
  inst <<- HybridArray$new(list(a = 1, b = 1, c = 1))
  expect_true(all(inst$rm(1, 3)))
  expect_identical(inst$get(list = TRUE), list(a = 1, b = 1, c = 1))

  inst <<- HybridArray$new(list(a = 1, b = 1, c = 1))
  expect_true(all(inst$rm(1, 3, numonly = FALSE)))
  expect_identical(inst$get(list = TRUE), list(b = 1))

})

test_that("HybridArray/remove/numeric/auto-adjust", {  

  expect_is(inst <<- HybridArray$new("1" = 1, a = 1, "2" = 2, 
    b = 1, "3" = 3, "4" = 4, "5" = 5), "HybridArray")
  expect_true(all(inst$rm(1:2)))
  expect_identical(inst$get(list = TRUE), 
    list("1" = 3, "2" = 4, "3" = 5, a = 1, b = 1))

  expect_is(inst <<- HybridArray$new("1" = 1, a = 1, "2" = 2, 
    b = 1, "3" = 3, "4" = 4, "5" = 5), "HybridArray")
  expect_true(all(inst$rm(2, 4)))
  expect_identical(inst$get(list = TRUE), 
    list("1" = 1, "2" = 3, "3" = 5, a = 1, b = 1))

  expect_is(inst <<- HybridArray$new("1" = 1, a = 1, "2" = 2, 
    b = 1, "3" = 3, "4" = 4, "5" = 5), "HybridArray")
  expect_true(all(inst$rm(2, 6)))
  expect_identical(inst$get(list = TRUE), 
    list("1" = 1, "2" = 3, "3" = 4, "4" = 5, a = 1, b = 1))
  
  expect_is(inst <<- HybridArray$new("1" = 1, a = 1, "2" = 2, 
    b = 1, "3" = 3, "4" = 4, "5" = 5), "HybridArray")
  expect_true(all(inst$rm(2, 6, numonly = FALSE)))
  expect_identical(inst$get(list = TRUE), 
    list("1" = 1, "2" = 3, "3" = 4, "4" = 5, b = 1))
  
  expect_is(inst <<- HybridArray$new("1" = 1, a = 1, "2" = 2, 
    b = 1, "3" = 3, "4" = 4, "5" = 5), "HybridArray")
  expect_true(all(inst$rm(1:5)))
  expect_identical(inst$get(list = TRUE), 
    list(a = 1, b = 1))
  
  expect_is(inst <<- HybridArray$new("1" = 1, a = 1, "2" = 2, 
    b = 1, "3" = 3, "4" = 4, "5" = 5), "HybridArray")
  expect_true(all(inst$rm(1:6)))
  expect_identical(inst$get(list = TRUE), 
    list(a = 1, b = 1))
  
  expect_is(inst <<- HybridArray$new("1" = 1, a = 1, "2" = 2, 
    b = 1, "3" = 3, "4" = 4, "5" = 5), "HybridArray")
  expect_true(all(inst$rm(1:6, numonly = FALSE)))
  expect_identical(inst$get(list = TRUE), list(b = 1))
  
})

##------------------------------------------------------------------------------
context("HybridArray/remove first")
##------------------------------------------------------------------------------

test_that("HybridArray/remove first", {  
  
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1, c = 1)), "HybridArray")
  expect_true(inst$rmFirst())
  expect_identical(as.list(inst$.array), list(b = 1, c = 1))
  expect_true(all(inst$rmFirst(2)))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1, c = 1)), "HybridArray")
  expect_identical(inst$rmFirst(4), structure(c(rep(TRUE, 3), FALSE), names = 1:4))
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1, c = 1)), "HybridArray")
  expect_warning(expect_identical(inst$rmFirst(4, strict = 1), 
    structure(c(rep(TRUE, 3), FALSE), names = 1:4)))
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1, c = 1)), "HybridArray")
  expect_error(inst$rmFirst(4, strict = 2))
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1, c = 1)), "HybridArray")
  expect_false(inst$rmFirst(4, simplify = TRUE))
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1, c = 1)), "HybridArray")
  expect_true(inst$rmFirst(3, simplify = TRUE))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
  expect_true(inst$rmFirst())
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
    
})  

##------------------------------------------------------------------------------
context("HybridArray/remove last")
##------------------------------------------------------------------------------

test_that("HybridArray/remove last", {  
  
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1, c = 1)), "HybridArray")
  expect_identical(inst$rmLast(), c("3" = TRUE))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1))
  expect_identical(inst$rmLast(2), c("2" = TRUE, "1" = TRUE))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
  ## Simplify //
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1, c = 1)), "HybridArray")
  expect_true(inst$rmLast(simplify = TRUE))
  expect_true(inst$rmLast(2, simplify = TRUE))
  
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1, c = 1)), "HybridArray")
  expect_identical(inst$rmLast(4), 
    structure(c(rep(TRUE, 3), FALSE), names = 3:0))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1, c = 1)), "HybridArray")
  expect_warning(expect_identical(inst$rmLast(4, strict = 1), 
    structure(c(rep(TRUE, 3), FALSE), names = 3:0)))
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1, c = 1)), "HybridArray")
  expect_error(inst$rmLast(4, strict = 2))
  
  ## Simplify //
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1, c = 1)), "HybridArray")
  expect_false(inst$rmLast(4, simplify = TRUE))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
  inst <- HybridArray$new()
  expect_false(inst$rmLast())
  expect_warning(expect_false(inst$rmLast(strict = 1)))
  expect_error(inst$rmLast(strict = 2))
  expect_false(all(inst$rmLast(2)))
  expect_warning(expect_false(all(inst$rmLast(strict = 1))))
  expect_error(inst$rmLast(strict = 2))
  expect_identical(as.list(inst$.array), structure(list(), names = character()))
  
  ## Simplify //
  inst <- HybridArray$new()
  expect_false(inst$rmLast(simplify = TRUE))
  expect_warning(expect_false(inst$rmLast(strict = 1, simplify = TRUE)))
  expect_false(inst$rmLast(2, simplify = TRUE))
  expect_warning(expect_false(inst$rmLast(strict = 1, simplify = TRUE)))
  
})

##------------------------------------------------------------------------------
context("HybridArray/copy")
##------------------------------------------------------------------------------

test_that("HybridArray/copy/single/character", { 
  
  expect_is(inst <<- HybridArray$new(list(a = 1)), "HybridArray")
  expect_true(inst$copy("a", "b"))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1))
  
  expect_false(inst$copy("c", "d"))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1))
  expect_warning(expect_false(inst$copy("c", "d", strict = 1)))
  expect_error(inst$copy("c", "d", strict = 2))

})

test_that("HybridArray/copy/single/numeric", { 
  
  expect_is(inst <<- HybridArray$new(list(a = 1)), "HybridArray")
  expect_true(inst$copy(1, 2))
  expect_identical(as.list(inst$.array), list(a = 1, "2" = 1))
  
  expect_false(inst$copy(3, 4))
  expect_identical(as.list(inst$.array), list(a = 1, "2" = 1))
  expect_warning(expect_false(inst$copy(3, 4, strict = 1)))
  expect_error(inst$copy(3, 4, strict = 2))

})

test_that("HybridArray/copy/multiple/character", { 
  
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1)), 
    "HybridArray")
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

test_that("HybridArray/copy/multiple/numeric", { 
  
  expect_is(inst <<- HybridArray$new(list(a = 1, b = 1)), 
    "HybridArray")
  expect_true(all(inst$copy(c(1, 2), c(3, 4))))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1, "3" = 1, "4" = 1))
  expect_false(all(inst$copy(c(1, 2), c(3, 4))))
  expect_identical(as.list(inst$.array), list(a = 1, b = 1, "3" = 1, "4" = 1))
  expect_warning(expect_false(all(inst$copy(c(1, 2), c(3, 4), strict = 1))))
  expect_error(all(inst$copy(c(1, 2), c(3, 4), strict = 2)))
  expect_true(all(inst$copy(c(1, 2), c(3, 4), overwrite = TRUE)))
  expect_identical(as.list(inst$get()), list(a = 1, b = 1, "3" = 1, "4" = 1))

  expect_false(all(inst$copy(c(1, 2), 4)))  
  expect_warning(expect_false(all(inst$copy(c(1, 2), 4, strict = 1))))
  expect_error(all(inst$copy(c(1, 2), 4, strict = 2)))
  
})

test_that("HybridArray/apply", { 
  
  expect_is(inst <<- HybridArray$new(1:10), "HybridArray")  
  fun <- function(x) x * 10
  expect_identical(inst$apply(x = 1:5, fun), 
    structure(as.list(seq(10, 50, 10)), names = 1:5))
  
})

test_that("HybridArray/mapReduce", { 
  
  expect_is(inst <<- HybridArray$new(1:10), "HybridArray")  
  expect_identical(inst$mapReduce(x = 1:5, mean), 3)
  
})

################################################################################
################################################################################
################################################################################

##------------------------------------------------------------------------------
context("HybridArray/private/.addToList")
##------------------------------------------------------------------------------

test_that("HybridArray/private/.addToList", {  

  inst <- HybridArray$new()
  expect_true(inst$.addToList(list(1)))
  expect_identical(inst$.array$._list, list(1))
  
})

##------------------------------------------------------------------------------
context("HybridArray/private/.autoadjustNumericKeys")
##------------------------------------------------------------------------------

test_that("HybridArray/private/.autoadjustNumericKeys", {  
  
  inst <<- HybridArray$new("1" = 1, a = 1, "2" = 2, 
    b = 1, "3" = 3, "4" = 4, "5" = 5)
  rm(list = c("2", "4"), envir = inst$.array, inherits = FALSE)
  expect_identical(inst$get(list = TRUE), 
    list("1" = 1, "3" = 3, "5" = 5, a = 1, b = 1))
  expect_true(inst$.autoadjustNumericKeys())
  expect_identical(inst$get(list = TRUE), 
    list("1" = 1, "2" = 3, "3" = 5, a = 1, b = 1))
  
})

##------------------------------------------------------------------------------
context("HybridArray/private/.getNumericKeys")
##------------------------------------------------------------------------------

test_that("HybridArray/private/.getNumericKeys", {  
  
  inst <<- HybridArray$new("1" = 1, a = 1, "2" = 2, 
    b = 1, "3" = 3, "4" = 4, "5" = 5)
  expect_identical(inst$.getNumericKeys(), as.character(1:5))
  
  inst <- HybridArray$new(a = 1)
  expect_identical(inst$.getNumericKeys(), character())
  
  inst <- HybridArray$new()
  expect_identical(inst$.getNumericKeys(), character())
  
})

##------------------------------------------------------------------------------
context("HybridArray/private/.indexNumericKeys")
##------------------------------------------------------------------------------

test_that("HybridArray/private/.indexNumericKeys", {  
  
  inst <<- HybridArray$new("1" = 1, a = 1, "2" = 2, 
    b = 1, "3" = 3, "4" = 4, "5" = 5)
  expect_identical(inst$.indexNumericKeys(), 1:5)
  
  inst <- HybridArray$new(a = 1)
  expect_identical(inst$.indexNumericKeys(), integer())
  
  inst <- HybridArray$new()
  expect_identical(inst$.indexNumericKeys(), integer())
  
})
