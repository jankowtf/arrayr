##------------------------------------------------------------------------------
context("Array/initialize")
##------------------------------------------------------------------------------

test_that("Array/initialize/default", {
  
  expect_is(inst <- Array$new(), "Array")
  expect_identical(inst$.array, structure(list(), names = character()))
  
})

test_that("Array/initialize/explicit/list/named", {
  
  expect_is(inst <- Array$new(list(a = 1, b = 2)), "Array")
  expect_identical(inst$.array, list(a = 1, b = 2))
  expect_is(inst <- Array$new(list(a = 1), list(b = 2)), "Array")
  expect_identical(inst$.array, list(a = 1, b = 2))

})

test_that("Array/initialize/explicit/list/unnamed", {
  
  expect_is(inst <- Array$new(list(1, 2)), "Array")
  expect_equivalent(inst$.array, list(1, 2))
  expect_is(inst <- Array$new(list(1), list(2)), "Array")
  expect_equivalent(inst$.array, list(1, 2))

})

test_that("Array/initialize/explicit/list/mixed", {
  
  expect_is(inst <- Array$new(list(1, a = 2)), "Array")
  expect_equivalent(inst$.array, list(1, a = 2))
  expect_is(inst <- Array$new(list(1), list(a = 2)), "Array")
  expect_equivalent(inst$.array, list(1, a = 2))

})

test_that("Array/initialize/explicit/atomic/named", {
  
  expect_is(inst <- Array$new(a = 1, b = 2), "Array")
  expect_identical(inst$.array, list(a = 1, b = 2))
  
})

test_that("Array/initialize/explicit/atomic/unnamed", {
  
  expect_is(inst <- Array$new(1, 2), "Array")
  expect_equivalent(inst$.array, list(1, 2))
  
})

test_that("Array/initialize/explicit/atomic/mixed", {
  
  expect_is(inst <- Array$new(1, a = 2), "Array")
  expect_equivalent(inst$.array, list(1, a = 2))
  
})

test_that("Array/initialize/explicit/mixed", {
  
  expect_is(inst <- Array$new(list(1), list(a = 1), 1, b = 1), "Array")
  expect_identical(inst$.array, list(1, a = 1, 1, b = 1))
  
})

##------------------------------------------------------------------------------
context("Array/add")
##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
context("Array/add/list")
##------------------------------------------------------------------------------

test_that("Array/add/list/single/named", {
  
  inst <- Array$new()
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
  
  inst <- Array$new()
  value <- list(new.env())
  expect_true(inst$add(a = value))
  expect_identical(inst$.array, list(a = value))
  
  inst <- Array$new()
  value <- list(data.frame(1:3))
  expect_true(inst$add(a = value))
  expect_identical(inst$.array, list(a = value))
  
  inst <- Array$new()
  value <- list(list(1:3))
  expect_true(inst$add(b = value))
  expect_identical(inst$.array, list(b = value))
  
})

test_that("Array/add/list/single/unnamed", {
  
  inst <- Array$new()
  expect_true(inst$add(list(1)))
  expect_equivalent(inst$.array, list(1))
  expect_true(inst$add(list(2)))
  expect_equivalent(inst$.array, list(1, 2))
  
  expect_true(inst$add(list(2)))
  expect_equivalent(inst$.array, list(1, 2, 2))
  
  inst <- Array$new()
  value <- list(new.env())
  expect_true(inst$add(value))
  expect_equivalent(inst$.array, value)
  
  inst <- Array$new()
  value <- list(data.frame(1:3))
  expect_true(inst$add(value))
  expect_equivalent(inst$.array, value)
  
  inst <- Array$new()
  value <- list(list(1:3))
  expect_true(inst$add(value))
  expect_equivalent(inst$.array, value[[1]])
  
})

test_that("Array/add/list/multiple", {
  
  inst <- Array$new()
  expect_true(all(inst$add(list(a = 1), list(b = 2))))
  expect_identical(inst$.array, list(a = 1, b = 2))
  
})

test_that("Array/add/list/multiple/id/character", {
  
  inst <- Array$new()
  expect_true(all(inst$add(list(1), list(1), list(1), id = letters[1:3])))
  expect_identical(inst$.array, list(a = list(1), b = list(1), c = list(1)))
  
  expect_false(all(inst$add(list(1), list(1), list(1), id = c("e", "f"), 
    dups = FALSE)))
  expect_identical(inst$.array, list(a = list(1), b = list(1), c = list(1)))
  
  inst <- Array$new()
  expect_false(inst$add(list(1), list(1), list(1), id = c("a", "b")))
  expect_warning(expect_false(inst$add(list(1), list(1), list(1), 
    id = c("a", "b"), strict = 1)))
  expect_error(inst$add(list(1), list(1), list(1), id = c("a", "b"), strict = 2))
  
})

test_that("Array/add/list/multiple/id/numeric", {
  
  inst <- Array$new()
  expect_true(all(inst$add(list(1), list(1), list(1), id = 2:4)))
  expect_equivalent(inst$.array, list(NULL, 1, 1, 1))
  
})

##------------------------------------------------------------------------------
context("Array/add/atomic")
##------------------------------------------------------------------------------

test_that("Array/add/atomic/single/named", {
  
  inst <- Array$new()
  expect_true(inst$add(a = 1))
  expect_equivalent(inst$.array, list(a = 1))
  
  envir <- new.env()
  inst <- Array$new()
  expect_true(inst$add(a = envir))
  expect_equivalent(inst$.array, list(a = envir))
  
  df <- data.frame(1:3)
  inst <- Array$new()
  expect_true(inst$add(a = df))
  expect_equivalent(inst$.array, list(a = df))
  
  lst <- list(list(1:3))
  inst <- Array$new()
  expect_true(inst$add(a = lst))
  expect_equivalent(inst$.array, list(a = lst))
  
})

test_that("Array/add/atomic/single/unnamed", {
  
  inst <- Array$new()
  expect_true(inst$add(1))
  expect_equivalent(inst$.array, list(1))
  
  envir <- new.env()
  inst <- Array$new()
  expect_true(inst$add(envir))
  expect_equivalent(inst$.array, list(envir))
  
  df <- data.frame(1:3)
  inst <- Array$new()
  expect_true(inst$add(df))
  expect_equivalent(inst$.array, list(df))
  
  lst <- list(list(1:3))
  inst <- Array$new()
  expect_true(inst$add(lst))
  expect_equivalent(inst$.array, lst[[1]])
  
})

test_that("Array/add/atomic/single/id/character", {
  
  inst <- Array$new()
  expect_true(inst$add(1, id = "a"))
  expect_equivalent(inst$.array, list(a = 1))
  
  inst <- Array$new()
  expect_true(inst$add(1, id = "1"))
  expect_equivalent(inst$.array, list("1" = 1))
  
  envir <- new.env()
  inst <- Array$new()
  expect_true(inst$add(envir, id = "a"))
  expect_equivalent(inst$.array, list(a = envir))
  
  df <- data.frame(1:3)
  inst <- Array$new()
  expect_true(inst$add(df, id = "a"))
  expect_equivalent(inst$.array, list(a = df))
  
  lst <- list(list(1:3))
  inst <- Array$new()
  expect_true(inst$add(lst, id = "a"))
  expect_equivalent(inst$.array, list(a = list(lst[[1]])))
  
})

test_that("Array/add/atomic/single/id/numeric", {
  
  inst <- Array$new()
  expect_true(inst$add(1, id = 1))
  expect_equivalent(inst$.array, list(1))
  
  inst <- Array$new()
  expect_true(inst$add(1, id = 2))
  expect_equivalent(inst$.array, list(NULL, 1))
  
  envir <- new.env()
  inst <- Array$new()
  expect_true(inst$add(envir, id = 2))
  expect_equivalent(inst$.array, list(NULL, envir))
  
  df <- data.frame(1:3)
  inst <- Array$new()
  expect_true(inst$add(df, id = 2))
  expect_equivalent(inst$.array, list(NULL, df))
  
  lst <- list(list(1:3))
  inst <- Array$new()
  expect_true(inst$add(lst, id = 2))
  expect_equivalent(inst$.array, list(NULL, lst[[1]]))
  
})

test_that("Array/add/atomic/single/mixed", {
  
  envir <- new.env()
  df <- data.frame(1:3)
  lst <- list(list(1:3))
  
  inst <- Array$new()
  expect_true(all(inst$add(1, a = 1, envir, b = envir, 
    df, c = df, lst, d = lst)))
  expect_equivalent(inst$.array, 
    list(1, a = 1, envir, b = envir, df, c = df, lst[[1]], d = lst))
  
})

test_that("Array/add/atomic/multiple", {
  
  inst <- Array$new()
  expect_true(all(inst$add(a = 1, b = 2, c = 3)))
  expect_equivalent(inst$.array, list(a = 1, b = 2, c = 3))
  
  inst <- Array$new()
  expect_true(all(inst$add(1, 2, 3)))
  expect_equivalent(inst$.array, list(1, 2, 3))
  
})

test_that("Array/add/atomic/multiple/id/character", {
  
  inst <- Array$new()
  expect_true(all(inst$add(1, 2, 3, id = letters[1:3])))
  expect_equivalent(inst$.array, list(a = 1, b = 2, c = 3))
  
})

test_that("Array/add/atomic/multiple/id/numeric", {
  
  inst <- Array$new()
  expect_true(all(inst$add(1, 2, 3, id = 2:4)))
  expect_equivalent(inst$.array, list(NULL, 1, 2, 3))
  
})

##------------------------------------------------------------------------------
context("Array/set")
##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
context("Array/set/list")
##------------------------------------------------------------------------------

test_that("Array/set/list/single/named", {
  
  inst <- Array$new(list(a = 1))
  expect_true(inst$set(list(a = 2)))
  expect_identical(inst$.array, list(a = 2))
  expect_false(inst$set(list(b = 2)))
  expect_warning(expect_false(inst$set(list(b = 2), strict = 1)))
  expect_error(inst$set(list(b = 2), strict = 2))
  expect_identical(inst$.array, list(a = 2))
  
})

test_that("Array/set/list/single/unnamed", {
  
  inst <- Array$new(list(a = 1))
  expect_false(inst$set(list(2)))
  expect_warning(expect_false(inst$set(list(2), strict = 1)))
  expect_error(inst$set(list(2), strict = 2))
  expect_identical(inst$.array, list(a = 1))
  
})

test_that("Array/set/list/multiple", {
  
  inst <- Array$new(list(a = 1, b = 1))
  expect_identical(inst$set(list(a = 2), list(b = 2)), c(a = TRUE, b = TRUE))
  expect_identical(inst$.array, list(a = 2, b = 2))
  
})

test_that("Array/set/list/new", {
  
  inst <- Array$new(list(a = 1))
  expect_true(inst$set(list(b = 1), must_exist = FALSE))
  expect_identical(inst$.array, list(a = 1, b = 1))
  expect_identical(inst$set(list(c = 1, d = 1), must_exist = FALSE), 
    c(c = TRUE, d = TRUE))

})

##------------------------------------------------------------------------------
context("Array/set/atomic")
##------------------------------------------------------------------------------

test_that("Array/set/atomic/single/named", {
  
  inst <- Array$new(a = 1)
  expect_true(inst$set(a = 2))
  expect_identical(inst$.array, list(a = 2))
  expect_false(inst$set(b = 2))
  expect_warning(expect_false(inst$set(b = 2, strict = 1)))
  expect_error(inst$set(b = 2, strict = 2))
  expect_identical(inst$.array, list(a = 2))
  
})

test_that("Array/set/atomic/single/unnamed", {
  
  inst <- Array$new(1)
  expect_false(inst$set(2))
  expect_warning(expect_false(inst$set(2, strict = 1)))
  expect_error(inst$set(2, strict = 2))
  expect_equivalent(inst$.array, list(1))
  
})

test_that("Array/set/atomic/multiple", {
  
  inst <- Array$new(a = 1, b = 1)
  expect_identical(inst$set(a = 2, b = 2), c(a = TRUE, b = TRUE))
  expect_identical(inst$.array, list(a = 2, b = 2))
  
})

test_that("Array/set/atomic/new", {
  
  inst <- Array$new(a = 1)
  expect_true(inst$set(b = 1, must_exist = FALSE))
  expect_identical(inst$.array, list(a = 1, b = 1))
  expect_identical(inst$set(c = 1, d = 1, must_exist = FALSE), 
    c(c = TRUE, d = TRUE))

})

##------------------------------------------------------------------------------
context("Array/set/id")
##------------------------------------------------------------------------------

test_that("Array/set/list/single/id", {
  
  inst <- Array$new(list(a = 1))
  expect_true(inst$set(2, id = "a"))
  expect_identical(inst$.array, list(a = 2))
  
  expect_false(inst$set(1, id = c("a", "b")))
  expect_warning(expect_false(inst$set(1, id = c("a", "b"), strict = 1)))
  expect_error(inst$set(1, id = c("a", "b"), strict = 2))
  
})

test_that("Array/set/list/multiple/id", {
  
  inst <- Array$new(list(a = 1, b = 1, c = 1))
  expect_true(all(inst$set(2, 2, 2, id = c("a", "b", "c"))))
  expect_identical(inst$.array, list(a = 2, b = 2, c = 2))
  
  expect_identical(inst$set(2, 2, 2, id = c("a", "b", "d")), 
    c(a = TRUE, b = TRUE, d = FALSE))
  expect_identical(inst$.array, list(a = 2, b = 2, c = 2))
  expect_warning(expect_identical(inst$set(2, 2, 2, id = c("a", "b", "d"), strict = 1), 
    c(a = TRUE, b = TRUE, d = FALSE)))
  expect_error(inst$set(2, 2, 2, id = c("a", "b", "d"), strict = 2))

})

test_that("Array/set/list/id/invalid", {
  
  inst <- Array$new(list(a = 1, b = 1))
  expect_true(all(inst$set(2, 2, id = c("a", "b"))))
  expect_identical(inst$.array, list(a = 2, b = 2))
  
  expect_false(inst$set(2, 2, 2, id = c("b", "c")))
  expect_warning(expect_false(inst$set(2, 2, 2, id = c("b", "c"), strict = 1)))
  expect_error(inst$set(2, 2, 2, id = c("b", "c"), strict = 2))
  
})

##------------------------------------------------------------------------------
context("Array/copy")
##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
context("Array/copy/character")
##------------------------------------------------------------------------------

test_that("Array/copy/character/single", { 
  
  inst <- Array$new(list(a = 1))
  expect_true(inst$copy("a", "b"))
  expect_identical(inst$.array, list(a = 1, b = 1))

})

test_that("Array/copy/character/single/non-existing", { 
  
  inst <- Array$new(list(a = 1))
  expect_false(inst$copy("c", "d"))
  expect_identical(inst$.array, list(a = 1))
  expect_warning(expect_false(inst$copy("c", "d", strict = 1)))
  expect_error(inst$copy("c", "d", strict = 2))
  
})

test_that("Array/copy/character/multiple", { 
  
  inst <- Array$new(list(a = 1, b = 1))
  expect_true(all(inst$copy(c("a", "b"), c("c", "d"))))
  expect_identical(inst$.array, list(a = 1, b = 1, c = 1, d = 1))
  
})

test_that("Array/copy/character/multiple/duplicates", { 
  
  inst <- Array$new(list(a = 1, b = 1, c = 1, d = 1))
  inst$get()
  expect_false(all(inst$copy(c("a", "b"), c("c", "d"), dups = FALSE)))
  expect_identical(inst$.array, list(a = 1, b = 1, c = 1, d = 1))
  expect_warning(expect_false(all(inst$copy(c("a", "b"), c("c", "d"), 
    dups = FALSE, strict = 1))))
  expect_error(all(inst$copy(c("a", "b"), c("c", "d"), 
    dups = FALSE, strict = 2)))
  expect_true(all(inst$copy(c("a", "b"), c("c", "d"))))
  expect_identical(inst$get(), list(a = 1, b = 1, c = 1, d = 1, c = 1, d = 1))

})

test_that("Array/copy/character/multiple/lengths differ", { 
  
  inst <- Array$new(list(a = 1, b = 1))
  expect_false(all(inst$copy(c("a", "b"), "d")))  
  expect_warning(expect_false(all(inst$copy(c("a", "b"), "d", strict = 1))))
  expect_error(all(inst$copy(c("a", "b"), "d", strict = 2)))
  
})

##------------------------------------------------------------------------------
context("Array/copy/numeric")
##------------------------------------------------------------------------------

test_that("Array/copy/numeric/single", { 
  
  inst <- Array$new(list(a = 1))
  expect_true(inst$copy(1, 2))
  expect_identical(inst$.array, list(a = 1, 1))
  
  inst <- Array$new(list(a = 1))
  expect_true(inst$copy(1, "b"))
  expect_identical(inst$.array, list(a = 1, b = 1))

})

test_that("Array/copy/numeric/single/non-existing", { 
  
  inst <- Array$new(list(a = 1))
  expect_false(inst$copy(2, "d"))
  expect_identical(inst$.array, list(a = 1))
  expect_warning(expect_false(inst$copy(2, "d", strict = 1)))
  expect_error(inst$copy(2, "d", strict = 2))
  
})

test_that("Array/copy/numeric/multiple", { 
  
  inst <- Array$new(list(a = 1, b = 1))
  expect_true(all(inst$copy(c(1, 2), c(3, 4))))
  expect_identical(inst$.array, list(a = 1, b = 1, 1, 1))
  
  inst <- Array$new(list(a = 1, b = 1))
  expect_true(all(inst$copy(c(1, 2), c("c", "d"))))
  expect_identical(inst$.array, list(a = 1, b = 1, c = 1, d = 1))
  
})

test_that("Array/copy/numeric/multiple/duplicates", { 
  
  inst <- Array$new(list(a = 1, b = 1, c = 1, d = 1))
  expect_false(all(inst$copy(c(1, 2), c("c", "d"), dups = FALSE)))
  expect_identical(inst$.array, list(a = 1, b = 1, c = 1, d = 1))
  expect_warning(expect_false(all(inst$copy(c(1, 2), c("c", "d"), 
    dups = FALSE, strict = 1))))
  expect_error(all(inst$copy(c(1, 2), c("c", "d"), 
    dups = FALSE, strict = 2)))
  expect_true(all(inst$copy(c(1, 2), c("c", "d"))))
  expect_identical(inst$get(), list(a = 1, b = 1, c = 1, d = 1, c = 1, d = 1))

})

test_that("Array/copy/numeric/multiple/lengths differ", { 
  
  inst <- Array$new(list(a = 1, b = 1))
  expect_false(all(inst$copy(c(1, 2), "d")))  
  expect_warning(expect_false(all(inst$copy(c(1, 2), "d", strict = 1))))
  expect_error(all(inst$copy(c(1, 2), "d", strict = 2)))
  
})

##------------------------------------------------------------------------------
context("Array/get")
##------------------------------------------------------------------------------

test_that("Array/get/all", {  

  inst <- Array$new()
  expect_equal(inst$get(), structure(list(), names = character()))
  
  inst <- Array$new(list(a = 1, b = 1))
  expect_equal(inst$get(), list(a = 1, b = 1))

})

test_that("Array/get/all/sorting", {  
  
  inst <- Array$new(list(a = 1, "1" = 1, "10" = 1, b = 1, 
    "20" = 1, "2" = 1, .a = 1))
  expect_equal(inst$get(), list(a = 1, "1" = 1, "10" = 1, b = 1, 
    "20" = 1, "2" = 1, .a = 1))
  expect_equal(inst$get(sorting = 1), list("1" = 1, "2" = 1, "10" = 1, 
    "20" = 1, .a = 1, a = 1, b = 1))
  expect_equal(inst$get(sorting = 2), list(b = 1 ,a = 1, .a = 1, 
    "20" = 1, "10" = 1, "2" = 1, "1" = 1))
  
})

test_that("Array/get/character/single", {  
  
  inst <- Array$new(list(a = 1, b = 1))
  expect_identical(inst$get("a"), 1)
  expect_identical(inst$get("a", inner = FALSE), list(a = 1))
  
  expect_identical(inst$get("c"), NULL)
  expect_identical(inst$get("c", inner = FALSE), NULL)
  expect_warning(expect_identical(inst$get("c", strict = 1), NULL))
  expect_error(inst$get("c", strict = 2))
  
})

test_that("Array/get/character/multiple/regular", {  
  
  inst <- Array$new(list(a = 1, b = 1))
  expect_identical(inst$get(c("a", "b")), list(a = 1, b = 1))
  expect_identical(inst$get(c("a", "b"), inner = FALSE), 
    list(a = list(a = 1), b = list(b = 1)))
  
})

test_that("Array/get/character/multiple/sorted/1", {  
  
  inst <- Array$new(list(a = 1, "1" = 2, "10" = 1, b = 1, 
    "20" = 1, "2" = 1, .a = 1))
  expect_identical(inst$get(c("a", "2", "b")), list(a = 1, "2" = 1, b = 1))
  expect_identical(inst$get(c("a", "2", "b"), sorting = 1), 
    list("2" = 1, a = 1, b = 1))
  expect_identical(inst$get(c("a", "2", "b"), sorting = 2), 
    list(b = 1, a = 1, "2" = 1))
  
})

test_that("Array/get/character/multiple/non-existing", {    
  
  inst <- Array$new(list(a = 1, b = 1))
  expect_identical(inst$get(c("a", "c")), list(a = 1, c = NULL))
  expect_identical(inst$get(c("a", "c"), inner = FALSE), 
    list(a = list(a = 1), c = NULL))
  expect_warning(expect_identical(inst$get(c("a", "c"), strict = 1), 
    list(a = 1, c = NULL)))
  expect_error(inst$get(c("a", "c"), strict = 2))
  
  expect_identical(inst$get(c("a", "c"), simplify = TRUE), list(a = 1))
  
  expect_identical(inst$get(c("c", "d")), list(c = NULL, d = NULL))
  expect_identical(inst$get(c("c", "d"), simplify = TRUE), list())
  
})

test_that("Array/get/numeric/single/regular", {  
  
  inst <- Array$new(list(a = 1, "1" = 2, "10" = 3, b = 4, 
    "20" = 5, "2" = 6, .a = 7))
  expect_identical(inst$get(2), 2)
  expect_identical(inst$get(2, inner = FALSE), list("1" = 2))

})

test_that("Array/get/numeric/single/sorted", {  
  
  inst <- Array$new(list(a = 1, "1" = 2, "10" = 1, b = 1, 
    "20" = 1, "2" = 1, .a = 1))
  expect_identical(inst$get(2, sorting = 1), 1)
  expect_identical(inst$get(2, inner = FALSE, sorting = 1), list("2" = 1))
  
})

test_that("Array/get/numeric/single/non-existing", {  
  
  inst <- Array$new(list(a = 1, b = 1))
  expect_identical(inst$get(3), NULL)
  expect_identical(inst$get(3, inner = FALSE), NULL)
  expect_warning(expect_identical(inst$get("c", strict = 1), NULL))
  expect_error(inst$get("c", strict = 2))
  
})

test_that("Array/get/numeric/multiple/regular", {  
  
  inst <- Array$new(list(a = 1, "1" = 2, "10" = 1, b = 1, 
    "20" = 1, "2" = 1, .a = 1))
  expect_identical(inst$get(1, 2), list(a = 1, "1" = 2))
  expect_identical(inst$get(1, 2, inner = FALSE), 
    list(a = list(a = 1), "1" = list("1" = 2)))
  
  expect_identical(inst$get(2, 1), list("1" = 2, a = 1))
  expect_identical(inst$get(2, 1, inner = FALSE), 
    list("1" = list("1" = 2), a = list(a = 1)))
  
})

test_that("Array/get/numeric/multiple/sorted/1", {  
  
  inst <- Array$new(list(a = 1, "1" = 2, "10" = 1, b = 1, 
    "20" = 1, "2" = 1, .a = 1))
  expect_identical(inst$get(1, 2, sorting = 1), list("1" = 2, "2" = 1))
  expect_identical(inst$get(1, 2, inner = FALSE, sorting = 1), 
    list("1" = list("1" = 2), "2" = list("2" = 1)))
  
  expect_identical(inst$get(2, 1, sorting = 1), list("1" = 2, "2" = 1))
  expect_identical(inst$get(2, 1, inner = FALSE, sorting = 1), 
    list("1" = list("1" = 2), "2" = list("2" = 1)))
  
})

test_that("Array/get/numeric/multiple/sorted/2", {  
  
  inst <- Array$new(list(a = 1, "1" = 2, "10" = 1, b = 1, 
    "20" = 1, "2" = 1, .a = 1))
  expect_identical(inst$get(1, 2, sorting = 2), list(b = 1, a = 1))
  expect_identical(inst$get(1, 2, inner = FALSE, sorting = 2), 
    list(b = list(b = 1), a = list(a = 1)))
  
  expect_identical(inst$get(2, 1, sorting = 2), list(b = 1, a = 1))
  expect_identical(inst$get(2, 1, inner = FALSE, sorting = 2), 
    list(b = list(b = 1), a = list(a = 1)))
  
})

test_that("Array/get/numeric/multiple/non-existing", {  
  
  inst <- Array$new(list(a = 1, b = 1))
  expect_identical(inst$get(1, 3), list(a = 1, NULL))
  expect_identical(inst$get(1, 3, inner = FALSE), 
    list(a = list(a = 1), NULL))
  expect_warning(expect_identical(inst$get(1, 3, strict = 1), list(a = 1, NULL)))
  expect_error(inst$get(1, 3, strict = 2))
  expect_identical(inst$get(1, 3, simplify = TRUE), list(a = 1))
  
  expect_equivalent(inst$get(3, 4), list(NULL, NULL))
  expect_identical(inst$get(3, 4, simplify = TRUE), list())
  
})

##------------------------------------------------------------------------------
context("Array/exists")
##------------------------------------------------------------------------------

test_that("Array/exists/character", {  
  
  inst <- Array$new(list(a = 1, b = 1))
  expect_true(inst$exists("a"))
  expect_true(all(inst$exists(c("a", "b"))))
  expect_true(all(inst$exists("a", "b")))
  expect_false(inst$exists("c"))
  expect_identical(inst$exists("a", "c"), c(a = TRUE, c = FALSE))
  
})

test_that("Array/exists/numeric", {  
  
  inst <- Array$new(list(a = 1, b = 1))
  expect_true(inst$exists(1))
  expect_true(all(inst$exists(c(1, 2))))
  expect_true(all(inst$exists(1, 2)))
  expect_false(inst$exists(3))
  expect_identical(inst$exists(1, 3), c("1" = TRUE, "3" = FALSE))
  
})

test_that("Array/exists/empty", {  
  
  inst <- Array$new()
  expect_equivalent(inst$exists(), logical())
  
})

##------------------------------------------------------------------------------
context("Array/index")
##------------------------------------------------------------------------------

test_that("Array/index/character/single", {  
  
  inst <- Array$new(list(a = 1, b = 1))
  expect_identical(inst$index("a"), structure(1, names = "a"))
#   expect_identical(inst$index("a", simplify = TRUE), 1)

})

test_that("Array/index/character/single/non-existing", {    
  
  inst <- Array$new(list(a = 1, b = 1))
  expect_identical(inst$index("c"), structure(NA_integer_, names = "c"))
  expect_warning(expect_identical(inst$index("c", strict = 1), 
    structure(NA_integer_, names = "c")))
  expect_error(inst$index("c", strict = 2))
  expect_identical(inst$index("c", simplify = TRUE), numeric())
  
})

test_that("Array/index/character/single/sorted", {  
  
  inst <- Array$new(list(a = 1, "1" = 2, "10" = 1, b = 1, 
    "20" = 1, "2" = 1, .a = 1))
  expect_identical(inst$index("a", sorting = 0), structure(1, names = "a"))
  expect_identical(inst$index("a", sorting = 1), structure(6, names = "a"))
  expect_identical(inst$index("a", sorting = 2), structure(2, names = "a"))

})

test_that("Array/index/character/multiple", {    
  
  inst <- Array$new(list(a = 1, b = 1))
  expect_identical(inst$index(c("a", "b")), 
    structure(as.numeric(1:2), names = c("a", "b")))

})

test_that("Array/index/character/multiple/non-existing", {   
  
  inst <- Array$new(list(a = 1, b = 1))
  expect_identical(inst$index(c("a", "c")), 
    structure(c(1, NA_integer_), names = c("a", "c")))  
  expect_identical(inst$index(c("a", "c"), simplify = TRUE), as.numeric(1))
  
})

test_that("Array/index/character/multiple/sorted", {  
  
  inst <- Array$new(list(a = 1, "1" = 2, "10" = 1, b = 1, 
    "20" = 1, "2" = 1, .a = 1))
  expect_identical(inst$index("a", ".a", sorting = 0), 
    structure(c(1, 7), names = c("a", ".a")))
  expect_identical(inst$index("a", ".a", sorting = 1), 
    structure(c(5, 6), names = c("a", ".a")))
  expect_identical(inst$index("a", ".a", sorting = 2), 
    structure(c(2, 3), names = c("a", ".a")))

})
  
##------------------------------------------------------------------------------
context("Array/clear")
##------------------------------------------------------------------------------

test_that("Array/clear", {  
  
  expect_is(inst <- Array$new(list(a = 1, b = 1)), "Array")
  expect_true(inst$clear())
  expect_identical(inst$.array, structure(list(), names = character()))
  
})  
  
##------------------------------------------------------------------------------
context("Array/remove")
##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
context("Array/remove/character")
##------------------------------------------------------------------------------

test_that("Array/remove/character/single", {  
  
  inst <- Array$new(list(a = 1, b = 1, c = 1))
  expect_true(inst$rm("a"))
  expect_false(inst$exists("a"))
  
})

test_that("Array/remove/character/single/fast", {  
  
  inst <- Array$new(list(a = 1, b = 1, c = 1))
  expect_identical(inst$rm("a", fast = TRUE), c(a = NA))
  expect_false(inst$rm("a"))
  
  if (FALSE) {
    require("microbenchmark")
    microbenchmark(
      "regular" = {
        inst <- Array$new(a = 1)
        inst$rm("a")
      },
      "fast" = {
        inst <- Array$new(a = 1)
        inst$rm("a", fast = TRUE)
      }
    )
  }
  
})

test_that("Array/remove/character/single/non-existing", {  
  
  inst <- Array$new(list())
  expect_false(inst$rm("a"))
  expect_warning(expect_false(inst$rm("a", strict = 1)))
  expect_error(inst$rm("a", strict = 2))

})

test_that("Array/remove/character/multiple", {  
  
  inst <- Array$new(list(a = 1, b = 1, c = 1))
  expect_true(all(inst$rm("b", "c")))
  expect_false(all(inst$rm("b", "c")))
  expect_equivalent(inst$rm(c("a", "b")), c(TRUE, FALSE))
  
})

##------------------------------------------------------------------------------
context("Array/remove/numeric")
##------------------------------------------------------------------------------

test_that("Array/remove/numeric/single", {  
  
  inst <- Array$new(list(a = 1, b = 1, c = 1))
  expect_true(inst$rm(1))
  expect_false(inst$exists("a"))
  expect_false(inst$rm(3))
  
})

test_that("Array/remove/numeric/single/fast", {  
  
  inst <- Array$new(list(a = 1, b = 1, c = 1))
  expect_identical(inst$rm(1, fast = TRUE), c("1" = NA))
  expect_false(all(inst$exists("a")))
  expect_identical(inst$rm(3, fast = TRUE), c("3" = NA))
  expect_true(all(inst$exists("c")))
  
})

test_that("Array/remove/numeric/single/non-existing", {  
  
  inst <- Array$new(list())
  expect_false(inst$rm(1))
  expect_warning(expect_false(inst$rm(1, strict = 1)))
  expect_error(inst$rm(1, strict = 2))

})

test_that("Array/remove/numeric/multiple", {  
  
  inst <- Array$new(list(a = 1, b = 1, c = 1))
  expect_true(all(inst$rm(1, 3)))
  expect_false(all(inst$exists("a", "c")))
  expect_identical(inst$rm(1, 2), structure(c(TRUE, FALSE), names = c("1", "2")))
  
})

test_that("Array/remove/numeric/multiple/fast", {  
  
  inst <- Array$new(list(a = 1, b = 1, c = 1))
  expect_identical(inst$rm(1, 2, fast = TRUE), c("1" = NA, "2" = NA))
  expect_false(all(inst$exists("a", "b")))
  
})

##------------------------------------------------------------------------------
context("Array/remove first")
##------------------------------------------------------------------------------

test_that("Array/remove first/empty", {  
  
  inst <- Array$new()
  expect_false(inst$rmFirst())
  
})

test_that("Array/remove first/default", {  
  
  inst <- Array$new(list(a = 1, b = 1, c = 1))
  expect_true(inst$rmFirst())
  expect_identical(inst$.array, list(b = 1, c = 1))
  
})

test_that("Array/remove first/explicit", {  
  
  inst <- Array$new(list(a = 1, b = 1, c = 1))
  expect_true(all(inst$rmFirst(3)))
  expect_identical(inst$.array, structure(list(), names = character()))
  
})

test_that("Array/remove first/exceeds scope", {  
  
  inst <- Array$new(list(a = 1, b = 1, c = 1))
  expect_identical(inst$rmFirst(4), structure(c(rep(TRUE, 3), FALSE), names = 1:4))
  
  inst <- Array$new(list(a = 1, b = 1, c = 1))
  expect_warning(expect_identical(inst$rmFirst(4, strict = 1), 
    structure(c(rep(TRUE, 3), FALSE), names = 1:4)))
  inst <- Array$new(list(a = 1, b = 1, c = 1))
  expect_error(inst$rmFirst(4, strict = 2))
  
})

test_that("Array/remove first/simplify", {  
  
  inst <- Array$new(list(a = 1, b = 1, c = 1))
  expect_false(inst$rmFirst(4, simplify = TRUE))
  inst <- Array$new(list(a = 1, b = 1, c = 1))
  expect_true(inst$rmFirst(3, simplify = TRUE))
  expect_identical(inst$.array, structure(list(), names = character()))
  
})  

##------------------------------------------------------------------------------
context("Array/remove last")
##------------------------------------------------------------------------------

test_that("Array/remove last/empty", {  
  
  inst <- Array$new()
  expect_false(inst$rmLast())
  
  expect_warning(expect_false(inst$rmLast(strict = 1)))
  expect_error(inst$rmLast(strict = 2))
  
})

test_that("Array/remove last/default", {  
  
  inst <- Array$new(list(a = 1, b = 1, c = 1))
  expect_identical(inst$rmLast(), c("3" = TRUE))
  expect_identical(inst$.array, list(a = 1, b = 1))
  
})

test_that("Array/remove last/explicit", {  
  
  inst <- Array$new(list(a = 1, b = 1, c = 1))
  expect_identical(inst$rmLast(3), c("3" = TRUE, "2" = TRUE, "1" = TRUE))
  expect_identical(inst$.array, structure(list(), names = character()))

})

test_that("Array/remove last/exceeding", {    
  
  inst <- Array$new(list(a = 1, b = 1, c = 1))
  expect_identical(inst$rmLast(4), 
    structure(c(rep(TRUE, 3), FALSE), names = 3:0))
  expect_identical(inst$.array, structure(list(), names = character()))
  
  expect_warning(expect_identical(inst$rmLast(4, strict = 1), 
    structure(rep(FALSE, 4), names = 0:-3)))
  expect_error(inst$rmLast(4, strict = 2))
  
})
  
test_that("Array/remove last/simplify", {  
  
  inst <- Array$new(list(a = 1, b = 1, c = 1))
  expect_true(inst$rmLast(simplify = TRUE))
  expect_true(inst$rmLast(2, simplify = TRUE))
  
  expect_false(inst$rmLast(simplify = TRUE))
  expect_warning(expect_false(inst$rmLast(strict = 1, simplify = TRUE)))
  expect_error(inst$rmLast(strict = 2, simplify = TRUE))
  
  expect_false(inst$rmLast(2, simplify = TRUE))
  expect_identical(inst$.array, structure(list(), names = character()))
  
})

##------------------------------------------------------------------------------
context("Array/private/.order")
##------------------------------------------------------------------------------

test_that("Array/.order", { 
  
  inst <- Array$new(list(c = 1, a = 1, "1" = 1, d = 1, "10" = 1, 
    "20" = 1, "2" = 1, b = 1))
  expect_identical(inst$.order(), c("1", "2", "10", "20", "a", "b", "c", "d"))
  expect_identical(inst$.order(decreasing = TRUE), 
    c("d", "c", "b", "a", "20", "10", "2", "1"))
  
})

