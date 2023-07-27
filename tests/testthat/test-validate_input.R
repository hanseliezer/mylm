gambia <- read.csv("../gambia - for testing.csv")

test_that("Verify formula argument", {
  
  # wrong type
  expect_error(mylm("not a formula", data=gambia),
               "Invalid formula entered.")
  expect_error(mylm(3333, data=gambia),
               "Invalid formula entered.")
  
  # column doesn't exist
  expect_error(mylm(pos ~ netuse + income, data=gambia),
               "Formula has included non-existing columns.")
  
  # correct formula and column names all exist
  expect_silent(mylm(pos ~ age + green, data=gambia))
})

test_that("Verify data argument", {
  # wrong dataset name
  expect_error(mylm(pos ~ age + netuse, data=gamb),
               "Dataset not found in local environment.")
  
  # correct dataset name
  expect_silent(mylm(pos ~ netuse + green, data=gambia))
})

test_that("Verify subset argument", {
  # subset is of type character
  expect_error(mylm(pos ~ age + netuse, data=gambia, subset=rep(c("A", "B"), 10)),
               "Subset invalid: must be numeric or logical.")
  
  # subset is of type data.frame
  expect_error(mylm(pos ~ age + netuse, data=gambia, subset=gambia),
               "Subset invalid: must be numeric or logical.")
  
  # subset is of type numeric
  expect_silent(mylm(pos ~ age + netuse, data=gambia, subset=seq(1, 100, 2)))
  
  # subset is of type logical
  expect_silent(mylm(pos ~ age + green, data=gambia, subset=c(TRUE, FALSE, TRUE)))
  
  # subset is larger
  expect_error(mylm(pos ~ age + netuse, data=gambia, subset=c(1:10000)),
               "Invalid subset: results in larger dataset.")
  
  # subset is smaller
  expect_silent(mylm(pos ~ age + green, data=gambia, subset=c(1, 5:19, 21:104)))
})