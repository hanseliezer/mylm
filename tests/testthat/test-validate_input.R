gambia <- read.csv("../gambia - for testing.csv")

test_that("Verify data argument", {
  expect_error(mylm(pos ~ age + netuse, data=gamb),
               "Dataset not found in local environment.")
})

test_that("Verify subset argument", {
  expect_error(mylm(pos ~ age + netuse, data=gambia, subset=c("poss", "age")),
               "At least one column in subset not found in dataset.")
  expect_silent(mylm(pos ~ age + green, data=gambia, subset=c("pos", "age", "green")))
})