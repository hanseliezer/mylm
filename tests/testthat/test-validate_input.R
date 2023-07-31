diab <- read.csv("../diabetes.txt", sep=" ")
# assign these variables to global to simulate being outside testing environment
assign("y_diab", diab$y, envir=.GlobalEnv)
assign("age_diab", diab$age, envir=.GlobalEnv)
assign("glu_diab", diab$glu, envir=.GlobalEnv)

test_that("Verify validity of formula argument", {
  
  # wrong type
  expect_error(mylm("not a formula", data=diab),
               "Invalid formula entered.")
  expect_error(mylm(3333, data=diab),
               "Invalid formula entered.")
  
  # correct formula
  expect_silent(mylm(y ~ age + ldl + ltg, data=diab))
  
})

test_that("Verify formula in presence/non-presence of data argument", {
  
  # no data is given, and no chol_diab in (global) environment
  expect_error(mylm(y_diab ~ age_diab + chol_diab),
               "Dataset not given, and variables stated in formula not found in environment.")
  
  # no data is given, but all variables exist in environment
  expect_silent(mylm(y_diab ~ age_diab + glu_diab))
  
  # correct dataset name
  expect_silent(mylm(y ~ tc + hdl, data=diab))
  
})

test_that("Verify subset argument", {
  
  # subset is of type character
  expect_error(mylm(y ~ age + bmi, data=diab, subset=rep(c("A", "B"), 10)),
               "Subset invalid: must be numeric or logical.")
  
  # subset is of type data.frame
  expect_error(mylm(y ~ age + bmi, data=diab, subset=diab),
               "Subset invalid: must be numeric or logical.")
  
  # subset is of type numeric
  expect_silent(mylm(y ~ age + bmi, data=diab, subset=seq(1, 100, 2)))
  
  # subset is of type logical
  expect_silent(mylm(y ~ age + bmi, data=diab, subset=c(TRUE, FALSE, TRUE)))
  
  # subset is larger
  expect_error(mylm(y ~ age + bmi, data=diab, subset=c(1:10000)),
               "Invalid subset: results in larger dataset.")
  
  # subset is smaller
  expect_silent(mylm(y ~ age + bmi, data=diab, subset=c(1, 5:19, 21:104)))
  
})

# clean up the created global variables
rm(y_diab, age_diab, glu_diab, envir=.GlobalEnv)