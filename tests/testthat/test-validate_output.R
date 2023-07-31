diab <- read.csv("../diabetes.txt", sep=" ")

test_that("Verify regression output", {
  
  # model with all predictors included
  diab_mod1 <- mylm(y ~ ., data=diab)
  
  # number of coefficients should match the number of columns in the original table (minus y, plus intercept)
  expect_length(diab_mod1$coef, ncol(diab))
  
  # number of residuals should match the number of rows/observations in the original table
  expect_length(diab_mod1$residuals, nrow(diab))
  
  # coefficients should all be type double ('decimals')
  expect_type(diab_mod1$coef, "double")
  
  # try smaller model with subsetting
  random_rows <- seq(1, nrow(diab), 3)
  diab_mod2 <- mylm(y ~ ldl + hdl+ bmi, data=diab, subset=random_rows)
  
  # number of coefficients should match the number of predictors chosen
  expect_length(diab_mod2$coef, 4)
  
  # number of rows in the data used should match the length of the subset
  expect_equal(nrow(diab_mod2$data), length(random_rows))
  
  # number of residuals should also match the length of the subset
  expect_length(diab_mod2$residuals, length(random_rows))
  
})