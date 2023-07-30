diab <- read.csv("../diabetes.txt", sep=" ")

# use stats' lm() as the reference, consider them to be the "correct" answer
lm_true <- lm(y ~ ., data=diab)
lm_mylm <- mylm(y ~ ., data=diab)

test_that("Verify regression results", {
  
  # compare beta estimates
  expect_equal(lm_true$coefficients, lm_mylm$coef)
  
  # compare residuals
  expect_equal(lm_true$residuals, lm_mylm$residuals)
  
  # compare variance-covariance matrices
  expect_equal(vcov(lm_true), lm_mylm$vcov)
  
})