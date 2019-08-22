# an example preprocess
output <- recipe(. ~ ., data = btcusdt) %>%
  step_bbands(high, low, close) %>%
  step_rm(datetime, open, high, low, close, volume, turnover) %>%
  step_naomit(all_predictors()) %>%
  prep() %>%
  juice()

# column and row output
test_that("step_bbands returned expected number of columns and rows", {

  expect_equal(ncol(output), 9)

  expect_gt(nrow(output), 1)

})
