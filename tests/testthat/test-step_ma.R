# test basic output
output0 <- recipe(. ~ ., data = btcusdt) %>%
  step_ma(close, volume) %>%
  step_rm(-matches("ma_")) %>%
  step_naomit(all_predictors()) %>%
  prep() %>%
  juice()

test_that("step_ma basic output returned expected number of columns and rows", {

  expect_equal(ncol(output0), 2)
  expect_gte(nrow(output0), 1)

})

# test state output
output1 <- recipe(. ~ ., data = btcusdt) %>%
  step_ma(close, volume, state = TRUE) %>%
  step_rm(-matches("ma_")) %>%
  step_naomit(all_predictors()) %>%
  prep() %>%
  juice()

test_that("step_ma state output returned expected number of columns and rows", {

  expect_equal(ncol(output1), 6)
  expect_gte(nrow(output1), 1)

})

# test tidying recipes meta
rec <- recipe(. ~ ., data = btcusdt) %>%
  step_ma(close) %>%
  step_rm(-matches("ma_")) %>%
  step_naomit(all_predictors()) %>%
  prep()

test_that("tidying step_ma meta doesn't produces any error", {

  expect_silent(tidy(rec$steps[[1]]))
  expect_silent(tidy(rec$steps[[1]], info = "params"))

})
