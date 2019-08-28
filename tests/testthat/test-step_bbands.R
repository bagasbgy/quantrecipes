# test basic output
output0 <- recipe(. ~ ., data = btcusdt) %>%
  step_bbands(high, low, close) %>%
  step_rm(-matches("bbands_")) %>%
  step_naomit(all_predictors()) %>%
  prep() %>%
  juice()

test_that("step_bbands basic output returned expected number of columns and rows", {

  expect_equal(ncol(output0), 4)
  expect_gte(nrow(output0), 1)

})

# test state output
output1 <- recipe(. ~ ., data = btcusdt) %>%
  step_bbands(high, low, close, state = TRUE) %>%
  step_rm(-matches("bbands_")) %>%
  step_naomit(all_predictors()) %>%
  prep() %>%
  juice()

test_that("step_bbands state output returned expected number of columns and rows", {

  expect_equal(ncol(output1), 9)
  expect_gte(nrow(output1), 1)

})

# test tidying recipes meta
rec <- recipe(. ~ ., data = btcusdt) %>%
  step_bbands(high, low, close) %>%
  step_rm(-matches("bbands_")) %>%
  step_naomit(all_predictors()) %>%
  prep()

test_that("tidying step_bbands meta doesn't produces any error", {

  expect_silent(tidy(rec$steps[[1]]))
  expect_silent(tidy(rec$steps[[1]], info = "params"))

})
