# test basic output
output0 <- recipe(. ~ ., data = actions) %>%
  step_cumret(benchmark, portfolio, prices = "close") %>%
  step_rm(-matches("cumret_")) %>%
  step_naomit(all_predictors()) %>%
  prep() %>%
  juice()

test_that("step_cumret basic output returned expected number of columns and rows", {

  expect_equal(ncol(output0), 2)
  expect_gte(nrow(output0), 1)

})

# test tidying recipes meta
rec <- recipe(. ~ ., data = actions) %>%
  step_cumret(benchmark, portfolio, prices = "close") %>%
  step_rm(-matches("cumret_")) %>%
  step_naomit(all_predictors()) %>%
  prep()

test_that("tidying step_cumret meta doesn't produces any error", {

  expect_silent(tidy(rec$steps[[1]]))
  expect_silent(tidy(rec$steps[[1]], info = "params"))

})
