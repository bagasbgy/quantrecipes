# test basic output
output0 <- recipe(. ~ ., data = btcusdt) %>%
  step_zigzag(close) %>%
  step_rm(-matches("zigzag_")) %>%
  step_naomit(all_predictors()) %>%
  prep() %>%
  juice()

test_that("step_zigzag basic output returned expected number of columns and rows", {

  expect_equal(ncol(output0), 1)
  expect_gte(nrow(output0), 1)

})

# test state output
output1 <- recipe(. ~ ., data = btcusdt) %>%
  step_zigzag(close, state = TRUE) %>%
  step_rm(-matches("zigzag_")) %>%
  step_naomit(all_predictors()) %>%
  prep() %>%
  juice()

test_that("step_zigzag state output returned expected number of columns and rows", {

  expect_equal(ncol(output1), 3)
  expect_gte(nrow(output1), 1)

})
