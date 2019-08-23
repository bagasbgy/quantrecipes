# test basic output
output0 <- recipe(. ~ ., data = btcusdt) %>%
  step_bbands(high, low, close) %>%
  step_rm(-matches("bbands")) %>%
  step_naomit(all_predictors()) %>%
  prep() %>%
  juice()

test_that("step_bbands basic output returned expected number of columns and rows", {

  expect_equal(ncol(output0), 4)
  expect_gte(nrow(output0), 1)

})

# test state output
output1 <- recipe(. ~ ., data = btcusdt) %>%
  step_bbands(high, low, close, state = TRUE, prev_state = TRUE) %>%
  step_rm(-matches("bbands")) %>%
  step_naomit(all_predictors()) %>%
  prep() %>%
  juice()

test_that("step_bbands state output returned expected number of columns and rows", {

  expect_equal(ncol(output1), 9)
  expect_gte(nrow(output1), 1)

})

# test passing options
outputA <- recipe(. ~ ., data = btcusdt) %>%
  step_bbands(high, low, close,
    ma_fun = "EMA",
    ma_options = list(wilder = TRUE)
  ) %>%
  step_rm(-matches("bbands")) %>%
  step_naomit(all_predictors()) %>%
  prep() %>%
  juice()

outputB <- recipe(. ~ ., data = btcusdt) %>%
  step_bbands(high, low, close,
    ma_fun = "EMA",
    ma_options = list(wilder = FALSE)
  ) %>%
  step_rm(-matches("bbands")) %>%
  step_naomit(all_predictors()) %>%
  prep() %>%
  juice()

outputC <- recipe(. ~ ., data = btcusdt) %>%
  step_bbands(high, low, close,
    ma_fun = "EMA",
    ma_options = list(wilder = FALSE),
    state = TRUE,
    prev_state = TRUE
  ) %>%
  step_rm(-matches("bbands")) %>%
  step_naomit(all_predictors()) %>%
  prep() %>%
  juice()

outputD <- recipe(. ~ ., data = btcusdt) %>%
  step_bbands(high, low, close,
    ma_fun = "EMA",
    ma_options = list(wilder = FALSE),
    state = TRUE,
    prev_state = TRUE,
    state_options = list(
      high = 0.9,
      medhigh = 0.6,
      medlow = 0.4,
      low = 0.1
    )
  ) %>%
  step_rm(-matches("bbands")) %>%
  step_naomit(all_predictors()) %>%
  prep() %>%
  juice()

test_that("step_bbands handle options passing as expected", {

  expect_true(grepl("Rows in", all_equal(outputA, outputB)))
  expect_true(grepl("Rows in", all_equal(outputC, outputD)))

})

# test tidying recipes meta
rec <- recipe(. ~ ., data = btcusdt) %>%
  step_bbands(high, low, close) %>%
  step_rm(-matches("bbands")) %>%
  step_naomit(all_predictors()) %>%
  prep()

test_that("tidying step_bbands meta produces any error", {

  expect_silent(tidy(rec$steps[[1]]))
  expect_silent(tidy(rec$steps[[1]]))

})
