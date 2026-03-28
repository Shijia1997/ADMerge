test_that("get_window_bound uses full half-window when overlap is TRUE", {
  original <- as.Date("2020-07-01")
  
  left_bound <- get_window_bound(
    original_date = original,
    bound_date = 10,
    is_left = 1,
    ovlp = TRUE,
    window_len = 180
  )
  
  right_bound <- get_window_bound(
    original_date = original,
    bound_date = 10,
    is_left = -1,
    ovlp = TRUE,
    window_len = 180
  )
  
  expect_equal(as.Date(left_bound, origin = "1970-01-01"), original - 90)
  expect_equal(as.Date(right_bound, origin = "1970-01-01"), original + 90)
})

test_that("get_window_bound truncates by neighbor distance when overlap is FALSE", {
  original <- as.Date("2020-07-01")
  
  left_bound <- get_window_bound(
    original_date = original,
    bound_date = 20,
    is_left = 1,
    ovlp = FALSE,
    window_len = 180
  )
  
  # since bound_date = 20 < 90, use 20 instead of half-window
  expect_equal(as.Date(left_bound, origin = "1970-01-01"), original - 20)
})

test_that("get_window_bound handles missing bound date by defaulting to half-window", {
  original <- as.Date("2020-07-01")
  
  left_bound <- get_window_bound(
    original_date = original,
    bound_date = NA,
    is_left = 1,
    ovlp = FALSE,
    window_len = 180
  )
  
  expect_equal(as.Date(left_bound, origin = "1970-01-01"), original - 90)
})