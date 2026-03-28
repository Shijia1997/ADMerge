test_that("review_complete returns complete-case dataframe and ggplot object", {
  td <- make_test_dir_basic()
  
  src <- get_src_table(
    path = td,
    ID_usr_list = c("RID"),
    DATE_usr_list = c("EXAMDATE", "VISDATE"),
    IS_overlap_list = FALSE,
    WINDOW_list = 180,
    non_longitudinal_list = c("PTDEMOG")
  )
  
  res <- ad_merge(
    path = td,
    DATE_type = "Date",
    timeline_file = "DXSUM",
    dict_src = src
  )
  
  out <- review_complete(res, check_cols = c("ST29SV", "LIMMTOTAL"))
  
  expect_true(is.list(out))
  expect_true(all(c("plot", "complete_df") %in% names(out)))
  expect_s3_class(out$plot, "ggplot")
  expect_true(is.data.frame(out$complete_df))
})

test_that("review_complete errors when requested columns are missing", {
  td <- make_test_dir_basic()
  
  src <- get_src_table(
    path = td,
    ID_usr_list = c("RID"),
    DATE_usr_list = c("EXAMDATE", "VISDATE"),
    IS_overlap_list = FALSE,
    WINDOW_list = 180,
    non_longitudinal_list = c("PTDEMOG")
  )
  
  res <- ad_merge(
    path = td,
    DATE_type = "Date",
    timeline_file = "DXSUM",
    dict_src = src
  )
  
  expect_error(
    review_complete(res, check_cols = c("NOT_A_REAL_COLUMN")),
    "Please make sure input data is within the column"
  )
})