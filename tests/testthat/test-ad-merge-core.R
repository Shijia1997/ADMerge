test_that("ad_merge returns merged longitudinal data with expected core columns", {
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
  
  expect_true(is.list(res))
  expect_true(all(c("analysis_data", "dict_src") %in% names(res)))
  
  df <- res$analysis_data
  expect_true(all(c("ID_merged", "Date_timeline") %in% names(df)))
  expect_true("ST29SV" %in% names(df))
  expect_true("LIMMTOTAL" %in% names(df))
  expect_true("AGE" %in% names(df))
})

test_that("cross-sectional variables are carried across all visits for each participant", {
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
  
  df <- res$analysis_data
  
  age_by_rid <- df %>%
    dplyr::filter(ID_merged == "1") %>%
    dplyr::pull(AGE) %>%
    unique()
  
  expect_equal(length(age_by_rid), 1)
  expect_equal(age_by_rid, 70)
})

test_that("participants missing in some source files are still retained from timeline", {
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
  
  df <- res$analysis_data
  
  expect_true("3" %in% df$ID_merged)
  expect_true(any(is.na(df$LIMMTOTAL[df$ID_merged == "3"])))
})