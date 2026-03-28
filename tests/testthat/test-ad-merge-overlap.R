test_that("non-overlap strategy prevents duplicated assignment in midpoint case", {
  td <- make_test_dir_overlap()
  
  src <- get_src_table(
    path = td,
    ID_usr_list = c("RID"),
    DATE_usr_list = c("EXAMDATE"),
    IS_overlap_list = FALSE,
    WINDOW_list = 366
  )
  
  res <- ad_merge(
    path = td,
    DATE_type = "Date",
    timeline_file = "DXSUM",
    dict_src = src
  )
  
  df <- res$analysis_data %>%
    dplyr::filter(!is.na(ST29SV))
  
  # under non-overlap, single MRI row should appear at most once
  expect_lte(nrow(df), 1)
})

test_that("overlap strategy can reuse the same midpoint MRI observation", {
  td <- make_test_dir_overlap()
  
  src <- get_src_table(
    path = td,
    ID_usr_list = c("RID"),
    DATE_usr_list = c("EXAMDATE"),
    IS_overlap_list = TRUE,
    WINDOW_list = 366
  )
  
  res <- ad_merge(
    path = td,
    DATE_type = "Date",
    timeline_file = "DXSUM",
    dict_src = src
  )
  
  df <- res$analysis_data %>%
    dplyr::filter(!is.na(ST29SV))
  
  # under overlap, same MRI observation may be matched to both visits
  expect_gte(nrow(df), 1)
})