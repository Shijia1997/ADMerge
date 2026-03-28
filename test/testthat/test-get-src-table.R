test_that("get_src_table detects IDs, dates, and non-longitudinal files", {
  td <- make_test_dir_basic()
  
  src <- get_src_table(
    path = td,
    ID_usr_list = c("RID"),
    DATE_usr_list = c("EXAMDATE", "VISDATE"),
    non_longitudinal_list = c("PTDEMOG")
  )
  
  expect_true("DXSUM" %in% src$file)
  expect_true("NEUROBAT" %in% src$file)
  expect_true("PTDEMOG" %in% src$file)
  
  expect_equal(src$ID_for_merge[src$file == "DXSUM"], "RID")
  expect_equal(src$DATE_for_merge[src$file == "DXSUM"], "EXAMDATE")
  expect_equal(src$DATE_for_merge[src$file == "NEUROBAT"], "VISDATE")
  
  expect_true(is.na(src$DATE_for_merge[src$file == "PTDEMOG"]))
  expect_false(src$longitudinal[src$file == "PTDEMOG"])
})

test_that("get_src_table respects user-specified window and overlap", {
  td <- make_test_dir_basic()
  
  src <- get_src_table(
    path = td,
    ID_usr_list = c("RID"),
    DATE_usr_list = c("EXAMDATE", "VISDATE"),
    IS_overlap_list = FALSE,
    WINDOW_list = 180,
    non_longitudinal_list = c("PTDEMOG")
  )
  
  expect_true(all(src$WINDOW[!is.na(src$WINDOW)] == 180))
  expect_true(all(src$IS_overlap[!is.na(src$IS_overlap)] == FALSE))
})

test_that("get_src_table rejects non-numeric WINDOW_list", {
  td <- make_test_dir_basic()
  
  expect_error(
    get_src_table(
      path = td,
      WINDOW_list = c("bad", "input")
    ),
    "WINDOW_list must contain only numeric values"
  )
})