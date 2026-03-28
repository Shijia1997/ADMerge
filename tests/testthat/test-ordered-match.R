test_that("ordered_match returns first matching user-preferred variable", {
  expect_equal(
    ordered_match(c("RID", "PTID"), "PTID; VISCODE; RID", c("PTID")),
    "RID"
  )
})

test_that("ordered_match falls back to default when no user variable matches", {
  expect_equal(
    ordered_match(c("ABC", "XYZ"), "PTID; VISCODE; RID", c("PTID")),
    "PTID"
  )
})

test_that("ordered_match returns NA when file variable list is empty", {
  expect_true(is.na(
    ordered_match(c("RID"), "", c("RID"))
  ))
})