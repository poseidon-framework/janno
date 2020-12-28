test_that("validating a minimal janno file works as expected", {
  expect_snapshot_value(
    validate_janno(path_janno_minimal),
    style = c("json2")
  )
})

test_that("validating a normal janno file works as expected", {
  expect_snapshot_value(
    validate_janno(path_janno_normal),
    style = c("json2")
  )
})

test_that("validating a broken janno file works as expected", {
  expect_snapshot_value(
    validate_janno(path_janno_borked),
    style = c("json2")
  )
})
