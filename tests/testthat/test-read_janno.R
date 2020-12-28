test_that("reading a minimal janno file works as expected", {
  expect_snapshot_value(
    read_janno(path_janno_minimal, validate = F),
    style = c("json2")
  )
})

test_that("reading a normal janno file works as expected", {
  expect_snapshot_value(
    read_janno(path_janno_normal, validate = F),
    style = c("json2")
  )
})

test_that("reading a broken janno file works as expected", {
  expect_snapshot_value(
    read_janno(path_janno_borked, validate = F),
    style = c("json2")
  )
})
