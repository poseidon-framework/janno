test_that("reading broken_full.janno works", {
  expect_snapshot_value(
    suppressMessages(read_janno(path_janno_borked_full, validate = F)),
    style = c("json2")
  )
})

test_that("reading broken_partial.janno works", {
  expect_snapshot_value(
    suppressMessages(read_janno(path_janno_borked_partial, validate = F)),
    style = c("json2")
  )
})

test_that("reading borked_wrong_name.janno fails", {
  expect_error(
    suppressMessages(read_janno(path_janno_borked_wrong_name, validate = F))
  )
})

test_that("reading minimal_full.janno works", {
  expect_snapshot_value(
    suppressMessages(read_janno(path_janno_minimal_full, validate = F)),
    style = c("json2")
  )
})

test_that("reading minimal_partial.janno works", {
  expect_snapshot_value(
    suppressMessages(read_janno(path_janno_minimal_partial, validate = F)),
    style = c("json2")
  )
})

test_that("reading minimal_partial_extra_columns.janno works", {
  expect_snapshot_value(
    suppressMessages(read_janno(path_janno_minimal_partial_extra_columns, validate = F)),
    style = c("json2")
  )
})

test_that("reading normal_full.janno works", {
  expect_snapshot_value(
    suppressMessages(read_janno(path_janno_normal_full, validate = F)),
    style = c("json2")
  )
})

test_that("reading normal_partial.janno works", {
  expect_snapshot_value(
    suppressWarnings(suppressMessages(read_janno(path_janno_normal_partial, validate = F))),
    style = c("json2")
  )
})

