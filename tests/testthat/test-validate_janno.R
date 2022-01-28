test_that("validating broken_full.janno works (list of issues)", {
  expect_snapshot_value(
    suppressMessages(validate_janno(path_janno_borked_full)),
    style = c("json2")
  )
})

test_that("validating broken_partial.janno works (one issue)", {
  expect_snapshot_value(
    suppressMessages(validate_janno(path_janno_borked_partial)),
    style = c("json2")
  )
})

test_that("validating minimal_full.janno works (no issues)", {
  expect_snapshot_value(
    suppressMessages(validate_janno(path_janno_minimal_full)),
    style = c("json2")
  )
})

test_that("validating minimal_partial.janno works (no issues)", {
  expect_snapshot_value(
    suppressMessages(validate_janno(path_janno_minimal_partial)),
    style = c("json2")
  )
})

test_that("validating minimal_partial_extra_columns.janno works (no issues)", {
  expect_snapshot_value(
    suppressMessages(validate_janno(path_janno_minimal_partial_extra_columns)),
    style = c("json2")
  )
})

test_that("validating normal_full.janno works (no issues)", {
  expect_snapshot_value(
    suppressMessages(validate_janno(path_janno_normal_full)),
    style = c("json2")
  )
})

test_that("validating normal_partial.janno works (no issues)", {
  expect_snapshot_value(
    suppressWarnings(suppressMessages(validate_janno(path_janno_normal_partial))),
    style = c("json2")
  )
})

### specifically borked files

test_that("validating borked_wrong_name.janno fails (fails to load)", {
  expect_error(
    suppressMessages(validate_janno(path_janno_borked_wrong_name))
  )
})
