test_that("quickcalibration works as expected", {
  expect_snapshot_value(
    quickcalibrate(list(2000, c(2000, 2300, 2100)), list(20, c(20, 30, 70))),
    style = c("json2")
  )
})
  