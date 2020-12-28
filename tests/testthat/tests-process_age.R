test_that("processing age in a normal janno file works as expected", {
  set.seed(123)
  normal_janno <- read_janno(path_janno_normal, validate = F)
  expect_snapshot_value(
    process_age(normal_janno) %>%
      dplyr::mutate(
        Date_BC_AD_Prob = Map(
          function(x){
            if (is.data.frame(x)) {
              x$sum_dens <- round(x$sum_dens, 5)
              return(x)
            } else {
              return(x)
            }
          },
          Date_BC_AD_Prob
        )
      ),
    style = c("json2")
  )
})
