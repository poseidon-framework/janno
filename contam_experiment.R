library(magrittr)
library(ggplot2)

anno <- data.table::fread("~/v42.4.1240K.anno", na.strings = c("..", "n/a (<200 SNPs)"))

xcontam <- anno %>% dplyr::transmute(
  xcon = `Xcontam ANGSD MOM point estimate (only if male and ≥200)`,
  err_list = gsub(
    "\\[|\\]", 
    "",
    `Xcontam ANGSD MOM 95% CI truncated at 0 (only if male and ≥200)`
    )
) %>%
  dplyr::filter(
    !is.na(xcon),
    xcon > 0
  ) %>%
  tidyr::separate(
    err_list, c("start", "stop"), sep = ","
  ) %>%
  tibble::as_tibble()

threshold <- (1 - 0.9545) / 2 # 2sigma range probability threshold

estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

predict_start_stop_one <- function(xcon, var = 0.00005) {
  beta <- estBetaParams(xcon, var)
  x <- seq(0, 0.1, length = 10000)
  dens <- dbeta(x, beta$alpha, beta$beta)
  cu <- cumsum(dens)
  threshold_adjusted <- max(cu) * threshold
  bottom <- round(x[max(which(cu <= threshold_adjusted))], 3)
  top <- round(x[min(which(cu > (max(cu) - threshold_adjusted)))], 3)
  return(tibble::tibble(bottom, top))
}

predict_contam_err <- lapply(
  xcontam$xcon,
  predict_start_stop_one
) %>% dplyr::bind_rows()

hu <- xcontam %>%
  dplyr::bind_cols(predict_contam_err)

View(hu)
