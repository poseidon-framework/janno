library(magrittr)
library(ggplot2)

published_data <- janno::read_janno("~/agora/published_data", validate = F)

published_data  %>% dplyr::filter(
  Date_Type %in% c("C14", "contextual")
  #!is.na(Latitude) & !is.na(Longitude)
  ) %>%
  dplyr::mutate(
    `Year of publication` = Publication %>% purrr::map_int(
      function(x) {
        if (is.null(x)) { return(NA) }
        as.integer(min(readr::parse_number(x)))
      }
    )
  ) -> hu

p <- hu %>%
  dplyr::group_by(`Year of publication`) %>%
  dplyr::tally() %>%
  dplyr::mutate(
    `Cumulative number of new, ancient samples` = cumsum(n)
  ) %>%
  ggplot() +
  geom_bar(
    aes(x = `Year of publication`, y = `Cumulative number of new, ancient samples`),
    stat = "identity"
  ) +
  scale_x_continuous(breaks = 2010:2021) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )

ggsave(
  "quick_analysis/cumsamps.png",
  plot = p,
  device = "png",
  width = 10,
  height = 5,
  units = "cm",
  scale = 2
)



