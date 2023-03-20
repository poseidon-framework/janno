library(magrittr)
library(ggplot2)

published_data <- janno::read_janno("~/agora/published_data", validate = F)

ancient_with_spatiotemporal_postion <- published_data %>% dplyr::filter(
  Date_Type %in% c("C14", "contextual"),
  !is.na(Latitude) & !is.na(Longitude)
)

ancient_with_spatiotemporal_postion %>% dplyr::mutate(
  year_of_publication = Publication %>% purrr::map_int(function(x) {as.integer(min(readr::parse_number(x)))})
) %>%
  dplyr::mutate(
    dplyr::across(
      where(is.list),
      function(x) { Map(function(y) { paste(y, collapse = ";") }, x) %>% unlist() }
    )
  ) %>%
  readr::write_csv(file = "quick_analysis/poseidon_05_2020_with_spatiotemporal_info.csv")
  

ancient_data_filtered <- ancient_with_spatiotemporal_postion %>% 
  janno::janno() %>%
  dplyr::filter(
    !is.na(Date_BC_AD_Median_Derived)
  )

ancient_data_filtered %>% dplyr::group_by(
  Date_Type
) %>%
  dplyr::tally()

countries <- rnaturalearth::ne_countries(returnclass = "sf")

data_sf <- ancient_data_filtered %>% sf::st_as_sf(
  coords = c("Longitude", "Latitude"),
  crs = 4326
) %>%
  sf::st_transform(
    crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"
  )

p_map <- ggplot() +
  geom_sf(data = countries, fill = "white", size = 0.1) +
  geom_sf(
    data = data_sf,
    size = 0.3,
    color = "red"
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "#BFD5E3"),
    panel.spacing = unit(c(0, 0, 0, 0), "cm"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_sf(expand = F, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")

p_hist <- ancient_data_filtered %>%
  dplyr::mutate(
    age_cut = cut(
      Date_BC_AD_Median_Derived, 
      breaks = c(
        min(ancient_data_filtered$Date_BC_AD_Median_Derived), 
        seq(-10000, 2000, 500)
      ),
      labels = c("< -10000", paste0("> ", seq(-10000, 1500, 500))),
      include.lowest = T
    )
  ) %>%
  ggplot() +
  geom_histogram(
    aes(x = age_cut),
    fill = "red",
    color = "white",
    stat = "count"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  ) +
  xlab("age [calBC/AD]")

p <- cowplot::plot_grid(p_map, p_hist, ncol = 1, rel_heights = c(0.7, 0.34))

ggsave(
  "quick_analysis/spatiotemporal_overview.png",
  plot = p,
  device = "png",
  width = 6.5,
  height = 5,
  units = "cm",
  scale = 3
)

###

p_nr_automsomal_snps <- ancient_data_filtered %>%
  ggplot() +
  geom_histogram(
    aes(x = Nr_autosomal_SNPs),
    boundary = 0, closed = "left",
    binwidth = 100000,
    fill = "red",
    color = "white"
  ) +
  theme_bw() +
  scale_x_continuous(labels = scales::comma) +
  xlab("Number of 1240k SNPs covered at least once")

p_coverage1240k <- ancient_data_filtered %>%
  dplyr::filter(Coverage_1240K < 7) %>%
  ggplot() +
  geom_histogram(
    aes(x = Coverage_1240K),
    boundary = 0, closed = "left",
    binwidth = 0.5,
    fill = "red",
    color = "white"
  ) +
  theme_bw() +
  xlab("Mean coverage on 1240k")

p2 <- cowplot::plot_grid(p_nr_automsomal_snps, p_coverage1240k, nrow = 1, rel_widths = c(0.5, 0.5))

ggsave(
  "quick_analysis/coverage.png",
  plot = p2,
  device = "png",
  width = 10,
  height = 5,
  units = "cm",
  scale = 2
)

