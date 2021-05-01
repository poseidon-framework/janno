library(magrittr)
library(ggplot2)

published_data <- poseidonR::read_janno("~/agora/published_data")

dating_distribution <- published_data$Date_Type %>% table(useNA = "always")
dating_distribution["C14"] + dating_distribution["contextual"]
dating_distribution["modern"]

ancient_with_coordinates <- published_data %>% dplyr::filter(
  Date_Type %in% c("C14", "contextual"),
  !is.na(Latitude) & !is.na(Longitude)
)

published_data_filtered <- published_data %>% dplyr::filter(
  !is.na(Latitude) & !is.na(Longitude)
) %>% poseidonR::process_age() %>%
  dplyr::filter(
    !is.na(Date_BC_AD_Median_Derived)
  )

countries <- rnaturalearth::ne_countries(returnclass = "sf")

data_sf <- published_data_filtered %>% sf::st_as_sf(
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

p_hist <- published_data_filtered %>%
  dplyr::mutate(
    age_cut = cut(
      Date_BC_AD_Median_Derived, 
      breaks = c(
        min(published_data_filtered$Date_BC_AD_Median_Derived), 
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
