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

ggplot() +
  geom_sf(data = countries, fill = "white", size = 0.1) +
  geom_sf(
    data = data_sf,
    size = 0.3,
    color = "red"
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "#BFD5E3"),
    panel.spacing = unit(c(0, 0, 0, 0), "cm")
  ) +
  coord_sf(expand = F, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")

ggsave(
  "quick_analysis/published_data_map.png",
  plot = p_map,
  device = "png",
  width = 10,
  height = 5,
  units = "cm",
  scale = 2
)

p_hist <- published_data_filtered %>%
  ggplot() +
  geom_histogram(
    aes(x = Date_BC_AD_Median_Derived),
    binwidth = 1000,
    fill = "red"
  ) +
  theme_bw() +
  scale_x_continuous(breaks = seq(-50000, 2000, 2000)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  ) +
  xlab("age [calBC/AD]")

ggsave(
  "quick_analysis/published_data_hist.png",
  plot = p_hist,
  device = "png",
  width = 10,
  height = 5,
  units = "cm",
  scale = 2
)


