library(magrittr)
library(ggplot2)

ancient <- poseidonR::read_janno("~/agora/data_ancient/")

ancient_filtered <- ancient %>% dplyr::filter(
  !is.na(Latitude) & !is.na(Longitude)
) %>% poseidonR::process_age() %>%
  dplyr::filter(
    !is.na(Date_BC_AD_Median_Derived)
  )

countries <- rnaturalearth::ne_countries(returnclass = "sf")

p_map <- ggplot() +
  geom_sf(data = countries, fill = "white", size = 0.1) +
  geom_point(
    data = ancient_filtered, 
    aes(x = Longitude, y = Latitude),
    size = 0.3,
    color = "red"
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "#BFD5E3"),
    panel.spacing = unit(c(0, 0, 0, 0), "cm")
  ) +
  coord_sf(expand = F)

ggsave(
  "~/Desktop/ancient_map.png",
  plot = p_map,
  device = "png",
  width = 10,
  height = 5,
  units = "cm",
  scale = 2
)

p_hist <- ancient_filtered %>%
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
  "~/Desktop/ancient_hist.png",
  plot = p_hist,
  device = "png",
  width = 10,
  height = 5,
  units = "cm",
  scale = 2
)


