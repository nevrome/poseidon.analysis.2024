library(magrittr)
library(ggplot2)

#### load janno data ####s

load("data/janno_data.RData")

#### space-time figure ####

# filter datasets to spatiotemporally informed subset

paa_ancient_with_coords <- paa %>%
  dplyr::filter(
    Date_Type %in% c("C14", "contextual"),
    !is.na(Date_BC_AD_Start) & !is.na(Date_BC_AD_Stop)
  ) %>%
  dplyr::filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  tibble::as_tibble()

# make sample data spatial

paa_ancient_sf_6933 <- paa_ancient_with_coords %>%
  sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326) %>%
  sf::st_transform(6933)

# prepare spatial context data

world <- giscoR::gisco_get_countries()

world_6933 <- world %>%
  sf::st_transform(6933) %>%
  sf::st_union() %>%
  sf::st_simplify(dTolerance = 20000)

extent_world_6933 <- world_6933 %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%
  sf::st_segmentize(dfMaxLength = 10000)

world_grid_6933_basic <- sf::st_make_grid(
  world_6933,
  n = c(72,36), #c(54, 27), #c(36,18),
  what = 'polygons',
  flat_topped = TRUE
) %>% sf::st_as_sf() %>%
  dplyr::mutate(
    area_id = seq_along(x)
  )

world_grid_6933 <- world_grid_6933_basic %>%
  sf::st_segmentize(dfMaxLength = 10000)

# perform counting in spatial bins

inter_world <- function(x) {
  sf::st_intersects(world_grid_6933, x) %>% lengths()
}

world_with_count <- world_grid_6933 %>%
  dplyr::mutate(
    PAA = paa_ancient_sf_6933 %>% inter_world()
  ) %>%
  tidyr::pivot_longer(
    tidyselect::one_of("PAA"),
    names_to = "database", values_to = "count"
  ) %>%
  dplyr::filter(count != 0)

# construct map figure

map_plot <- ggplot() +
  geom_sf(data = extent_world_6933, fill = "#c2eeff", color = "black", alpha = 0.5) +
  geom_sf(data = world_6933, fill = "white", color = NA) +
  geom_sf(
    data = world_with_count, 
    aes(fill = count),
    color = NA,
    alpha = 0.5
  ) +
  scale_fill_viridis_c(
    option = "magma", direction = -1, limits = c(0, 800), oob = scales::squish
  ) +
  guides(fill = guide_colorbar(barwidth = 20, barheight = 1)) +
  geom_sf(
    data = world_with_count %>%
      dplyr::group_by(area_id) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup(), 
    color = "black", size = 0.1, fill = NA
  ) +
  geom_sf(data = world_6933, fill = NA, color = "black", cex = 0.2) +
  coord_sf(expand = F, crs = "+proj=natearth") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_line(colour = "grey", linewidth = 0.3),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

# time histogram

samples_with_mean_age <- paa_ancient_with_coords %>%
  dplyr::select(Approx_Individual_ID, tidyselect::starts_with("Date_BC_AD"), package, source) %>%
  dplyr::mutate(
    Date_BC_AD_Median = dplyr::case_when(
      is.na(Date_BC_AD_Median) ~ (Date_BC_AD_Start + Date_BC_AD_Stop) / 2,
      TRUE ~ Date_BC_AD_Median
    ),
    age_cut = cut(
      Date_BC_AD_Median, 
      breaks = c(
        min(Date_BC_AD_Median), 
        seq(-10000, 2000, 500)
      ),
      labels = c("< -10000", paste0("> ", seq(-10000, 1500, 500))),
      include.lowest = T
    )
  )

age_groups_count <- samples_with_mean_age %>%
  dplyr::group_by(age_cut) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")

time_hist_plot <- ggplot() +
  geom_bar(
    data = age_groups_count,
    mapping = aes(x = age_cut, y = n, fill = n),
    stat = "identity",
    alpha = 0.7,
    color = "black",
    linewidth = 0.2
  ) +
  scale_fill_viridis_c(option = "magma", direction = -1, limits = c(0, 800), oob = scales::squish) +
  ylim(-100, 1500) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(angle = 20, hjust = 1, vjust = 0.5),
    axis.title.x = element_blank()
  ) +
  coord_flip() +
  xlab("age BC/AD")

# combine plots

p <- cowplot::plot_grid(
  time_hist_plot, map_plot,
  ncol = 2,
  #labels = c("A", "B"),
  rel_widths = c(0.3, 1)
)

ggsave(
  paste0("plots/presentation/figure_spacetime_paa.png"),
  plot = p,
  device = "png",
  scale = 0.6,
  dpi = 300,
  width = 500, height = 220, units = "mm",
  limitsize = F,
  bg = "white"
)
