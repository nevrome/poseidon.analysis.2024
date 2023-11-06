library(magrittr)
library(ggplot2)

#### prepare spatial data objects ####

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

world_grid_6933_top_triangles <- world_grid_6933_basic %>%
  dplyr::mutate(x = purrr::map(x, function(y) {
    sf::st_polygon(list(y[[1]][-2,]))
  }) %>% sf::st_sfc(crs = sf::st_crs(world_grid_6933_basic))) %>%
  sf::st_segmentize(dfMaxLength = 10000)

world_grid_6933_bottom_triangles <- world_grid_6933_basic %>%
  dplyr::mutate(x = purrr::map(x, function(y) {
    sf::st_polygon(list(y[[1]][-4,]))
  }) %>% sf::st_sfc(crs = sf::st_crs(world_grid_6933_basic))) %>%
  sf::st_segmentize(dfMaxLength = 10000)

#### prepare sample data ####

pca <- janno::read_janno("~/agora/community-archive", validate = F)
paa <- janno::read_janno("~/agora/aadr-archive", validate = F)

pca_ancient_with_coords <- pca %>%
  dplyr::filter(Date_Type %in% c("C14", "contextual")) %>%
  dplyr::filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  tibble::as_tibble()
  
paa_ancient_with_coords <- paa %>%
  dplyr::filter(Date_Type %in% c("C14", "contextual")) %>%
  dplyr::filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  tibble::as_tibble()

pca_ancient_sf_6933 <- pca_ancient_with_coords %>%
  sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326) %>%
  sf::st_transform(6933)

paa_ancient_sf_6933 <- paa_ancient_with_coords %>%
  sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326) %>%
  sf::st_transform(6933)

#### perform counting in spatial bins ####

inter_world <- function(x) {
  x %>% sf::st_intersects(world_grid_6933, .) %>% lengths()
}

world_with_count <- world_grid_6933 %>%
  dplyr::mutate(
    PCA = pca_ancient_sf_6933 %>% inter_world(),
    PAA = paa_ancient_sf_6933 %>% inter_world()
  ) %>%
  tidyr::pivot_longer(
    tidyselect::one_of("PCA", "PAA"),
    names_to = "database", values_to = "count"
  ) %>%
  dplyr::filter(count != 0)

centroid_pca <- world_with_count %>%
  dplyr::filter(database == "PCA") %>%
  sf::st_centroid()

triangles_pca <- world_grid_6933_bottom_triangles %>%
  dplyr::inner_join(
    world_with_count %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(database == "PCA") %>%
      dplyr::select(area_id, database, count),
    by = "area_id"
  )

triangles_paa <- world_grid_6933_top_triangles %>%
  dplyr::inner_join(
    world_with_count %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(database == "PAA") %>%
      dplyr::select(area_id, database, count),
    by = "area_id"
  )

#### world map ###

ggplot() +
  geom_sf(data = extent_world_6933, fill = "#c2eeff", color = NA, alpha = 0.5) +
  geom_sf(data = world_6933, fill = "white", color = NA) +
  geom_sf(
    data = triangles_paa, 
    #mapping = aes(fill = count),
    fill = "#f37748",
    color = NA, size = 0.1,
    alpha = 0.5
  ) +
  # scale_fill_gradientn(
  #   colours = c("lightgray", "#f37748"),
  #   guide = "colorbar"
  # ) +
  # guides(
  #   fill = guide_colorbar(title = "?", barwidth = 20, barheight = 1.5)
  # ) +
  # ggnewscale::new_scale_fill() +
  geom_sf(
    data = triangles_pca, 
    #mapping = aes(fill = count),
    fill = "#095256",
    color = NA, size = 0.1,
    alpha = 0.5
  ) +
  # scale_fill_gradientn(
  #   colours = c("lightgray", "#095256"),
  #   guide = "colorbar"
  # ) +
  # guides(
  #   fill = guide_colorbar(title = "?", barwidth = 20, barheight = 1.5)
  # ) +
  geom_sf(
    data = world_with_count %>%
      dplyr::group_by(area_id) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup(), 
    color = "black", size = 0.1, fill = NA
  ) +
  geom_sf(data = world_6933, fill = NA, color = "black", cex = 0.2) +
  geom_sf(
    data = centroid_pca,
    color = "black", size = 0.4
  ) +
  # geom_sf_text(
  #   data = world_with_count, 
  #   mapping = aes(label = count), color = "black", size = 3
  # ) +
  coord_sf(expand = F, crs = "+proj=natearth") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    panel.grid.major = element_line(colour = "grey", size = 0.3),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  ggtitle(
    paste("Spatial distribution of ancient human individuals in the", "?", "database"),
    paste0(Sys.Date(), ", World in Natural Earth projection")
  )

