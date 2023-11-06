library(magrittr)
library(ggplot2)

#### prepare sample data ####

pca_raw <- janno::read_janno("~/agora/community-archive", validate = F)
pca_author_packages <- readr::read_lines("data_tracked/author_submitted_or_maintained_packages.txt")
paa_raw <- janno::read_janno("~/agora/aadr-archive", validate = F)

pca <- pca_raw %>%
  dplyr::mutate(
    archive = "PCA",
    package = source_file %>% dirname(),
    author_submitted = package %in% pca_author_packages
  )
paa <- paa_raw %>%
  dplyr::mutate(
    archive = "PAA",
    package = source_file %>% dirname(),
    author_submitted = FALSE
  )

#### prepare source barplot ####

hu <- dplyr::bind_rows(pca, paa) %>%
  dplyr::group_by(archive, author_submitted) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")

hu %>%
  ggplot() +
  geom_bar(
    mapping = aes(x = archive, y = n, fill = author_submitted),
    stat = "identity"
  )

#### prepare dating barplot ####

dating_count <- dplyr::bind_rows(pca, paa) %>%
  dplyr::group_by(archive, Date_Type) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  tidyr::replace_na(list(Date_Type = "unknown")) %>%
  dplyr::mutate(
    Date_Type = factor(
      Date_Type,
      levels = c("modern", "C14", "contextual", "unknown") %>% rev()
    )
  )

dating_count %>%
  ggplot() +
  geom_bar(
    mapping = aes(x = archive, y = n, fill = Date_Type),
    stat = "identity"
  )

#### mapping barplot ####

gu <- dplyr::bind_rows(pca, paa) %>%
  dplyr::mutate(
    has_coordinates = dplyr::case_when(
      Date_Type == "modern" ~ "modern",
      !is.na(Latitude) & !is.na(Longitude) ~ "ancient with coordinates",
      TRUE ~ "ancient without coordinates"
    )
  ) %>%
  dplyr::group_by(archive, has_coordinates) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")

gu %>%
  ggplot() +
  geom_bar(
    mapping = aes(x = archive, y = n, fill = has_coordinates),
    stat = "identity"
  )

#### package count figure

#### map figure ####

# make sample data spatial

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

pca_ancient_author_submitted_sf_6933 <- pca_ancient_sf_6933 %>%
  dplyr::filter(package %in% pca_author_packages)

paa_ancient_sf_6933 <- paa_ancient_with_coords %>%
  sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326) %>%
  sf::st_transform(6933)

# prepare context data

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

# perform counting in spatial bins

inter_world <- function(x) {
  sf::st_intersects(world_grid_6933, x) %>% lengths()
}

world_with_count <- world_grid_6933 %>%
  dplyr::mutate(
    PCA = pca_ancient_sf_6933 %>% inter_world(),
    PCA_authors_submitted = pca_ancient_author_submitted_sf_6933 %>% inter_world(),
    PAA = paa_ancient_sf_6933 %>% inter_world()
  ) %>%
  tidyr::pivot_longer(
    tidyselect::one_of("PCA", "PAA", "PCA_authors_submitted"),
    names_to = "database", values_to = "count"
  ) %>%
  dplyr::filter(count != 0)

centroid_pca_author_submitted <- world_with_count %>%
  dplyr::filter(database == "PCA_authors_submitted") %>%
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

# construct map figure

ggplot() +
  geom_sf(data = extent_world_6933, fill = "#c2eeff", color = NA, alpha = 0.5) +
  geom_sf(data = world_6933, fill = "white", color = NA) +
  geom_sf(
    data = triangles_paa, 
    fill = "#f37748",
    color = NA, size = 0.1,
    alpha = 0.5
  ) +
  geom_sf(
    data = triangles_pca, 
    fill = "#095256",
    color = NA, size = 0.1,
    alpha = 0.5
  ) +
  geom_sf(
    data = world_with_count %>%
      dplyr::group_by(area_id) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup(), 
    color = "black", size = 0.1, fill = NA
  ) +
  geom_sf(data = world_6933, fill = NA, color = "black", cex = 0.2) +
  geom_sf(
    data = centroid_pca_author_submitted,
    color = "#042325", size = 0.4
  ) +
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

