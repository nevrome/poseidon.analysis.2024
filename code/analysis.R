library(magrittr)
library(ggplot2)

#### prepare sample data ####

pca_raw <- janno::read_janno("~/agora/community-archive", validate = F)
pca_author_packages <- readr::read_lines("data_tracked/author_submitted_packages.txt")
paa_raw <- janno::read_janno("~/agora/aadr-archive", validate = F)

pca <- pca_raw %>%
  dplyr::mutate(
    archive = factor("PCA", levels = c("PAA", "PCA") %>% rev()),
    package = source_file %>% dirname(),
    source = dplyr::case_when(
      package %in% pca_author_packages                    ~ "Submitted by author",
      purrr::map_lgl(Publication, \(x) "AADRv424" %in% x) ~ "AADR v42.4",
      purrr::map_lgl(Publication, \(x) "AADRv443" %in% x) ~ "AADR v44.3",
      purrr::map_lgl(Publication, \(x) "AADRv50" %in% x)  ~ "AADR v50",
      TRUE                                                ~ "Extracted from paper"
    ),
    main_publication = purrr::map_chr(Publication, \(x) x[[1]])
  )
paa <- paa_raw %>%
  dplyr::mutate(
    archive = factor("PAA", levels = c("PAA", "PCA") %>% rev()),
    package = source_file %>% dirname(),
    source = "AADR v54.1.p1",
    main_publication = purrr::map_chr(Publication, \(x) x[[1]])
  )

source_order <- c(
  "AADR v42.4", "AADR v44.3", "AADR v50", "AADR v54.1.p1",
  "Extracted from paper", "Submitted by author"
  ) %>% rev()
pca$source <- factor(pca$source, levels = source_order)
paa$source <- factor(paa$source, levels = source_order)

#### barplots ####

# publications barplot

publication_per_package <- dplyr::bind_rows(pca, paa) %>%
  dplyr::group_by(archive, package, main_publication) %>%
  dplyr::summarise(.groups = "drop")

publication_count <- publication_per_package %>%
  dplyr::group_by(archive, package) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::group_by(archive) %>%
  dplyr::arrange(archive, package) %>%
  dplyr::mutate(colour_group = rep_len(c("A", "B"), dplyr::n())) %>%
  dplyr::ungroup()

exclusive_publication_count_PAA <- publication_per_package %>%
  dplyr::filter(archive == "PAA") %>%
  dplyr::select(-archive) %>%
  dplyr::group_by(main_publication) %>%
  dplyr::filter(dplyr::n() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(package) %>%
  dplyr::summarise(exclusive_n = dplyr::n())

publication_count_with_exclusive_count <- publication_count %>%
  dplyr::left_join(exclusive_publication_count_PAA, by = "package")

publication_plot <- publication_count_with_exclusive_count %>%
  ggplot() +
  geom_bar(
    mapping = aes(
      x = archive,
      y = n,
      group = factor(package, levels = package),
      fill = colour_group
    ),
    stat = "identity"
  ) +
  geom_label(
    mapping = aes(
      x = archive,
      y = n,
      group = factor(package, levels = package),
      label = exclusive_n
    ),
    position = position_stack(vjust = 0.5)
  ) +
  scale_fill_manual(values = c("A" = "lightgrey", "B" = "darkgrey")) +
  guides(fill = guide_legend(
    title = "Alternating colours for the packages"
  )) +
  coord_flip() +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    legend.position = "bottom"
  ) +
  ggtitle("Publications per Poseidon package")

# source barplot

source_count <- dplyr::bind_rows(pca, paa) %>%
  dplyr::group_by(archive, source) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")

source_plot <- source_count %>%
  ggplot() +
  geom_bar(
    mapping = aes(x = archive, y = n, fill = source),
    stat = "identity"
  ) +
  coord_flip() +
  scale_fill_manual(values = wesanderson::wes_palette("IsleofDogs1")) +
  guides(fill = guide_legend(title = "Original data source", reverse = TRUE)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_blank()
  ) +
  ggtitle("Samples per original data source")

# dating barplot

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

dating_plot <- dating_count %>%
  ggplot() +
  geom_bar(
    mapping = aes(x = archive, y = n, fill = Date_Type),
    stat = "identity"
  ) +
  coord_flip() +
  scale_fill_manual(values = wesanderson::wes_palette("IsleofDogs2")) +
  guides(fill = guide_legend(title = "Age information", reverse = TRUE)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_blank()
  ) +
  ggtitle("Samples with age information")

# coords barplot

coord_count <- dplyr::bind_rows(pca, paa) %>%
  dplyr::mutate(
    has_coordinates = dplyr::case_when(
      !is.na(Latitude) & !is.na(Longitude) ~ "available",
      TRUE ~ "missing"
    )
  ) %>%
  dplyr::group_by(archive, has_coordinates) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")

coord_plot <- coord_count %>%
  ggplot() +
  geom_bar(
    mapping = aes(x = archive, y = n, fill = has_coordinates),
    stat = "identity"
  ) +
  coord_flip() +
  scale_fill_manual(values = wesanderson::wes_palette("GrandBudapest1")) +
  guides(fill = guide_legend(title = "Coordinate information", reverse = TRUE)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_blank()
  ) +
  ggtitle("Samples with spatial coordinates")

# combine barplots

p <- cowplot::plot_grid(
  publication_plot, source_plot, dating_plot, coord_plot,
  nrow = 2, ncol = 2, align = "hv", axis = "tb",
  labels = c("A", "B", "C", "D")
)

ggsave(
  paste0("plots/figure_barplots.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 500, height = 170, units = "mm",
  limitsize = F,
  bg = "white"
)

#### space-time figure ####

# filter datasets to spatiotemporally informed subset

pca_ancient_with_coords <- pca %>%
  dplyr::filter(
    Date_Type %in% c("C14", "contextual"),
    !is.na(Date_BC_AD_Start) & !is.na(Date_BC_AD_Stop)
  ) %>%
  dplyr::filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  tibble::as_tibble()

paa_ancient_with_coords <- paa %>%
  dplyr::filter(
    Date_Type %in% c("C14", "contextual"),
    !is.na(Date_BC_AD_Start) & !is.na(Date_BC_AD_Stop)
  ) %>%
  dplyr::filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  tibble::as_tibble()

# make sample data spatial

pca_ancient_sf_6933 <- pca_ancient_with_coords %>%
  sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326) %>%
  sf::st_transform(6933)

pca_ancient_author_submitted_sf_6933 <- pca_ancient_sf_6933 %>%
  dplyr::filter(package %in% pca_author_packages)

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

map_plot <- ggplot() +
  geom_sf(data = extent_world_6933, fill = "#c2eeff", color = "black", alpha = 0.5) +
  geom_sf(data = world_6933, fill = "white", color = NA) +
  geom_sf(
    data = triangles_paa, 
    fill = "#f37748",
    color = NA,
    alpha = 0.5
  ) +
  geom_sf(
    data = triangles_pca, 
    fill = "#095256",
    color = NA,
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
    color = "#042325",
    shape = 18, size = 2.5
  ) +
  coord_sf(expand = F, crs = "+proj=natearth") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    panel.grid.major = element_line(colour = "grey", linewidth = 0.3),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  ggtitle(
    paste("Spatial and temporal distribution of ancient samples in PAA and PCA"),
    paste0(Sys.Date(), ", World in Natural Earth projection")
  )

# time histogram

samples_with_mean_age <- dplyr::bind_rows(pca_ancient_with_coords, paa_ancient_with_coords) %>%
  dplyr::select(Poseidon_ID, tidyselect::starts_with("Date_BC_AD"), package, archive, source) %>%
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

age_groups_author_submitted <- samples_with_mean_age %>%
  dplyr::filter(
    archive == "PCA",
    source == "Submitted by author"
  ) %>%
  dplyr::group_by(age_cut) %>%
  dplyr::summarise()

age_groups_count <- samples_with_mean_age %>%
  dplyr::group_by(archive, age_cut) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")

time_hist_plot <- ggplot() +
  geom_bar(
    data = age_groups_count,
    mapping = aes(x = age_cut, y = n, fill = archive),
    stat = "identity",
    position = "dodge",
    alpha = 0.7
  ) +
  geom_point(
    data = age_groups_author_submitted,
    mapping = aes(x = age_cut, y = -80, color = "Author-submitted samples"),
    shape = 18, size = 2.5
  ) +
  scale_color_manual("", values = c("Author-submitted samples" = "black")) +
  ylim(-100, 1500) +
  theme_bw() +
  theme(
    legend.position = c(.7,0.15),
    legend.box.background = element_rect(colour = "black", fill = "white"),
    legend.box.margin = margin(0,0,0,0),
    legend.background = element_rect(fill = NA),
    #legend.box = "vertical",
    legend.spacing.y = unit(-0.1, "cm"),
    axis.text.y = element_text(angle = 20, hjust = 1, vjust = 0.5),
    axis.title.x = element_blank()
  ) +
  scale_fill_manual(
    values = c("PAA" = "#f37748", "PCA" = "#095256")
  ) +
  guides(
    fill = guide_legend(title = "Archive", direction = "horizontal")
    #color = guide_legend()
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
  paste0("plots/figure_spacetime.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 500, height = 220, units = "mm",
  limitsize = F,
  bg = "white"
)

