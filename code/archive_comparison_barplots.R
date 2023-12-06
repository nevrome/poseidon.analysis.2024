library(magrittr)
library(ggplot2)

#### load janno data ####

load("data/janno_data.RData")
load("data/bib_data.RData")
load("data/bibkey_lookup_hashmap.RData")

#### barplots ####

# publications barplot

publication_per_package <- dplyr::bind_rows(pca, paa) %>%
  dplyr::group_by(archive, package, main_publication) %>%
  dplyr::mutate(main_publication = lookup_paa_key(main_publication)) %>%
  dplyr::summarise(.groups = "drop")

publication_count <- publication_per_package %>%
  dplyr::group_by(archive, package) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::group_by(archive) %>%
  dplyr::arrange(archive, package) %>%
  dplyr::mutate(colour_group = rep_len(c("A", "B"), dplyr::n())) %>%
  dplyr::ungroup()

unique_publication_count <- publication_per_package %>%
  dplyr::distinct(archive, main_publication, .keep_all = T) %>%
  dplyr::group_by(archive) %>%
  dplyr::summarise(unique_n = dplyr::n(), .groups = "drop")

package_publication_plot <- publication_count %>%
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
  geom_point(
    data = unique_publication_count,
    mapping = aes(
      x = archive,
      y = unique_n
    ),
    shape = 18
  ) +
  scale_fill_manual(values = c("A" = "darkgrey", "B" = "lightgrey")) +
  guides(fill = guide_legend(
    title = "Alternating colours for packages:  ",
    label = F
  )) +
  coord_flip() +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.margin = margin(t = -0.25, b = -0.15, unit='cm'),
    legend.justification = "right",
    legend.spacing.x = unit(0, 'cm'),
    plot.title = element_text(size = 11)
  ) +
  ggtitle(
    #"Publications per Poseidon package",
    "Number of main sample-carrying publications represented in each package"
  )

ggsave(
  paste0("plots/figure_barplots_A.pdf"),
  plot = package_publication_plot,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 250, height = 70, units = "mm",
  limitsize = F,
  bg = "white"
)

# publication comparison

keys_with_years <- dplyr::bind_rows(pca_bib, paa_bib) %>%
  dplyr::select(archive, bibtexkey, year) %>%
  dplyr::distinct() %>%
  # select paa key if available, otherwise the pca key
  dplyr::group_by(bibtexkey) %>%
  dplyr::arrange(archive) %>%
  dplyr::slice_head() %>%
  dplyr::ungroup() %>%
  dplyr::select(-archive)

samples_per_publication <- dplyr::bind_rows(pca, paa) %>%
  # dplyr::select(Approx_Individual_ID, archive, Publication = main_publication) %>%
  dplyr::select(Approx_Individual_ID, archive, Publication) %>%
  tidyr::unnest(cols = "Publication") %>%
  dplyr::filter(!grepl("AADR", Publication)) %>%
  dplyr::distinct() %>%
  dplyr::mutate(Publication = lookup_paa_key(Publication)) %>%
  dplyr::group_by(archive, Publication) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  tidyr::complete(archive, Publication, fill = list(n = 0)) %>%
  dplyr::left_join(keys_with_years, by = c("Publication" = "bibtexkey")) %>%
  dplyr::group_split(Publication) %>%
  purrr::map_dfr(
    function(x) {
      if (x$n %>% unique() %>% length() == 1) {
        x %>% dplyr::mutate(availability = "yes")
      } else {
        less <- x %>% dplyr::slice_min(order_by = n)
        more <- x %>% dplyr::slice_max(order_by = n)
        dplyr::bind_rows(
          x %>% dplyr::mutate(availability = "yes", year = more$year),
          less %>% dplyr::mutate(n = more$n - less$n, year = more$year, availability = "no")
        ) 
      }
    }
  ) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(
    Publication = factor(Publication, levels = rev(unique(Publication))),
    archive = factor(archive, levels = c("PCA", "PAA"))
  )

year_separators <- samples_per_publication %>%
  dplyr::filter(archive == "PAA") %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(n = sum(n)) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(n = cumsum(n)) %>%
  dplyr::mutate(year = dplyr::lead(year, n = 1, default = NA)) %>%
  dplyr::filter(year >= 2012)

# samples_per_publication %>% dplyr::filter(availability == "no") %>%
# dplyr::arrange(dplyr::desc(n)) %>% View()

publication_barcode_plot <- samples_per_publication %>%
  ggplot() +
  geom_bar(
    aes(x = archive, y = n, group = Publication, fill = availability),
    stat = "identity"
  ) +
  ggrepel::geom_text_repel(
    data = year_separators %>% dplyr::mutate(x = "PCA"),
    mapping = aes(x = x, y = n, label= year),
    position = position_nudge(x = -0.5),
    angle = 90, direction = "x", segment.color = 'transparent',
    hjust = -0.2,
    size = 2.4,
    box.padding = 0.1
  ) + 
  geom_point(
    data = year_separators %>% dplyr::mutate(x = "PCA"),
    mapping = aes(x = x, y = n),
    position = position_nudge(x = -0.5),
    shape = 17, size = 0.7
  ) + 
  coord_flip() +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_blank(),
    legend.margin = margin(t = -0.25, b = -0.15, unit='cm'),
    legend.justification = "right",
    plot.title = element_text(size = 11)
  ) +
  scale_fill_manual(values = c("yes" = "lightgrey", "no" = "darkgrey")) +
  guides(fill = guide_legend(title = "Is the respective sample in the archive?")) +
  ggtitle(
    #"Samples per Publication",
    "Number of individuals available in each archive for each referenced publication (by year)"
  )

ggsave(
  paste0("plots/figure_barplots_B.pdf"),
  plot = publication_barcode_plot,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 250, height = 70, units = "mm",
  limitsize = F,
  bg = "white"
)

# source barplot

source_count_poseidon_id <- dplyr::bind_rows(pca, paa) %>%
  dplyr::distinct(archive, Poseidon_ID, .keep_all = T) %>%
  dplyr::group_by(archive, source) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")

source_count_approx_ind_id <- dplyr::bind_rows(pca, paa) %>%
  dplyr::distinct(archive, Approx_Individual_ID, .keep_all = T) %>%
  dplyr::group_by(archive, source) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")

source_count <- dplyr::full_join(
  source_count_poseidon_id, source_count_approx_ind_id,
  by = c("archive", "source"), suffix = c("_poseidon_id", "_approx_ind_id")
) %>%
  dplyr::transmute(
    archive, source,
    n_approx_ind_id,
    n_diff = n_poseidon_id - n_approx_ind_id
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("n_"),
    names_to = "count_type",
    values_to = "n"
  ) %>%
  dplyr::mutate(
    count_type = factor(count_type, levels = c("n_diff", "n_approx_ind_id"))
  )

source_plot <- source_count %>%
  ggplot() +
  ggpattern::geom_col_pattern(
    mapping = aes(x = archive, y = n, fill = source, pattern = count_type),
    pattern_color = "white", pattern_fill = "white"
  ) +
  coord_flip() +
  scale_fill_manual(values = wesanderson::wes_palette("IsleofDogs1")) +
  ggpattern::scale_pattern_manual(
    values = c("n_approx_ind_id" = "none", "n_diff" = "stripe"),
    guide = "none"
  ) +
  guides(
    fill = guide_legend(
      title = "Original data source",
      reverse = TRUE,
      override.aes = list(pattern = "none")
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_blank(),
    legend.margin = margin(t = -0.25, b = -0.15, unit='cm'),
    legend.justification = "right",
    plot.title = element_text(size = 11)
  ) +
  ggtitle(
    # "Samples per original data source",
    "Number of individuals/samples by source & primary mechanism of origin"
  )

ggsave(
  paste0("plots/figure_barplots_C.pdf"),
  plot = source_plot,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 250, height = 70, units = "mm",
  limitsize = F,
  bg = "white"
)

#### sankey sources ####

sankey_sources_input <- dplyr::bind_rows(pca, paa) %>%
  dplyr::select(Poseidon_ID, Approx_Individual_ID, archive, source) %>%
  dplyr::mutate(source = factor(source, levels = c(levels(source), "not in archive"))) %>%
  dplyr::group_by(Approx_Individual_ID, archive) %>%
  dplyr::arrange(source) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    id_cols = "Approx_Individual_ID", names_from = "archive", values_from = "source"
  ) %>%
  tidyr::replace_na(list(PCA = "not in archive", PAA = "not in archive")) %>%
  ggsankey::make_long(PCA, PAA)

source_colour_mapping <- wesanderson::wes_palette("IsleofDogs1")[1:6]
names(source_colour_mapping) <- levels(pca$source)

sources_sankey_plot <- sankey_sources_input %>%
  ggplot(
    aes(
      x = x, 
      next_x = next_x, 
      node = node, 
      next_node = next_node,
      fill = factor(node, levels = levels(pca$source)),
      label = node
    )
  ) +
  ggsankey::geom_alluvial(
    flow.alpha = .7,
    width = 0.1,
    #space = 200
  ) +
  labs(x = NULL) +
  scale_fill_manual(values = source_colour_mapping, na.value = "lightblue") +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  guides(fill = guide_legend(title = "Original data source")) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)
  ) +
  coord_flip() +
  ggtitle(
    # "Samples matching across archives",
    "Number of individuals that match across the archives (by data source)"
  )

ggsave(
  paste0("plots/figure_barplots_D.pdf"),
  plot = sources_sankey_plot,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 250, height = 70, units = "mm",
  limitsize = F,
  bg = "white"
)

# dating barplot

dating_count_poseidon_id <- dplyr::bind_rows(pca, paa) %>%
  dplyr::distinct(archive, Poseidon_ID, .keep_all = T) %>%
  dplyr::group_by(archive, Date_Type) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")

dating_count_approx_ind_id <- dplyr::bind_rows(pca, paa) %>%
  dplyr::distinct(archive, Approx_Individual_ID, .keep_all = T) %>%
  dplyr::group_by(archive, Date_Type) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")

dating_count <- dplyr::full_join(
  dating_count_poseidon_id, dating_count_approx_ind_id,
  by = c("archive", "Date_Type"), suffix = c("_poseidon_id", "_approx_ind_id")
) %>%
  tidyr::replace_na(list(Date_Type = "unknown")) %>%
  dplyr::mutate(
    Date_Type = factor(
      Date_Type,
      levels = c("modern", "C14", "contextual", "unknown") %>% rev()
    )
  ) %>%
  dplyr::transmute(
    archive, Date_Type,
    n_approx_ind_id,
    n_diff = n_poseidon_id - n_approx_ind_id
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("n_"),
    names_to = "count_type",
    values_to = "n"
  ) %>%
  dplyr::mutate(
    count_type = factor(count_type, levels = c("n_diff", "n_approx_ind_id"))
  )

dating_plot <- dating_count %>%
  ggplot() +
  ggpattern::geom_col_pattern(
    mapping = aes(x = archive, y = n, fill = Date_Type, pattern = count_type),
    pattern_color = "white", pattern_fill = "white"
  ) +
  coord_flip() +
  scale_fill_manual(values = c("darkgrey", wesanderson::wes_palette("IsleofDogs2")[1:3])) +
  ggpattern::scale_pattern_manual(
    values = c("n_approx_ind_id" = "none", "n_diff" = "stripe"),
    guide = "none"
  ) +
  guides(
    fill = guide_legend(
      title = "Age information",
      reverse = TRUE,
      override.aes = list(pattern = "none")
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_blank(),
    legend.margin = margin(t = -0.25, b = -0.15, unit='cm'),
    legend.justification = "right",
    plot.title = element_text(size = 11)
  ) +
  ggtitle(
    # "Samples with age information",
    "Number of individuals/samples with different types of archaeological age information"
  )

ggsave(
  paste0("plots/figure_barplots_E.pdf"),
  plot = dating_plot,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 250, height = 70, units = "mm",
  limitsize = F,
  bg = "white"
)

# coords barplot

coord_count_poseidon_id <- dplyr::bind_rows(pca, paa) %>%
  dplyr::distinct(archive, Poseidon_ID, .keep_all = T) %>%
  dplyr::mutate(
    has_coordinates = dplyr::case_when(
      !is.na(Latitude) & !is.na(Longitude) ~ "available",
      TRUE ~ "missing"
    )
  ) %>%
  dplyr::group_by(archive, has_coordinates) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")

coord_count_approx_ind_id <- dplyr::bind_rows(pca, paa) %>%
  dplyr::distinct(archive, Approx_Individual_ID, .keep_all = T) %>%
  dplyr::mutate(
    has_coordinates = dplyr::case_when(
      !is.na(Latitude) & !is.na(Longitude) ~ "available",
      TRUE ~ "missing"
    )
  ) %>%
  dplyr::group_by(archive, has_coordinates) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")

coord_count <- dplyr::full_join(
  coord_count_poseidon_id, coord_count_approx_ind_id,
  by = c("archive", "has_coordinates"), suffix = c("_poseidon_id", "_approx_ind_id")
) %>%
  dplyr::transmute(
    archive, has_coordinates,
    n_approx_ind_id,
    n_diff = n_poseidon_id - n_approx_ind_id
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("n_"),
    names_to = "count_type",
    values_to = "n"
  ) %>%
  dplyr::mutate(
    count_type = factor(count_type, levels = c("n_diff", "n_approx_ind_id"))
  )

coord_plot <- coord_count %>%
  ggplot() +
  ggpattern::geom_col_pattern(
    mapping = aes(x = archive, y = n, fill = has_coordinates, pattern = count_type),
    pattern_color = "white", pattern_fill = "white"
  ) +
  coord_flip() +
  #scale_fill_manual(values = wesanderson::wes_palette("GrandBudapest1")) +
  scale_fill_manual(values = c("lightgrey", "darkgrey")) +
  ggpattern::scale_pattern_manual(
    values = c("n_approx_ind_id" = "none", "n_diff" = "stripe"),
    guide = "none"
  ) +
  guides(
    fill = guide_legend(
      title = "Coordinate information",
      reverse = TRUE,
      override.aes = list(pattern = "none")
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_blank(),
    legend.margin = margin(t = -0.25, b = -0.15, unit='cm'),
    legend.justification = "right",
    plot.title = element_text(size = 11)
  ) +
  ggtitle(
    #"Samples with spatial coordinates",
    "Number of individuals/samples with latitude and longitude coordinates"
  )

ggsave(
  paste0("plots/figure_barplots_F.pdf"),
  plot = coord_plot,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 250, height = 70, units = "mm",
  limitsize = F,
  bg = "white"
)

# combine plots

p <- cowplot::plot_grid(
  package_publication_plot, publication_barcode_plot, source_plot, sources_sankey_plot, dating_plot, coord_plot,
  nrow = 3, ncol = 2, align = "v", axis = "tb",
  labels = c("A", "B", "C", "D", "E", "F")
)

ggsave(
  paste0("plots/figure_barplots.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 500, height = 220, units = "mm",
  limitsize = F,
  bg = "white"
)

