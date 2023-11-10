library(magrittr)
library(ggplot2)

#### load janno data ####

load("data/janno_data.RData")

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

unique_publication_count <- publication_per_package %>%
  dplyr::distinct(archive, main_publication, .keep_all = T) %>%
  dplyr::group_by(archive) %>%
  dplyr::summarise(unique_n = dplyr::n(), .groups = "drop")

publication_plot <- publication_count %>%
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
    )
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
  )# +
  #ggtitle("Publications per Poseidon package")

# publication comparison

load("data/bib_data.RData")
keys_with_years <- dplyr::bind_rows(pca_bib, paa_bib) %>%
  dplyr::select(archive, bibtexkey, year)

samples_per_publication <- dplyr::bind_rows(pca, paa) %>%
  dplyr::select(Poseidon_ID, archive, Publication) %>%
  tidyr::unnest(cols = "Publication") %>%
  dplyr::filter(!grepl("AADR", Publication)) %>%
  dplyr::group_by(archive, Publication) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  tidyr::complete(archive, Publication, fill = list(n = 0)) %>%
  dplyr::left_join(keys_with_years, by = c("archive", "Publication" = "bibtexkey")) %>%
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
  dplyr::mutate(year = dplyr::lead(year, n = 1, default = 2023)) %>%
  dplyr::filter(year >= 2012)

# samples_per_publication %>% dplyr::filter(availability == "no") %>% dplyr::arrange(dplyr::desc(n)) %>% View()

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
    position = position_nudge(x = -0.5)
  ) + 
  coord_flip() +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_blank()
  ) +
  scale_fill_manual(values = c("yes" = "lightgrey", "no" = "darkgrey")) +
  guides(fill = guide_legend(title = "Is the respecitve sample in the archive?"))

p <- cowplot::plot_grid(
  publication_plot, publication_barcode_plot, source_plot, sources_sankey_plot, dating_plot, coord_plot,
  nrow = 3, ncol = 2, align = "v", axis = "tb",
  labels = c("A", "B", "C", "D", "E", "F")
)

ggsave(
  paste0("plots/figure_barplots3.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 500, height = 220, units = "mm",
  limitsize = F,
  bg = "white"
)


# source barplot

source_count <- dplyr::bind_rows(pca, paa) %>%
  dplyr::distinct(archive, Poseidon_ID_simple, .keep_all = T) %>%
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
  ) #+
  #ggtitle("Samples per original data source")

# dating barplot

dating_count <- dplyr::bind_rows(pca, paa) %>%
  dplyr::distinct(archive, Poseidon_ID_simple, .keep_all = T) %>%
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
  )# +
  #ggtitle("Samples with age information")

# coords barplot

coord_count <- dplyr::bind_rows(pca, paa) %>%
  dplyr::distinct(archive, Poseidon_ID_simple, .keep_all = T) %>%
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
  )# +
  #ggtitle("Samples with spatial coordinates")

#### sankey sources ####

sankey_sources_input <- dplyr::bind_rows(pca, paa) %>%
  dplyr::select(Poseidon_ID_simple, archive, source) %>%
  dplyr::distinct(Poseidon_ID_simple, archive, .keep_all = T) %>%
  dplyr::mutate(source = factor(source, levels = c(levels(source), "not in archive"))) %>%
  tidyr::pivot_wider(id_cols = "Poseidon_ID_simple", names_from = "archive", values_from = "source") %>%
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
    plot.title = element_text(hjust = .5),
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank()
  ) +
  coord_flip()

# combine plots

p <- cowplot::plot_grid(
  publication_plot, publication_barcode_plot, source_plot, sources_sankey_plot, dating_plot, coord_plot,
  nrow = 3, ncol = 2, align = "v", axis = "tb",
  labels = c("A", "B", "C", "D", "E", "F")
)

ggsave(
  paste0("plots/figure_barplots3.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 500, height = 220, units = "mm",
  limitsize = F,
  bg = "white"
)

