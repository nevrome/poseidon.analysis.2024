library(magrittr)
library(ggplot2)

#### load janno data ####

load("data/janno_data.RData")
load("data/bib_data.RData")
load("data/bibkey_lookup_hashmap.RData")

pca_bib_linked_to_samples <- pca %>%
  dplyr::select(Publication) %>%
  tidyr::unnest(cols = c("Publication")) %>%
  dplyr::distinct() %>%
  dplyr::left_join(
    pca_bib, by = c("Publication" = "bibtexkey")
  ) %>%
  dplyr::mutate(
    Publication = lookup_paa_key(Publication)
  )

paa_bib_linked_to_samples <- paa %>%
  dplyr::select(Publication) %>%
  tidyr::unnest(cols = c("Publication")) %>%
  dplyr::distinct() %>%
  dplyr::left_join(
    paa_bib, by = c("Publication" = "bibtexkey")
  )

publication_overlap <- dplyr::full_join(
  pca_bib_linked_to_samples,
  paa_bib_linked_to_samples,
  by = c("Publication", "year"),
  suffix = c("_PCA", "_PAA")
) %>%
  dplyr::arrange(year, Publication) %>%
  dplyr::mutate(
    plot_group = sort(rep_len(c("A", "B", "C", "D"), length.out = dplyr::n()))
  ) %>%
  tidyr::pivot_longer(
    cols = c("archive_PCA", "archive_PAA"),
    values_to = "archive"
  ) %>%
  dplyr::filter(!is.na(archive)) %>%
  dplyr::arrange(year, Publication) %>%
  dplyr::mutate(
    Publication = factor(Publication, levels = rev(unique(Publication)))
  )

# table_for_stephan <- dplyr::full_join(
#   pca_bib_linked_to_samples,
#   paa_bib_linked_to_samples,
#   by = c("Publication", "year"),
#   suffix = c("_PCA", "_PAA")
# ) %>%
#   dplyr::group_by(Publication) %>%
#   dplyr::summarize(
#     year = min(year),
#     doi = na.omit(c(doi_PAA, doi_PCA))[1],
#     archive_PCA = any(!is.na(archive_PCA)),
#     archive_PAA = any(!is.na(archive_PAA))
#   ) %>%
#   dplyr::arrange(year, Publication) %>%
#   dplyr::filter(!is.na(doi))
# 
# readr::write_csv(table_for_stephan, file = "publication_list.csv")

make_one_figure <- function(x) {
  x %>%
    ggplot() +
    geom_point(
      aes(
        y = Publication,
        x = archive
      ),
      shape = 15
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 8, hjust = 0),
      axis.text.x = element_text(size = 8, angle = 90),
      panel.grid.major = element_line(linewidth = 0.5),
      axis.title = element_blank()
    ) +
    scale_y_discrete(position = "right")
}

p_list <- purrr::map(
  c("A", "B", "C", "D"),
  function(x) {
    publication_overlap %>%
      dplyr::filter(plot_group == x) %>%
      make_one_figure()
  }
)

p <- cowplot::plot_grid(
  plotlist = p_list, nrow = 1, ncol = 4,
  align = "v"
)

ggsave(
  paste0("plots/figure_publication_overlap.pdf"),
  plot = p,
  device = "pdf",
  scale = 0.7,
  dpi = 300,
  width = 500, height = 280, units = "mm",
  limitsize = F,
  bg = "white"
)

