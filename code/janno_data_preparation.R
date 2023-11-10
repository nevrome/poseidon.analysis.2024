library(magrittr)

pca_raw <- janno::read_janno("~/agora/community-archive", validate = F)
pca_author_packages <- readr::read_lines("data_tracked/author_submitted_packages.txt")
paa_raw <- janno::read_janno("~/agora/aadr-archive", validate = F)

cleanPoseidonIDs <- function(x) {
  x %>%
    dplyr::mutate(
      Poseidon_ID_simple = 
        Poseidon_ID %>%
        stringr::str_replace(., "\\.HO$", "") %>%
        stringr::str_replace(., "\\.DG$", "") %>%
        stringr::str_replace(., "\\.SG$", "")
    )
}

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
  ) %>%
  cleanPoseidonIDs()
paa <- paa_raw %>%
  dplyr::mutate(
    archive = factor("PAA", levels = c("PAA", "PCA") %>% rev()),
    package = source_file %>% dirname(),
    source = "AADR v54.1.p1",
    main_publication = purrr::map_chr(Publication, \(x) x[[1]])
  ) %>%
  cleanPoseidonIDs()

source_order <- c(
  "AADR v42.4", "AADR v44.3", "AADR v50", "AADR v54.1.p1",
  "Extracted from paper", "Submitted by author"
) %>% rev()
pca$source <- factor(pca$source, levels = source_order)
paa$source <- factor(paa$source, levels = source_order)

pcapaa <- dplyr::bind_rows(pca, paa)

save(
  pca, paa,
  file = "data/janno_data.RData"
)
