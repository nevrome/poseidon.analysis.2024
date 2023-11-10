library(magrittr)

#### prepare .janno file data ####

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
  tibble::as_tibble() %>%
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
  tibble::as_tibble() %>%
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

save(
  pca, paa,
  file = "data/janno_data.RData"
)

#### prepare .bib file data ####

pca_bib_raw <- purrr::map_dfr(
  list.files("~/agora/community-archive", pattern = "\\.bib$", full.names = T, recursive = T),
  function(bib_path) {
    bib_df <- bib2df::bib2df(bib_path)
    bib_df %>% dplyr::mutate(
      package = bib_path %>% dirname() %>% basename(),
      archive = "PCA"
    )
  }
)

pca_bib <- pca_bib_raw %>%
  dplyr::transmute(
    bibtexkey = BIBTEXKEY,
    doi = DOI,
    year = YEAR,
    archive = "PCA"
  ) %>%
  dplyr::group_by(bibtexkey) %>%
  dplyr::arrange(doi) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup()

paa_bib_raw <- purrr::map_dfr(
  list.files("~/agora/aadr-archive", pattern = "\\.bib$", full.names = T, recursive = T),
  function(bib_path) {
    bib_df <- bib2df::bib2df(bib_path)
    bib_df %>% dplyr::mutate(
      package = bib_path %>% dirname() %>% basename(),
      archive = "PAA"
    )
  }
)

paa_bib <- paa_bib_raw %>%
  dplyr::transmute(
    bibtexkey = BIBTEXKEY,
    doi = DOI,
    year = YEAR,
    archive = "PAA"
  ) %>%
  dplyr::distinct(.keep_all = T)

save(
  pca_bib, paa_bib,
  file = "data/bib_data.RData"
)
