library(magrittr)

#### prepare .janno file data ####

pca_raw <- janno::read_janno("~/agora/community-archive", validate = F)
pca_author_packages <- readr::read_lines("data_tracked/author_submitted_packages.txt")
paa_raw <- list.files("~/agora/aadr-archive", pattern = "62", full.names = TRUE) %>%
  janno::read_janno(validate = F)

cleaningPatterns <- c(
  "\\.HO",
  "\\.DG",
  "\\.SG",
  "\\.SDG",
  "\\.AG",
  "\\_WGA",
  "\\_noUDG",
  "\\_udg",
  "\\.WGC",
  "\\_d",
  "\\_old",
  "\\_new",
  "\\_alt",
  "\\.EC",
  "\\_genotyping",
  "\\_enhanced",
  "\\_snpAD",
  "\\_merged",
  "\\.merged",
  "\\_merge",
  "\\_lib",
  "\\_renamed",
  "\\_in.preparation",
  "\\_contam",
  "\\.cont",
  "\\.A0101",
  "\\.A",
  "\\.B0101",
  "\\.B",
  "\\_v54.1_addback",
  "\\_petrous",
  "\\_published",
  "\\_all",
  "\\_provisional",
  "\\_final",
  "-ALL_DATA",
  "\\.minus",
  "\\.bam",
  "\\.sorted",
  "\\.fixedHeader",
  "\\_oEEF",
  "\\_LC",
  "\\_ss",
  "_original"
) %>% paste0(collapse = "|")

cleanPoseidonIDs <- function(x) {
  x %>%
    dplyr::mutate(
      Approx_Individual_ID = stringr::str_remove_all(
        Poseidon_ID, cleaningPatterns
      ) %>%
        stringr::str_replace_all(., "\\+", "\\_"),
      .before = "Poseidon_ID"
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
    main_publication = purrr::map_chr(Publication, \(x) x[[1]]),
    .before = "Poseidon_ID"
  ) %>%
  cleanPoseidonIDs()

#janno::write_janno(pca, "poseidon_community_archive_2024-07-15.tsv")

paa <- paa_raw %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    archive = factor("PAA", levels = c("PAA", "PCA") %>% rev()),
    package = source_file %>% dirname(),
    source = "AADR v62.0",
    main_publication = purrr::map_chr(Publication, \(x) x[[1]])
  ) %>%
  cleanPoseidonIDs()

paa %>%
  dplyr::mutate(main_id = purrr::map_chr(Alternative_IDs, \(x) x[[1]]), .after = "Approx_Individual_ID") %>%
  dplyr::filter(
    Approx_Individual_ID != main_id
  )

# check for avoidable mismatches in Approx_Individual_ID
# dplyr::bind_rows(pca, paa) %>%
#   dplyr::select(Approx_Individual_ID, archive) %>%
#   dplyr::distinct() %>%
#   dplyr::group_by(Approx_Individual_ID) %>%
#   dplyr::mutate(n = dplyr::n()) %>%
#   dplyr::filter(n == 1) %>%
#   dplyr::ungroup() %>%
#   dplyr::arrange(Approx_Individual_ID) %>%
#   dplyr::filter(dplyr::lag(as.character(archive), n = 1, default = "") != as.character(archive)) %>%
#   View()
# very similar entries should appear right after each other

# set levels of source factor
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
  list.files(
    "~/agora/community-archive",
    pattern = "\\.bib$",
    full.names = T,
    recursive = T
  ),
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
  list.files(
    "~/agora/aadr-archive",
    pattern = "\\.bib$",
    full.names = T,
    recursive = T
  ),
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

#### prepare bib key lookup PCA -> PAA ####

# The following code is commented out, because it was only used run once to prepare the
# original template for "data_tracked/bibkey_mapping.csv", which was then manually
# cleaned

# pca_bib_linked_to_samples <- pca %>%
#   dplyr::select(Publication) %>%
#   tidyr::unnest(cols = c("Publication")) %>%
#   dplyr::distinct() %>%
#   dplyr::left_join(
#     pca_bib, by = c("Publication" = "bibtexkey")
#   )
# 
# paa_bib_linked_to_samples <- paa %>%
#   dplyr::select(Publication) %>%
#   tidyr::unnest(cols = c("Publication")) %>%
#   dplyr::distinct() %>%
#   dplyr::left_join(
#     paa_bib, by = c("Publication" = "bibtexkey")
#   )
#
# publication_overlap <- dplyr::full_join(
#   pca_bib_linked_to_samples,
#   paa_bib_linked_to_samples,
#   by = c("Publication", "year"),
#   suffix = c("_PCA", "_PAA"),
#   keep = TRUE
# )
# 
# publication_overlap %>%
#   dplyr::select(Publication_PAA, doi_PAA, Publication_PCA, doi_PCA) %>%
#   readr::write_csv("data_tracked/bibkey_mapping.csv")

bibkey_lookup_table <- readr::read_csv("data_tracked/bibkey_mapping.csv") %>%
  dplyr::filter(!is.na(Publication_PCA))

bibkey_lookup_hashmap <- hash::hash(
  bibkey_lookup_table$Publication_PCA,
  bibkey_lookup_table$Publication_PAA
)

lookup_paa_key <- function(pca_keys) {
  purrr::map_chr(pca_keys, function(pca_key) {
    if (!hash::has.key(pca_key, bibkey_lookup_hashmap)) {
      pca_key
    } else {
      paa_key <- hash::values(bibkey_lookup_hashmap, pca_key)
      if (!is.na(paa_key)) {
        paa_key
      } else {
        pca_key
      }
    }
  })
}

save(
  bibkey_lookup_hashmap, lookup_paa_key,
  file = "data/bibkey_lookup_hashmap.RData"
)
