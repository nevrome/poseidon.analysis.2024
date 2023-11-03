library(magrittr)

pca <- janno::read_janno("~/agora/community-archive", validate = F)
paa <- janno::read_janno("~/agora/aadr-archive", validate = F)

pca_samples <- pca %>% dplyr::transmute(Poseidon_ID, pca = T)
paa_samples <- paa %>% dplyr::transmute(Poseidon_ID, paa = T)

pca_paa_full_join <- dplyr::full_join(
  pca_samples, paa_samples, by = "Poseidon_ID"
) %>%
  tidyr::replace_na(list(pca = FALSE, paa = FALSE))

pca_paa_full_join %>%
  dplyr::filter(!(pca & paa)) %>% View()

pca_paa_full_fuzzyjoin <- zoomerjoin::jaccard_full_join(
  pca_samples, paa_samples,
  by = "Poseidon_ID",
  clean = T
)

pca_paa_full_fuzzyjoin %>%
  dplyr::filter(
    Poseidon_ID.x != Poseidon_ID.y
  ) %>% View()
