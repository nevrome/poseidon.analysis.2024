library(magrittr)
library(ggplot2)

#### load janno data ####

load("data/janno_data.RData")
load("data/bib_data.RData")

#### count individual-proxy per year ####

individuals_with_year <- paa %>%
  dplyr::filter(Date_Type != "modern") %>%
  dplyr::select(Approx_Individual_ID, Publication) %>%
  dplyr::mutate(Publication = purrr::map_chr(Publication, \(x) x[[1]])) %>%
  dplyr::left_join(paa_bib, by = c("Publication" = "bibtexkey")) %>%
  dplyr::select(Approx_Individual_ID, year) %>%
  dplyr::arrange(dplyr::desc(year)) %>%
  dplyr::distinct(Approx_Individual_ID, .keep_all = TRUE)

individuals_per_year <- individuals_with_year %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(n = dplyr::n())

# control with the AADR_Year_First_Publication column
# paa %>%
#   dplyr::filter(Date_Type != "modern") %>%
#   dplyr::select(Poseidon_ID, AADR_Year_First_Publication) %>%
#   dplyr::group_by(AADR_Year_First_Publication) %>%
#   dplyr::summarise(dplyr::n())

#### prepare plot ####

individuals_per_year %>%
  dplyr::filter(year < 2023) %>%
  ggplot() +
  geom_col(aes(x = year, y = n))



