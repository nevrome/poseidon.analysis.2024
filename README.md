## Data analysis code for 'Poseidon -- A framework for archaeogenetic human genotype data management'

### Published in:

to be announced

### Compendium DOI (long-term archive):

to be announced

The files in this archived storage will generate the results as found in the publication. The files hosted on GitHub are the development versions and may have changed since the paper was published.

### Authors of this repository:

- Clemens Schmid [![ORCiD](https://img.shields.io/badge/ORCiD-0000--0003--3448--5715-green.svg)](http://orcid.org/0000-0003-3448-5715)

### Overview of contents:

This repository contains the following main top level directories:

- `code`: The R scripts necessary to reproduce the analysis and create the figures. Some scripts provide code beyond what is required to reproduce figures and results in the publication (e.g. scripts to create didactic figures for presentations).
- `data`: Intermediate data output by the scripts, not tracked by Git.
- `data_tracked`: Input data files manually or only semi-automatically created for this analysis.
- `plots`: Rendered versions of the plots for the publication, not tracked by Git.
- `schemata`: Schematic drawings created for the paper.

### Reproducing the results:

The following versions of the Poseidon public archives downloaded on 2024-03-15 were analysed:

|archive     |Git commit | Corresponding package versions |
|:-----------|:----------|:----------------------------------|
|PCA         |[b159991](https://github.com/poseidon-framework/community-archive/tree/b159991)|[archive.chron](https://github.com/poseidon-framework/community-archive/blob/b15999124b357794fbc9b2e18fe86dbcd9947788/archive.chron)|
|PAA         |[60ddda0](https://github.com/poseidon-framework/aadr-archive/tree/60ddda0)|[archive.chron](https://github.com/poseidon-framework/aadr-archive/blob/60ddda0/archive.chron)|

The archive.chron files include all present and past package versions, but only the latest ones were considered here.

The analysis was performed with R v4.3.2 using the following packages and package versions as available on CRAN on 2024-03-13:

<details>
<summary>Generated with this code</summary>
  
```r
tibble::tibble(
  package = rrtools::add_dependencies_to_description(just_packages = T),
  version = purrr::map_chr(package, \(x) utils::packageVersion(x) |> as.character())
) |> knitr::kable()

```
</details>

|package     |version   |
|:-----------|:---------|
|bib2df      |1.1.1     |
|cowplot     |1.1.3     |
|dplyr       |1.1.4     |
|ggpattern   |1.0.1     |
|ggplot2     |3.5.0     |
|ggrepel     |0.9.5     |
|ggsankey    |0.0.99999 |
|giscoR      |0.4.0     |
|hash        |2.2.6.3   |
|janno       |1.0.0     |
|magrittr    |2.0.3     |
|purrr       |1.0.2     |
|readr       |2.1.5     |
|scales      |1.3.0     |
|sf          |1.0.15    |
|stringr     |1.5.1     |
|tibble      |3.2.1     |
|tidyr       |1.3.1     |
|tidyselect  |1.2.1     |
|wesanderson |0.3.7     |

### Licenses:

[![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/) year: 2024, copyright holder: Clemens Schmid
