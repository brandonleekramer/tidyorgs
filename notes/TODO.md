
[Package Development CSV](https://docs.google.com/spreadsheets/d/1zhqUYRuQg97WCycyfMiNmMpr00Ke_bZ9G2BYpl3PYTw/edit#gid=1497511867)

- Expand warning messages in each function with helper function 

  - Warnings for not being a dataframe, not being character or numeric, variable not being entered properly (see https://stackoverflow.com/questions/25640161/r-how-to-test-for-character0-in-if-statement)

- Still need to fix Harvard errors 

- Need to fix wisconsin madison issue

- Need to fix duplicate entries in like 6-10 universities 

- Add country_code parameter 

- Add in state and/or lat/long

- Add in private public 

[] The goal of the `standardize_*` verbs would be to facilitate merges across multiple academic datasets. During the fall, we want to work on integrating the following datasets: ]

- [UMETRICS Data](https://www.census.gov/programs-surveys/ces/data/restricted-use-data/umetrics-data.html)

- [IRIS Data](https://iris.isr.umich.edu/research-data/access/)

- [IPEDS Data](https://nces.ed.gov/ipeds/find-your-college)

- [AAUP Data](https://www.aaup.org/2020-21-faculty-compensation-survey-results)

[] Other tasks to complete: 

- [Add in spellcheck](https://cran.r-project.org/web/packages/hunspell/vignettes/intro.html)

- [Build pkgdown website](https://pkgdown.r-lib.org/)

- [Validate tidy principles](https://principles.tidyverse.org/structure.html)

- README 

[] Resources that have been helpful so far: 

- [r-pkgs](https://r-pkgs.org/index.html)

- [tidyr](https://github.com/tidyverse/tidyr/tree/v1.1.2/data-raw)

- [tidytable](https://github.com/markfairbanks/tidytable/blob/main/R/filter.R)

- [tidytext](https://github.com/juliasilge/tidytext/blob/master/R/dictionary_tidiers.R)