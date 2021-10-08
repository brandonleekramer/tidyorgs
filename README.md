
# tidyorgs: A tidy package that standardizes text data for organizational and geographic analysis

**Authors:** [Brandon Kramer](https://www.brandonleekramer.com/) with
contributions from members of the [University of Virginia’s
Biocomplexity
Institute](https://biocomplexity.virginia.edu/institute/divisions/social-and-decision-analytics),
the [National Center for Science and Engineering
Statistics](https://www.nsf.gov/statistics/), and the
[2020](https://dspg-young-scholars-program.github.io/dspg20oss/team/?dspg)
and [2021](https://dspgtools.shinyapps.io/dspg21oss/) [UVA Data Science
for the Public Good Open Source Software
Teams](https://biocomplexity.virginia.edu/institute/divisions/social-and-decision-analytics/dspg)<br/>
**License:** [MIT](https://opensource.org/licenses/MIT)<br/>

### Installation

You can install this package using the `devtools` package:

``` r
#install.packages("devtools")
#devtools::install_github("brandonleekramer/tidyorgs") 
```

The `tidyorgs` package provides several functions that help standardize
messy text data for organizational analysis. More specifically, the
package’s two core functions `detect_orgs()` and `email_to_orgs()`
standardize organizations from across the academic, business, government
and nonprofit sectors based on unstructured text and email domains. The
package is intended to support linkage across multiple datasets,
bibliometric analysis, and sector classification for social, economic,
and policy analysis.

### Matching organizations with the `detect_orgs()` function

``` r
library(tidyverse)
#library(tidyorgs)
devtools::load_all()
data(github_users)

users_to_orgs <- github_users %>%
  detect_orgs(login, company, organization, academic, email, 
              country = TRUE, parent_org = TRUE, org_type = TRUE) 

users_to_orgs %>% 
  filter(academic == 1) %>% 
  select(company, organization) 
```

    ## # A tibble: 19 × 2
    ##    company                                                  organization        
    ##    <chr>                                                    <chr>               
    ##  1 "Karlsruhe Institute of Technology"                      Karlsruher Institut…
    ##  2 "Harvard University"                                     Harvard University  
    ##  3 "Universit de Montral"                                   Université de Montr…
    ##  4 "University of Turku"                                    University of Turku 
    ##  5 "University of North Carolina at Chapel Hill"            University of North…
    ##  6 "University of Massachusetts Amherst"                    University of Massa…
    ##  7 "Universit Catholique de Louvain"                        Université Catholiq…
    ##  8 "Worcester State University Computer Science Department" Worcester State Uni…
    ##  9 "University of Michigan"                                 University of Michi…
    ## 10 "@JuliaComputing / @NYU-MSDSE-SWG "                      New York University 
    ## 11 "Namibia University of Science and Technology"           Misc. Academic      
    ## 12 "The University of Texas at Austin"                      The University of T…
    ## 13 "University of Jyvskyl"                                  Misc. Academic      
    ## 14 "Kyoto University"                                       Kyoto University    
    ## 15 "KTH Royal Institute of Technology"                      KTH Royal Institute…
    ## 16 "Simon Fraser University"                                Simon Fraser Univer…
    ## 17 "Microsoft. Formerly at UC Berkeley"                     University of Calif…
    ## 18 "University of California, Berkeley"                     University of Calif…
    ## 19 "National Technical University of Athens"                National Technical …

``` r
users_to_orgs %>% 
  filter(academic == 1) %>% 
  select(organization, country, org_type) 
```

    ## # A tibble: 19 × 3
    ##    organization                                country       org_type           
    ##    <chr>                                       <chr>         <chr>              
    ##  1 Karlsruher Institut für Technologie         Germany       <NA>               
    ##  2 Harvard University                          United States Private not-for-pr…
    ##  3 Université de Montréal                      Canada        <NA>               
    ##  4 University of Turku                         Finland       <NA>               
    ##  5 University of North Carolina at Chapel Hill United States Public             
    ##  6 University of Massachusetts-Amherst         United States Public             
    ##  7 Université Catholique de Louvain            Belgium       <NA>               
    ##  8 Worcester State University                  United States Public             
    ##  9 University of Michigan-Ann Arbor            United States Public             
    ## 10 New York University                         United States Private not-for-pr…
    ## 11 Misc. Academic                              <NA>          <NA>               
    ## 12 The University of Texas at Austin           United States Public             
    ## 13 Misc. Academic                              <NA>          <NA>               
    ## 14 Kyoto University                            Japan         <NA>               
    ## 15 KTH Royal Institute of Technology           Sweden        <NA>               
    ## 16 Simon Fraser University                     Canada        <NA>               
    ## 17 University of California-Berkeley           United States Public             
    ## 18 University of California-Berkeley           United States Public             
    ## 19 National Technical University of Athens     Greece        <NA>

### Matching users to organizations by emails alone using the `email_to_orgs()` function

``` r
user_emails_to_orgs <- github_users %>%
  email_to_orgs(login, email, country_name, academic) 

github_users %>% 
  left_join(user_emails_to_orgs, by = "login") %>% 
  drop_na(country_name) %>% 
  select(email, country_name)
```

    ##                      email                                country_name
    ## 1    username@umontreal.ca                      Université de Montréal
    ## 2          username@utu.fi                         University of Turku
    ## 3         username@unc.edu University of North Carolina at Chapel Hill
    ## 4       username@umich.edu            University of Michigan-Ann Arbor
    ## 5 username@mail.utexas.edu           The University of Texas at Austin
    ## 6          username@kth.se           KTH Royal Institute of Technology
    ## 7          username@sfu.ca                     Simon Fraser University
    ## 8    username@berkeley.edu           University of California-Berkeley
    ## 9 username@softlab.ntua.gr     National Technical University of Athens
