
# tidyorgs: A tidy package that standardizes text data for organizational and sector analysis <img src="man/figures/tidyorgs_logo.png" align="right" height="250" />

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
install.packages("devtools")
devtools::install_github("brandonleekramer/tidyorgs") 
```

The `tidyorgs` package provides several functions that help standardize
messy text data for organizational analysis. More specifically, the
package’s two core sets of functions `detect_{sector}()` and
`email_to_orgs()` standardize organizations from across the academic,
business, government and nonprofit sectors based on unstructured text
and email domains. The package is intended to support linkage across
multiple datasets, bibliometric analysis, and sector classification for
social, economic, and policy analysis.

### Matching organizations with the `detect_orgs()` function

The `detect_{sector}()` functions detects patterns in messy text data
and then standardizes them into organizations based on a curated
dictionary. For example, messy bio information scraped from GitHub can
be easily codified so that statistical analysis can be done on academic
users.

#### `detect_academic()`

``` r
library(tidyverse)
library(tidyorgs)
data(github_users)

classified_academic <- github_users %>%
  detect_academic(login, company, organization, email) %>% 
  filter(academic == 1) %>% 
  select(login, organization, company) 

classified_academic
```

    ## # A tibble: 19 × 3
    ##    login           organization                                company          
    ##    <chr>           <chr>                                       <chr>            
    ##  1 simongog        Karlsruher Institut für Technologie         "Karlsruhe Insti…
    ##  2 capooti         Harvard University                          "Harvard Univers…
    ##  3 tpoisot         Université de Montréal                      "Universit de Mo…
    ##  4 jbjorne         University of Turku                         "University of T…
    ##  5 rybesh          University of North Carolina at Chapel Hill "University of N…
    ##  6 brendano        University of Massachusetts-Amherst         "University of M…
    ##  7 swilmet         Université Catholique de Louvain            "Universit Catho…
    ##  8 kwurst          Worcester State University                  "Worcester State…
    ##  9 wellman         University of Michigan-Ann Arbor            "University of M…
    ## 10 StefanKarpinski New York University                         "@JuliaComputing…
    ## 11 mtjikuzu        Misc. Academic                              "Namibia Univers…
    ## 12 pgoetz          The University of Texas at Austin           "The University …
    ## 13 deggis          Misc. Academic                              "University of J…
    ## 14 eiennohito      Kyoto University                            "Kyoto Universit…
    ## 15 senorcarbone    KTH Royal Institute of Technology           "KTH Royal Insti…
    ## 16 rtv             Simon Fraser University                     "Simon Fraser Un…
    ## 17 faridani        University of California-Berkeley           "Microsoft. Form…
    ## 18 baugarten       University of California-Berkeley           "University of C…
    ## 19 nickie          National Technical University of Athens     "National Techni…

#### `detect_business()`

``` r
classified_businesses <- github_users %>%
  detect_business(login, company, organization, email) %>% 
  filter(business == 1) %>%
  select(login, organization, company)
classified_businesses
```

    ## # A tibble: 201 × 3
    ##    login         organization     company                                
    ##    <chr>         <chr>            <chr>                                  
    ##  1 mcollina      Nearform         "@nearform "                           
    ##  2 ephur         Rackspace        "Object Rocket at Rackspace"           
    ##  3 topaxi        Puzzle Itc       "@Puzzle ITC GmbH"                     
    ##  4 santegoeds    Misc. Business   "Quality Enterprise Consulting Limited"
    ##  5 briancavalier Yelp             "@yelp "                               
    ##  6 joshuamckenty Pivotal Software "Pivotal, Inc."                        
    ##  7 jckarter      Apple            "Apple"                                
    ##  8 sethvargo     Google           "@Google "                             
    ##  9 peterskeide   Misc. Business   "Skalar AS"                            
    ## 10 e2kaneko      Misc. Business   "E2info, Inc."                         
    ## # … with 191 more rows

#### `detect_government()`

``` r
classified_government <- github_users %>%
  detect_government(login, company, organization, email) %>% 
  filter(government == 1) %>% 
  select(login, organization, company)
classified_government
```

    ## # A tibble: 3 × 3
    ##   login         organization                                  company           
    ##   <chr>         <chr>                                         <chr>             
    ## 1 state-hiu-dev US Department of State                        U.S. Department o…
    ## 2 ogrisel       Institut National de la Recherche Agronomique Inria             
    ## 3 ondrae        US General Services Administration            Code For America

#### `detect_nonprofit()`

``` r
classified_nonprofit <- github_users %>%
  detect_nonprofit(login, company, organization, email) %>% 
  filter(nonprofit == 1) %>% 
  select(login, organization, company, email)
classified_nonprofit
```

    ## # A tibble: 5 × 4
    ##   login       organization               company                       email    
    ##   <chr>       <chr>                      <chr>                         <chr>    
    ## 1 kostajh     Wikimedia Foundation       @wikimedia                    username…
    ## 2 elad661     The Fedora Project         Red Hat                       username…
    ## 3 youngwookim Apache Software Foundation SK telecom                    username…
    ## 4 nyampire    Misc. Non-Profit           OpenSteetMap Foundation Japan username…
    ## 5 ondrae      Code for America           Code For America              username…

### Matching users to organizations by emails using `email_to_orgs()`

For those that only have email information, the `email_to_orgs()`
function matches users to organizations based on our curated domain
list.

``` r
user_emails_to_orgs <- github_users %>%
  email_to_orgs(login, email, country_name, "academic") 

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
