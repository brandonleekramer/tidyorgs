
# tidyorgs: A tidy package that cleans messy text data for organizational and geographic analysis

**Authors:** [Brandon Kramer](https://www.brandonleekramer.com/) with
contributions from the
[2020](https://dspg-young-scholars-program.github.io/dspg20oss/team/?dspg)
and [2021](https://dspgtools.shinyapps.io/dspg21oss/) UVA Data Science
for the Public Good Teams<br/> **License:**
[MIT](https://opensource.org/licenses/MIT)<br/> **Sponsors:** [National
Center for Science and Engineering
Statistics](https://www.nsf.gov/statistics/)

### Installation

You can install this package using the `devtools` package:

``` r
install.packages("devtools")
devtools::install_github("brandonleekramer/tidyorgs")
```

The `tidyorgs` package provides several functions that help standardize
messy text data for organizational and geographic analysis. More
specifically, the package’s two core functions `detect_orgs()` and
`detect_countries()` standardize organizations in the academic,
business, government and nonprofit sectors as well as cities and
countries based on unstructured text and email domains. The package is
intended to support linkage across multiple datasets, bibliometric
analysis, and sector classification for social, economic, and policy
analysis.

### Matching organizations with the `detect_orgs()` function

``` r
users_to_orgs <- github_users %>%
  detect_orgs(login, company, organization, academic, email) 

users_to_orgs %>% 
  filter(academic == 1) %>% 
  select(organization, company) 
```

    ## # A tibble: 20 × 2
    ##    organization                             company                             
    ##    <chr>                                    <chr>                               
    ##  1 karlsruher institut für technologie      "Karlsruhe Institute of Technology" 
    ##  2 harvard university                       "Harvard University"                
    ##  3 université de montréal                   "Universit de Montral"              
    ##  4 university of turku                      "University of Turku"               
    ##  5 university of north carolina chapel hill "University of North Carolina at Ch…
    ##  6 university of massachusetts amherst      "University of Massachusetts Amhers…
    ##  7 université catholique de louvain         "Universit Catholique de Louvain"   
    ##  8 misc. academic                           "Worcester State University Compute…
    ##  9 university of michigan ann arbor         "University of Michigan"            
    ## 10 new york university                      "@JuliaComputing / @NYU-MSDSE-SWG " 
    ## 11 misc. academic                           "Namibia University of Science and …
    ## 12 university of texas austin               "The University of Texas at Austin" 
    ## 13 university of nebraska lincoln           "UNL"                               
    ## 14 misc. academic                           "University of Jyvskyl"             
    ## 15 kyoto university                         "Kyoto University"                  
    ## 16 kth royal institute of technology        "KTH Royal Institute of Technology" 
    ## 17 simon fraser university                  "Simon Fraser University"           
    ## 18 university of california berkeley        "Microsoft. Formerly at UC Berkeley"
    ## 19 university of california berkeley        "University of California, Berkeley"
    ## 20 national technical university of athens  "National Technical University of A…

### Standardizing countries with the `detect_countries()` function

``` r
users_to_countries <- github_users %>%
  detect_countries(login, location, country_name, email) 

users_to_countries  %>% 
  drop_na(country_name) %>% 
  select(country_name, location)
```

    ## # A tibble: 465 × 2
    ##    country_name   location                 
    ##    <chr>          <chr>                    
    ##  1 italy          In the clouds above Italy
    ##  2 united states  St. Louis, MO            
    ##  3 brazil         Porto Alegre, RS - Brazil
    ##  4 united states  San Antonio, TX          
    ##  5 poland         Poland, Wrocaw           
    ##  6 united kingdom Manchester, UK           
    ##  7 united states  Sunnyvale, CA            
    ##  8 germany        Karlsruhe                
    ##  9 italy          Vicenza, Italy           
    ## 10 france         Marseille, France        
    ## # … with 455 more rows
