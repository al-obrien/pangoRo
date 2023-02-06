
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pangoRo

<!-- badges: start -->
<!-- badges: end -->

COVID-19 lineage names can be confusing to navigate; there are many
aliases and if you want to catch them all to examine further, it helps
to have some additional tools…

{pangoRo} is an R package to support interacting with [PANGO
lineage](https://cov-lineages.org/index.html) information. The core
functionality was inspired by a similar package called
[pango_aliaser](https://github.com/corneliusroemer/pango_aliasor)
created by Cornelius Roemer for the Python language.

## Installation

You can install {pangoRo} from GitHub:

``` r
remotes::install_github('al-obrien/pangoRo')
```

## Example

The basic usage of {pangoRo} is to expand, collapse, and sort COVID-19
lineages. Start by creating the *pangoro* object that links to the
latest (or cached) PANGO reference. This is then passed to subsequent
operations as reference.

``` r
library(pangoRo)

# Create pangoro object
my_pangoro <- pangoro()
#> Loading alias table from PANGO webiste...
```

``` r
# Vector of COVID-19 lineages to collapse
cov_lin <- c('B.1.617.2', 'BL.2', 'B.1.1.529.2.75.1.2', 'XD.1')

# Collapse lineage names as far as possible
collapse_pangoro(my_pangoro, cov_lin)
#> [1] "B.1.617.2" "BL.2"      "BL.2"      "XD.1"
```

``` r
# Vector of COVID-19 lineages to expand
cov_lin <- c('B.1.617.2', 'AY.4', 'AY.39', 'BL.2', 'XD.1')

# Expand lineage names as far as possible
exp_lin <- expand_pangoro(my_pangoro, cov_lin)
exp_lin
#>            B.1.617.2                 AY.4                AY.39 
#>          "B.1.617.2"        "B.1.617.2.4"       "B.1.617.2.39" 
#>                 BL.2                 XD.1 
#> "B.1.1.529.2.75.1.2"               "XD.1"
```

``` r
# Sort lineages
sort_pangoro(my_pangoro, exp_lin)
#>                 BL.2            B.1.617.2                 AY.4 
#> "B.1.1.529.2.75.1.2"          "B.1.617.2"        "B.1.617.2.4" 
#>                AY.39                 XD.1 
#>       "B.1.617.2.39"               "XD.1"
```
