
<!-- README.md is generated from README.Rmd. Please edit that file -->

# esajdoc [![Travis build status](https://travis-ci.org/abjur/esajdoc.svg?branch=master)](https://travis-ci.org/abjur/esajdoc)

The goal of esajdoc is to download esaj results by document number (CPF
or CNPJ).

## Installation

You can install the dev version of `esajdoc` here

``` r
devtools::install_github("abjur/esajdoc")
```

``` r
library(esajdoc)
```

## Example

``` r
cpopg_doc("96926147868")
fs::dir_ls("data-raw/cpopg/96926147868")
```

    #> data-raw/cpopg/96926147868/pag_01.html

``` r
files <- fs::dir_ls("data-raw/cpopg/96926147868")
result <- cpopg_doc_parse(files)
tidyr::unnest(result, output)
#> # A tibble: 9 x 5
#>   id                     return id1   key      value                       
#>   <chr>                  <chr>  <chr> <chr>    <chr>                       
#> 1 data-raw/cpopg/969261… result 1     n_proce… 1002542-95.2018.8.26.0081   
#> 2 data-raw/cpopg/969261… result 1     Reqte    José Francisco Figueiredo M…
#> 3 data-raw/cpopg/969261… result 1     Recebid… 14/09/2018 - Juizado Especi…
#> 4 data-raw/cpopg/969261… result 2     n_proce… 1002229-37.2018.8.26.0081   
#> 5 data-raw/cpopg/969261… result 2     Reqte    José Francisco Figueiredo M…
#> 6 data-raw/cpopg/969261… result 2     Recebid… 14/08/2018 - 2ª Vara        
#> 7 data-raw/cpopg/969261… result 3     n_proce… 1002777-33.2016.8.26.0081   
#> 8 data-raw/cpopg/969261… result 3     Reqte    José Francisco Figueiredo M…
#> 9 data-raw/cpopg/969261… result 3     Recebid… 22/09/2016 - 2ª Vara
```
