tdeckers_assignement_b1
================
Thomas Deckers
01/11/2023

``` r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(palmerpenguins))
```

# Function Definition

One helpful function, and similar to something I found myself doing in
part A of the course, is collecting the four quartiles of a numerical
variable. I’ll allow the ability to use `group_by` as well. I’ll write
up and document this function.

``` r
#' @Title Get Quartiles
#' 
#' This function returns a table of the quartiles for a given numerical variable
#' in a tibble, optionally divided into categories based on an additional column.
#' 
#' @param data the input data table to be analyzed. Named because data is the 
#' typical thing to analyze in this fashion (and for consistency with dplyr)
#' @param summarized_variable the name of a numerical column in `data` for which
#' to calculate the quartiles.Named because this variable will be summarized.
#' @param grouping_variable (optional) the name of a column in `data` with a 
#' categorical variable. If specified, the results will be summarized within 
#' each unique entry of `grouping_variable`. Named since this is the variable 
#' used for grouping the output.
#' 
#' @return a tibble of 3 columns showing the lower quartile, mean, and upper
#' quartile of `summarized_variable`. If `grouping_variable` is not specified, 
#' the table will have one row representing the entire data. If 
#' `grouping_variable` is specified, each row shows the quartile within one 
#' unique category, as specified.

get_quartiles <- function(data, summarized_variable, grouping_variable = NULL){
  data %>% group_by({{grouping_variable}}) %>%
    summarize(lower_quantile = quantile({{summarized_variable}},0.25, na.rm = TRUE), 
              mean = mean({{summarized_variable}}, na.rm = TRUE),
              upper_quantile = quantile({{summarized_variable}},0.75, , na.rm = TRUE)
              )
}
```

# Demonstration

Let’s try this out with the `palmerpenguins` dataset. We’ll check out
the quartiles of the bill length, sorted by species

``` r
get_quartiles(penguins, bill_length_mm, species)
```

    ## # A tibble: 3 × 4
    ##   species   lower_quantile  mean upper_quantile
    ##   <fct>              <dbl> <dbl>          <dbl>
    ## 1 Adelie              36.8  38.8           40.8
    ## 2 Chinstrap           46.3  48.8           51.1
    ## 3 Gentoo              45.3  47.5           49.6

And, because of our use of the default variable, we can also summarize
over the whole dataset without grouping.

``` r
get_quartiles(penguins, bill_length_mm)
```

    ## # A tibble: 1 × 3
    ##   lower_quantile  mean upper_quantile
    ##            <dbl> <dbl>          <dbl>
    ## 1           39.2  43.9           48.5
