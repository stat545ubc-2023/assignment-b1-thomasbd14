Get Quartiles Function
================
Thomas Deckers
01/11/2023

``` r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(palmerpenguins))
suppressPackageStartupMessages(library(testthat))
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
#' to calculate the quartiles. Named because this variable will be summarized.
#' @param grouping_variable (optional) the name of a column in `data` with a 
#' categorical variable. If specified, the results will be summarized within 
#' each unique entry of `grouping_variable`. Named since this is the variable 
#' used for grouping the output.
#' 
#' @return a tibble of showing the lower quartile, mean, and upper
#' quartile of `summarized_variable`, ignoring NA values. If `grouping_variable`
#' is not specified, the table will have one row representing the entire data. If 
#' `grouping_variable` is specified, each row shows the quartile within one 
#' unique category, which will be represented with an additional column.

get_quartiles <- function(data, summarized_variable, grouping_variable = NULL){
   
  #get our column names as strings for error checking
  sum_var_str <- deparse(substitute(summarized_variable))
  grp_var_str <- deparse(substitute(grouping_variable))
  
  #Check if all necessary variables have been specified
  if(is.null(data)){
    stop("Please specify data")
  }
  if(is.null(data[[sum_var_str]])){
    stop("Please speficy a valid column name to summarize")
  }
  
  # check for non-numeric summarized_variable
  if(!is.numeric(data[[sum_var_str]])){
    stop("Summarized_variable must be a numeric column\n",
         sum_var_str, " is of type ", class(data[[sum_var_str]][1]))
  }
  
  # check if we are going to remove any NA values, warn if so
  if(anyNA(data[[sum_var_str]])){
    warning("removing NA values")
  }
  # check for NAs in the category variable. These will be treated as their own category
  # but it's nice to warn of this behavior
  if((!is.null(grp_var_str) && anyNA(data[[grp_var_str]]))){
    warning("NAs found in category variable. Treating these as a separate group")
  }
  
  
  
  #execute the function
  data %>% group_by({{grouping_variable}}) %>%
    summarize(lower_quartile = quantile({{summarized_variable}},0.25, na.rm = TRUE, names = FALSE), 
              median = median({{summarized_variable}}, na.rm = TRUE),
              upper_quartile = quantile({{summarized_variable}},0.75, , na.rm = TRUE, names = FALSE)
              )
}
```

# Demonstration

Let’s try this out with the `palmerpenguins` dataset. We’ll check out
the quartiles of the bill length, sorted by species

``` r
get_quartiles(penguins, bill_length_mm, species)
```

    ## Warning in get_quartiles(penguins, bill_length_mm, species): removing NA values

    ## # A tibble: 3 × 4
    ##   species   lower_quartile median upper_quartile
    ##   <fct>              <dbl>  <dbl>          <dbl>
    ## 1 Adelie              36.8   38.8           40.8
    ## 2 Chinstrap           46.3   49.6           51.1
    ## 3 Gentoo              45.3   47.3           49.6

Note that, as hoped, we get a warning that some NA variables are being
removed. Otherwise, we get a summary as expected.

And, because of our use of the default variable, we can also summarize
over the whole dataset without grouping.

``` r
get_quartiles(penguins, bill_length_mm)
```

    ## Warning in get_quartiles(penguins, bill_length_mm): removing NA values

    ## # A tibble: 1 × 3
    ##   lower_quartile median upper_quartile
    ##            <dbl>  <dbl>          <dbl>
    ## 1           39.2   44.4           48.5

Lets get a feel for some of our error conditions. We’ll try a
non-numeric `summarized_variable`

``` r
get_quartiles(penguins, island)
```

    ## Error in get_quartiles(penguins, island): Summarized_variable must be a numeric column
    ## island is of type factor

Or when we give a column that doesn’t exist at all

``` r
get_quartiles(penguins, country)
```

    ## Error in get_quartiles(penguins, country): Please speficy a valid column name to summarize

# Error Checking

Let’s do a more formal error check. I’ll make a few simple datasets with
some interesting cases

``` r
# Simple case with only one variable
no_category_test <- tibble(values = c(0,1,2,3,4))
no_category_expect <- tibble(lower_quartile = c(1),
                                median = c(2),
                                upper_quartile = c(3)
                                )


# Now let's add some categories
with_category_test <- tibble(categories = c("a","a","a","a","a","b","b","b","b","b"),
       values = c(1,1,1,1,1,2,3,4,5,6),
       decoy = c(1,2,3,"ab", "a",c(5),"a",8,10,9))
with_categories_expect <- tibble(categories = c("a","b"),
                                lower_quartile = c(1,3),
                                median = c(1,4),
                                upper_quartile = c(1,5)
                                )

# Try numerical categories
numerical_category_test <- tibble(categories = c(1,1,1,1,1,2,2,2,2,2),
       values = c(1,1,1,1,1,2,3,4,5,6))
numerical_category_expect <- tibble(categories = c(1,2),
                                lower_quartile = c(1,3),
                                median = c(1,4),
                                upper_quartile = c(1,5)
                                )

#Test each of these cases when used correctly 
test_that("Base cases",{
  expect_identical(get_quartiles(no_category_test, values), no_category_expect)
  expect_identical(get_quartiles(with_category_test, values, categories), with_categories_expect)
  expect_identical(get_quartiles(numerical_category_test, values, categories), numerical_category_expect)
})
```

    ## Test passed 🥇

We can reuse these datasets for some cases that should give an error

``` r
#Test that we get errors when we expect
test_that("Error cases",{
  #null data
  expect_error(get_quartiles(NULL, values), "Please specify data")
  #non numerical summarized_variable
  expect_error(get_quartiles(with_category_test,decoy),"Summarized_variable must be a numeric column
decoy is of type character")
  #non existent column
  expect_error(get_quartiles(with_category_test, non_existent_column), "Please speficy a valid column name to summarize")
})
```

    ## Test passed 🥳

Lastly, we’ll try out NA cases to see if we are issuing warnings when we
expect

``` r
#Insert NAs in summarized_variable
na_in_summarized <- tibble(categories = c("a","a","a","b","b","b"),
                           values = c(NA,2,NA,1,2,3),
                           )

#Insert NAs in  categories
na_in_categories <- tibble(categories = c("a","a","a",NA,"b","b","b"),
                           values = c(1,2,3,1000,1,2,3),
                           )

test_that("Warning cases",{
  expect_warning(get_quartiles(na_in_summarized,values,categories),"removing NA values")
  expect_warning(get_quartiles(na_in_categories,values, categories), "NAs found in category variable. Treating these as a separate group")
})
```

    ## Test passed 😀
