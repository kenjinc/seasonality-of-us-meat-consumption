Data Cleaning and Analysis
================

## Required Packages

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.1      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

## Data Loading

``` r
read_csv("/Users/kenjinchang/github/seasonality-of-us-meat-consumption/data/st_proc_mn_wtfix_abb.csv.gz") 
```

    ## New names:
    ## Rows: 2796805 Columns: 11
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (3): state, concatenated, region dbl (7): ...1, yr, mn,
    ## conv_factor_to_units_kg, unit_price_kg, units_kg, re... date (1): date
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

    ## # A tibble: 2,796,805 × 11
    ##     ...1 date          yr    mn state   conv_fa…¹ unit_…² conca…³ units…⁴ region
    ##    <dbl> <date>     <dbl> <dbl> <chr>       <dbl>   <dbl> <chr>     <dbl> <chr> 
    ##  1     1 2017-01-28  2017     1 Alabama     0.340    5.65 PROCES…  15230. south 
    ##  2     2 2017-02-25  2017     2 Alabama     0.340    5.90 PROCES…  12711. south 
    ##  3     3 2017-03-25  2017     3 Alabama     0.340    5.83 PROCES…  13712. south 
    ##  4     4 2017-04-22  2017     4 Alabama     0.340    5.74 PROCES…  14436. south 
    ##  5     5 2017-05-20  2017     5 Alabama     0.340    5.63 PROCES…  14063. south 
    ##  6     6 2017-06-17  2017     6 Alabama     0.340    5.59 PROCES…  14673. south 
    ##  7     7 2017-07-15  2017     7 Alabama     0.340    5.30 PROCES…  18294. south 
    ##  8     8 2017-08-12  2017     8 Alabama     0.340    5.80 PROCES…  13823. south 
    ##  9     9 2017-09-09  2017     9 Alabama     0.340    5.77 PROCES…  15330. south 
    ## 10    10 2017-10-07  2017    10 Alabama     0.340    6.04 PROCES…  13152. south 
    ## # … with 2,796,795 more rows, 1 more variable: region_dec2019dollars <dbl>, and
    ## #   abbreviated variable names ¹​conv_factor_to_units_kg, ²​unit_price_kg,
    ## #   ³​concatenated, ⁴​units_kg
