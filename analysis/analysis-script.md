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

``` r
library(scales)
```

    ## 
    ## Attaching package: 'scales'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

library(usmap) library(maps) library(socviz)

## Data Loading

### Retail Data

``` r
read_csv("/Users/kenjinchang/github/seasonality-of-us-meat-consumption/data/st_proc_mn_wtfix_abb.csv.gz") %>%
  select(-...1) %>%
  head(5)
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

    ## # A tibble: 5 × 10
    ##   date          yr    mn state   conv_f…¹ unit_…² conca…³ units…⁴ region regio…⁵
    ##   <date>     <dbl> <dbl> <chr>      <dbl>   <dbl> <chr>     <dbl> <chr>    <dbl>
    ## 1 2017-01-28  2017     1 Alabama    0.340    5.65 PROCES…  15230. south     1.05
    ## 2 2017-02-25  2017     2 Alabama    0.340    5.90 PROCES…  12711. south     1.05
    ## 3 2017-03-25  2017     3 Alabama    0.340    5.83 PROCES…  13712. south     1.05
    ## 4 2017-04-22  2017     4 Alabama    0.340    5.74 PROCES…  14436. south     1.05
    ## 5 2017-05-20  2017     5 Alabama    0.340    5.63 PROCES…  14063. south     1.04
    ## # … with abbreviated variable names ¹​conv_factor_to_units_kg, ²​unit_price_kg,
    ## #   ³​concatenated, ⁴​units_kg, ⁵​region_dec2019dollars

### Agricultural Subsidy Data

``` r
read_csv("/Users/kenjinchang/github/seasonality-of-us-meat-consumption/data/county_data.csv") %>%
  head(5)
```

    ## Rows: 3055 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): county, state_abb, state
    ## dbl (2): total_subsidies_1995_2021, perc_state_total
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## # A tibble: 5 × 5
    ##   county                total_subsidies_1995_2021 perc_state_total state…¹ state
    ##   <chr>                                     <dbl>            <dbl> <chr>   <chr>
    ## 1 Gaines County, Texas                 1476802523            0.033 TX      Texas
    ## 2 Hale County, Texas                   1159637316            0.026 TX      Texas
    ## 3 Dawson County, Texas                 1110737641            0.025 TX      Texas
    ## 4 Terry County, Texas                   987748081            0.022 TX      Texas
    ## 5 Wharton County, Texas                 954867910            0.021 TX      Texas
    ## # … with abbreviated variable name ¹​state_abb

County Shapefile Data

``` r
map_data("county") %>%
  head(6)
```

    ##        long      lat group order  region subregion
    ## 1 -86.50517 32.34920     1     1 alabama   autauga
    ## 2 -86.53382 32.35493     1     2 alabama   autauga
    ## 3 -86.54527 32.36639     1     3 alabama   autauga
    ## 4 -86.55673 32.37785     1     4 alabama   autauga
    ## 5 -86.57966 32.38357     1     5 alabama   autauga
    ## 6 -86.59111 32.37785     1     6 alabama   autauga

## Total Volume (kg) of Processed Meat Sales by State

``` r
read_csv("/Users/kenjinchang/github/seasonality-of-us-meat-consumption/data/st_proc_mn_wtfix_abb.csv.gz") %>%
  select(-...1) %>%
  group_by(state) %>%
  summarize(total_kg=sum(units_kg,na.rm=TRUE)) %>% 
  ggplot(aes(x=total_kg,y=reorder(state,total_kg))) + 
  geom_col(fill="firebrick") + 
  xlab("") + 
  ylab("") +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),panel.grid.minor=element_blank())
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

![](analysis-script_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Mean Volume (kg) of Processed Meat Sales by State

``` r
read_csv("/Users/kenjinchang/github/seasonality-of-us-meat-consumption/data/st_proc_mn_wtfix_abb.csv.gz") %>%
  select(-...1) %>%
  group_by(state) %>%
  summarize(mean_kg=mean(units_kg,na.rm=TRUE)) %>% 
  ggplot(aes(x=mean_kg,y=reorder(state,mean_kg))) + 
  geom_col(fill="firebrick") + 
  xlab("") + 
  ylab("") +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),panel.grid.minor=element_blank())
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

![](analysis-script_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Total Volume (kg) of Processed Meat Sales by Month

``` r
read_csv("/Users/kenjinchang/github/seasonality-of-us-meat-consumption/data/st_proc_mn_wtfix_abb.csv.gz") %>%
  select(-...1) %>%
  group_by(mn) %>%
  summarize(total_kg=sum(units_kg,na.rm=TRUE)) %>% 
  ggplot(aes(x=mn,y=total_kg)) + 
  geom_col(fill="firebrick") + 
  scale_x_continuous(breaks=c(1:12)) +
  xlab("") + 
  ylab("") +
  theme(panel.grid.minor=element_blank())
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

![](analysis-script_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### Mean Sales by Month

``` r
read_csv("/Users/kenjinchang/github/seasonality-of-us-meat-consumption/data/st_proc_mn_wtfix_abb.csv.gz") %>%
  select(-...1) %>%
  group_by(mn) %>%
  summarize(mean_kg=mean(units_kg,na.rm=TRUE)) %>% 
  ggplot(aes(x=mn,y=mean_kg)) + 
  geom_col(fill="firebrick") + 
  scale_x_continuous(breaks=c(1:12)) +
  xlab("") + 
  ylab("") +
  theme(panel.grid.minor=element_blank())
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

![](analysis-script_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## State-Level Agricultural Subsidy Payments

``` r
us_counties <- map_data("county")
```

``` r
county_data <- read_csv("/Users/kenjinchang/github/seasonality-of-us-meat-consumption/data/county_data.csv") %>%
  mutate(county_no_state=gsub("(.*),.*","\\1",county)) %>%
  mutate(county_abb=str_remove_all(county_no_state, " County| Parish")) %>%
  mutate(state=tolower(state),county_abb=tolower(county_abb)) %>%
  unite(id,c("state","county_abb"),sep=", ",remove=FALSE) %>%
  select(id,state,county_abb,total_subsidies_1995_2021,perc_state_total) %>%
  mutate(perc_natl_total=perc_state_total/sum(perc_state_total)) %>%
  mutate(total_subsidies_1995_2021_bil=total_subsidies_1995_2021/1000000000)
```

    ## Rows: 3055 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): county, state_abb, state
    ## dbl (2): total_subsidies_1995_2021, perc_state_total
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
county_data %>%
  head(6)
```

    ## # A tibble: 6 × 7
    ##   id             state county_abb total_subsidies_1995…¹ perc_…² perc_…³ total…⁴
    ##   <chr>          <chr> <chr>                       <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 texas, gaines  texas gaines                 1476802523   0.033 6.96e-4   1.48 
    ## 2 texas, hale    texas hale                   1159637316   0.026 5.49e-4   1.16 
    ## 3 texas, dawson  texas dawson                 1110737641   0.025 5.28e-4   1.11 
    ## 4 texas, terry   texas terry                   987748081   0.022 4.64e-4   0.988
    ## 5 texas, wharton texas wharton                 954867910   0.021 4.43e-4   0.955
    ## 6 texas, lamb    texas lamb                    928633397   0.021 4.43e-4   0.929
    ## # … with abbreviated variable names ¹​total_subsidies_1995_2021,
    ## #   ²​perc_state_total, ³​perc_natl_total, ⁴​total_subsidies_1995_2021_bil

``` r
county_map <- map_data("county") %>%
  unite(id,c("region","subregion"),sep=", ",remove=FALSE)
county_map %>%
  head(6)
```

    ##        long      lat group order               id  region subregion
    ## 1 -86.50517 32.34920     1     1 alabama, autauga alabama   autauga
    ## 2 -86.53382 32.35493     1     2 alabama, autauga alabama   autauga
    ## 3 -86.54527 32.36639     1     3 alabama, autauga alabama   autauga
    ## 4 -86.55673 32.37785     1     4 alabama, autauga alabama   autauga
    ## 5 -86.57966 32.38357     1     5 alabama, autauga alabama   autauga
    ## 6 -86.59111 32.37785     1     6 alabama, autauga alabama   autauga

``` r
county_full <- left_join(county_map,county_data,by="id")
county_full %>%
  head(6)
```

    ##        long      lat group order               id  region subregion   state
    ## 1 -86.50517 32.34920     1     1 alabama, autauga alabama   autauga alabama
    ## 2 -86.53382 32.35493     1     2 alabama, autauga alabama   autauga alabama
    ## 3 -86.54527 32.36639     1     3 alabama, autauga alabama   autauga alabama
    ## 4 -86.55673 32.37785     1     4 alabama, autauga alabama   autauga alabama
    ## 5 -86.57966 32.38357     1     5 alabama, autauga alabama   autauga alabama
    ## 6 -86.59111 32.37785     1     6 alabama, autauga alabama   autauga alabama
    ##   county_abb total_subsidies_1995_2021 perc_state_total perc_natl_total
    ## 1    autauga                  61027557            0.012    0.0002532554
    ## 2    autauga                  61027557            0.012    0.0002532554
    ## 3    autauga                  61027557            0.012    0.0002532554
    ## 4    autauga                  61027557            0.012    0.0002532554
    ## 5    autauga                  61027557            0.012    0.0002532554
    ## 6    autauga                  61027557            0.012    0.0002532554
    ##   total_subsidies_1995_2021_bil
    ## 1                    0.06102756
    ## 2                    0.06102756
    ## 3                    0.06102756
    ## 4                    0.06102756
    ## 5                    0.06102756
    ## 6                    0.06102756

### Total Subsidies Given across Counties, 1995-2021

``` r
ggplot(county_full,aes(x=long,y=lat,fill=total_subsidies_1995_2021_bil,group=group)) + 
  geom_polygon(color="white",linewidth=0.05) +
  coord_map(projection="albers",lat0=39,lat1=45) + 
  scale_fill_distiller(palette="Greens",trans="reverse",na.value="white") +
  labs(fill="Total Agricultural Subsidies Given, 1995-2021") +
  xlab("") + 
  ylab("") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### Percent of All Agricultural Subsidies Given across Counties, 1995-2021

``` r
ggplot(county_full,aes(x=long,y=lat,fill=perc_natl_total,group=group)) + 
  geom_polygon(color="white",linewidth=0.05) +
  coord_map(projection="albers",lat0=39,lat1=45) + 
  scale_fill_distiller(palette="Greens",trans="reverse",na.value="white",labels=percent) +
  labs(fill="Proportion of Agricultural Subsidies Given, 1995-2021") +
  xlab("") + 
  ylab("") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### Mean subsidies given by state

``` r
county_data %>%
  group_by(state) %>%
  summarize(mean_subsidies_1995_2021=mean(total_subsidies_1995_2021)) %>%
  ggplot(aes(x=mean_subsidies_1995_2021,y=reorder(state,mean_subsidies_1995_2021))) + 
  geom_col(fill="seagreen")
```

![](analysis-script_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Total subsidies given by state

``` r
county_data %>%
  group_by(state) %>%
  summarize(state_subsidies_1995_2021=sum(total_subsidies_1995_2021)) %>%
  ggplot(aes(x=state_subsidies_1995_2021,y=reorder(state,state_subsidies_1995_2021))) + 
  geom_col(fill="seagreen")
```

![](analysis-script_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## State-Level Processed Meat Sales (Per Capita)

### Mean Annual Sales Volume (kg) by State

``` r
read_csv("/Users/kenjinchang/github/seasonality-of-us-meat-consumption/data/st_proc_mn_wtfix_abb.csv.gz") %>%
  select(-...1) %>%
  group_by(state) %>%
  summarize(mean_annual_kg=sum(units_kg,na.rm=TRUE)/3) %>%
  ggplot(aes(x=reorder(state,mean_annual_kg),y=mean_annual_kg)) + 
  geom_col(fill="firebrick") + 
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1)) 
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

![](analysis-script_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

### Total Sales Volume (kg) by State by Year

``` r
read_csv("/Users/kenjinchang/github/seasonality-of-us-meat-consumption/data/st_proc_mn_wtfix_abb.csv.gz") %>%
  select(-...1) %>%
  group_by(state,yr) %>%
  summarize(total_kg=sum(units_kg,na.rm=TRUE)) %>%
  mutate(yr=as.character(yr)) %>%
  ggplot(aes(x=total_kg,y=reorder(state,total_kg),fill=yr)) + 
  geom_col(position="dodge") + 
  scale_fill_brewer(palette="Reds")
```

    ## New names:
    ## Rows: 2796805 Columns: 11
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (3): state, concatenated, region dbl (7): ...1, yr, mn,
    ## conv_factor_to_units_kg, unit_price_kg, units_kg, re... date (1): date
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## `summarise()` has grouped output by 'state'. You can override using the
    ## `.groups` argument.
    ## • `` -> `...1`

![](analysis-script_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

### Total Sales Volume (kg) by Year

``` r
read_csv("/Users/kenjinchang/github/seasonality-of-us-meat-consumption/data/st_proc_mn_wtfix_abb.csv.gz") %>%
  select(-...1) %>%
  group_by(yr) %>%
  summarize(total_kg=sum(units_kg,na.rm=TRUE)) %>%
  ggplot(aes(x=yr,y=total_kg)) + 
  geom_col(fill="firebrick")
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

![](analysis-script_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

### Mean Per Capita Sales Volume (kg) by State

``` r
state_sales_data <- read_csv("/Users/kenjinchang/github/seasonality-of-us-meat-consumption/data/st_proc_mn_wtfix_abb.csv.gz") %>%
  select(-...1) %>%
  group_by(state) %>%
  summarize(mean_annual_kg=sum(units_kg,na.rm=TRUE)/3) 
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

``` r
state_sales_data %>%
  head(6)
```

    ## # A tibble: 6 × 2
    ##   state       mean_annual_kg
    ##   <chr>                <dbl>
    ## 1 Alabama          34868520.
    ## 2 Arizona          37375950.
    ## 3 California      129162414.
    ## 4 Colorado         31450775.
    ## 5 Connecticut      14165642.
    ## 6 Florida         108567214.

Prep for left_join with state-level population data

Population data taken from
[here](https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html),
specifically the annual estimates of the resident population for the US

``` r
state_pop_data <- read_csv("/Users/kenjinchang/github/seasonality-of-us-meat-consumption/data/state_pop_data.csv") %>%
  rowwise() %>%
  mutate(mean_pop=mean(c(pop2017,pop2018,pop2019))) %>%
  select(state,mean_pop)
```

    ## Rows: 31 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): state
    ## dbl (3): pop2017, pop2018, pop2019
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
state_pop_data %>%
  head(6)
```

    ## # A tibble: 6 × 2
    ## # Rowwise: 
    ##   state        mean_pop
    ##   <chr>           <dbl>
    ## 1 Alabama      4888451.
    ## 2 Arizona      7160250.
    ## 3 California  39444103.
    ## 4 Colorado     5687303.
    ## 5 Connecticut  3570035.
    ## 6 Florida     21228556.

``` r
state_data <- left_join(state_sales_data,state_pop_data,by="state") %>%
  rowwise() %>%
  mutate(pc_annual_kg=mean_annual_kg/mean_pop) %>%
  mutate(state=tolower(state))
state_data %>%
  head(6)
```

    ## # A tibble: 6 × 4
    ## # Rowwise: 
    ##   state       mean_annual_kg  mean_pop pc_annual_kg
    ##   <chr>                <dbl>     <dbl>        <dbl>
    ## 1 alabama          34868520.  4888451.         7.13
    ## 2 arizona          37375950.  7160250.         5.22
    ## 3 california      129162414. 39444103.         3.27
    ## 4 colorado         31450775.  5687303.         5.53
    ## 5 connecticut      14165642.  3570035.         3.97
    ## 6 florida         108567214. 21228556.         5.11

``` r
ggplot(state_data,aes(y=reorder(state,pc_annual_kg),x=pc_annual_kg)) + 
  geom_point(color="firebrick")
```

![](analysis-script_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
state_map <- map_data("state") %>%
  rename(state=region)
state_map %>%
  head(6)
```

    ##        long      lat group order   state subregion
    ## 1 -87.46201 30.38968     1     1 alabama      <NA>
    ## 2 -87.48493 30.37249     1     2 alabama      <NA>
    ## 3 -87.52503 30.37249     1     3 alabama      <NA>
    ## 4 -87.53076 30.33239     1     4 alabama      <NA>
    ## 5 -87.57087 30.32665     1     5 alabama      <NA>
    ## 6 -87.58806 30.32665     1     6 alabama      <NA>

``` r
state_full <- left_join(state_data,state_map,by="state")
state_full %>%
  head(6)
```

    ## # A tibble: 6 × 9
    ## # Rowwise: 
    ##   state   mean_annual_kg mean_pop pc_annual_kg  long   lat group order subregion
    ##   <chr>            <dbl>    <dbl>        <dbl> <dbl> <dbl> <dbl> <int> <chr>    
    ## 1 alabama      34868520. 4888451.         7.13 -87.5  30.4     1     1 <NA>     
    ## 2 alabama      34868520. 4888451.         7.13 -87.5  30.4     1     2 <NA>     
    ## 3 alabama      34868520. 4888451.         7.13 -87.5  30.4     1     3 <NA>     
    ## 4 alabama      34868520. 4888451.         7.13 -87.5  30.3     1     4 <NA>     
    ## 5 alabama      34868520. 4888451.         7.13 -87.6  30.3     1     5 <NA>     
    ## 6 alabama      34868520. 4888451.         7.13 -87.6  30.3     1     6 <NA>

``` r
ggplot(state_full,aes(x=long,y=lat,fill=pc_annual_kg,group=group)) + 
  geom_polygon(color="white",linewidth=0.05) +
  coord_map(projection="albers",lat0=39,lat1=45) + 
  scale_fill_distiller(palette="YlOrRd",trans="reverse",na.value="white") + 
  labs(fill="Mean Annual Per Capita Processed Meat Sales (kg), 2017-2019") +
  xlab("") + 
  ylab("") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

Next steps: need to add county health and county water data. Now looking
to pivot so that one project focuses on how county health aligns with
state retail patterns of red and processed meat and the other looks at
how agricultural subsidy payments align with water usage for irrigation
(and water scarcity), both at the county level
