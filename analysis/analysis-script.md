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

``` r
library(RColorBrewer)
```

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
us_counties %>%
  head(6)
```

    ##        long      lat group order  region subregion
    ## 1 -86.50517 32.34920     1     1 alabama   autauga
    ## 2 -86.53382 32.35493     1     2 alabama   autauga
    ## 3 -86.54527 32.36639     1     3 alabama   autauga
    ## 4 -86.55673 32.37785     1     4 alabama   autauga
    ## 5 -86.57966 32.38357     1     5 alabama   autauga
    ## 6 -86.59111 32.37785     1     6 alabama   autauga

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

### Importing county-level water usage data

``` r
county_water_data <- read_csv("/Users/kenjinchang/github/seasonality-of-us-meat-consumption/data/county_water_data.csv",skip=1) %>%
  select(STATE,COUNTY,FIPS,'IR-IrTot','IR-WGWFr','IR-WFrTo') %>%
  rename(state=STATE,county=COUNTY,fips=FIPS,acres='IR-IrTot',groundwater='IR-WGWFr',freshwater='IR-WFrTo') %>%
  mutate(county=str_remove_all(county," County| Parish")) %>%
  mutate(county=tolower(county)) %>%
  mutate(across(state,str_replace,'AL','alabama')) %>%
  mutate(across(state,str_replace,'AK','alaska')) %>%
  mutate(across(state,str_replace,'AZ','arizona')) %>%
  mutate(across(state,str_replace,'AR','arkansas')) %>%
  mutate(across(state,str_replace,'CA','california')) %>%
  mutate(across(state,str_replace,'CO','colorado')) %>%
  mutate(across(state,str_replace,'CT','connecticut')) %>%
  mutate(across(state,str_replace,'DE','delaware')) %>%
  mutate(across(state,str_replace,'FL','florida')) %>%
  mutate(across(state,str_replace,'GA','georgia')) %>%
  mutate(across(state,str_replace,'HI','hawaii')) %>%
  mutate(across(state,str_replace,'ID','idaho')) %>%
  mutate(across(state,str_replace,'IL','illinois')) %>%
  mutate(across(state,str_replace,'IN','indiana')) %>%
  mutate(across(state,str_replace,'IA','iowa')) %>%
  mutate(across(state,str_replace,'KS','kansas')) %>%
  mutate(across(state,str_replace,'KY','kentucky')) %>%
  mutate(across(state,str_replace,'LA','louisiana')) %>%
  mutate(across(state,str_replace,'ME','maine')) %>%
  mutate(across(state,str_replace,'MD','maryland')) %>%
  mutate(across(state,str_replace,'MA','massachusetts')) %>%
  mutate(across(state,str_replace,'MI','michigan')) %>%
  mutate(across(state,str_replace,'MN','minnesota')) %>%
  mutate(across(state,str_replace,'MS','mississippi')) %>%
  mutate(across(state,str_replace,'MO','missouri')) %>%
  mutate(across(state,str_replace,'MT','montana')) %>%
  mutate(across(state,str_replace,'NE','nebraska')) %>%
  mutate(across(state,str_replace,'NV','nevada')) %>%
  mutate(across(state,str_replace,'NH','new hampshire')) %>%
  mutate(across(state,str_replace,'NJ','new jersey')) %>%
  mutate(across(state,str_replace,'NM','new mexico')) %>%
  mutate(across(state,str_replace,'NY','new york')) %>%
  mutate(across(state,str_replace,'NC','north carolina')) %>%
  mutate(across(state,str_replace,'ND','north dakota')) %>%
  mutate(across(state,str_replace,'OH','ohio')) %>%
  mutate(across(state,str_replace,'OK','oklahoma')) %>%
  mutate(across(state,str_replace,'OR','oregon')) %>%
  mutate(across(state,str_replace,'PA','pennsylvania')) %>%
  mutate(across(state,str_replace,'RI','rhode island')) %>%
  mutate(across(state,str_replace,'SC','south carolina')) %>%
  mutate(across(state,str_replace,'SD','south dakota')) %>%
  mutate(across(state,str_replace,'TN','tennessee')) %>%
  mutate(across(state,str_replace,'TX','texas')) %>%
  mutate(across(state,str_replace,'UT','utah')) %>%
  mutate(across(state,str_replace,'VT','vermont')) %>%
  mutate(across(state,str_replace,'VA','virginia')) %>%
  mutate(across(state,str_replace,'WA','washington')) %>%
  mutate(across(state,str_replace,'WV','west virginia')) %>%
  mutate(across(state,str_replace,'WI','wisconsin')) %>%
  mutate(across(state,str_replace,'WY','wyoming')) %>%
  unite(id,c("state","county"),sep=", ",remove=FALSE)
```

    ## Rows: 3223 Columns: 141
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (37): STATE, STATEFIPS, COUNTY, COUNTYFIPS, FIPS, PS-GWPop, PS-SWPop, D...
    ## dbl (104): YEAR, TP-TotPop, PS-TOPop, PS-WGWFr, PS-WGWSa, PS-WGWTo, PS-WSWFr...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
county_water_data %>%
  head(6)
```

    ## # A tibble: 6 × 7
    ##   id               state   county  fips  acres groundwater freshwater
    ##   <chr>            <chr>   <chr>   <chr> <dbl>       <dbl>      <dbl>
    ## 1 alabama, autauga alabama autauga 01001  2.26        3.36       3.52
    ## 2 alabama, baldwin alabama baldwin 01003 20.7        47.2       58.3 
    ## 3 alabama, barbour alabama barbour 01005  4.53        0.57       2.96
    ## 4 alabama, bibb    alabama bibb    01007  0.16        0.03       0.19
    ## 5 alabama, blount  alabama blount  01009  0.94        0.2        1.08
    ## 6 alabama, bullock alabama bullock 01011  1.67        0.92       2

``` r
county_water_full <- left_join(county_map,county_water_data,by="id")
county_water_full %>%
  head(6)
```

    ##        long      lat group order               id  region subregion   state
    ## 1 -86.50517 32.34920     1     1 alabama, autauga alabama   autauga alabama
    ## 2 -86.53382 32.35493     1     2 alabama, autauga alabama   autauga alabama
    ## 3 -86.54527 32.36639     1     3 alabama, autauga alabama   autauga alabama
    ## 4 -86.55673 32.37785     1     4 alabama, autauga alabama   autauga alabama
    ## 5 -86.57966 32.38357     1     5 alabama, autauga alabama   autauga alabama
    ## 6 -86.59111 32.37785     1     6 alabama, autauga alabama   autauga alabama
    ##    county  fips acres groundwater freshwater
    ## 1 autauga 01001  2.26        3.36       3.52
    ## 2 autauga 01001  2.26        3.36       3.52
    ## 3 autauga 01001  2.26        3.36       3.52
    ## 4 autauga 01001  2.26        3.36       3.52
    ## 5 autauga 01001  2.26        3.36       3.52
    ## 6 autauga 01001  2.26        3.36       3.52

``` r
ggplot(county_water_full,aes(x=long,y=lat,fill=freshwater,group=group)) + 
  geom_polygon(color="white",linewidth=0.05) +
  coord_map(projection="albers",lat0=39,lat1=45) + 
  scale_fill_distiller(palette="Blues",trans="reverse",na.value="white") +
  labs(fill="Total Freshwater Withdrawals (Mgal/d), 2015") +
  xlab("") + 
  ylab("") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
ggplot(county_water_full,aes(x=long,y=lat,fill=groundwater,group=group)) + 
  geom_polygon(color="white",linewidth=0.05) +
  coord_map(projection="albers",lat0=39,lat1=45) + 
  scale_fill_distiller(palette="Blues",trans="reverse",na.value="white") +
  labs(fill="Total Groundwater Withdrawals (Mgal/d), 2015") +
  xlab("") + 
  ylab("") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

``` r
ggplot(county_water_full,aes(x=long,y=lat,fill=acres,group=group)) + 
  geom_polygon(color="white",linewidth=0.05) +
  coord_map(projection="albers",lat0=39,lat1=45) + 
  scale_fill_distiller(palette="Blues",trans="reverse",na.value="white") +
  labs(fill="Total Acres Irrigated, 2015") +
  xlab("") + 
  ylab("") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

prepping health data for spatial join

``` r
county_health_data <- read_csv("/Users/kenjinchang/github/seasonality-of-us-meat-consumption/data/county_health_data.csv") %>%
  mutate(state=tolower(state),county=tolower(county)) %>%
  unite(id,c("state","county"),sep=", ",remove=FALSE) 
```

    ## Rows: 3193 Columns: 34
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): fips, state, county
    ## dbl (31): perc.obese, perc.obese.95ci.low, perc.obese.95ci.high, perc.obese....
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
county_health_data %>%
  head(6)
```

    ## # A tibble: 6 × 35
    ##   fips  id          state county perc.…¹ perc.…² perc.…³ perc.…⁴ prev.…⁵ prev.…⁶
    ##   <chr> <chr>       <chr> <chr>    <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 01000 alabama, NA alab… <NA>        36      NA      NA      NA    5466      NA
    ## 2 01001 alabama, a… alab… autau…      33      27      39       1    6650       4
    ## 3 01003 alabama, b… alab… baldw…      30      27      33       1    3471       1
    ## 4 01005 alabama, b… alab… barbo…      41      34      49       3    5314       2
    ## 5 01007 alabama, b… alab… bibb        37      30      46       3    6690       4
    ## 6 01009 alabama, b… alab… blount      33      27      39       1    4440       1
    ## # … with 25 more variables: county.deaths <dbl>, yppl.rate <dbl>,
    ## #   yppl.rate.95ci.low <dbl>, yppl.rate.95ci.high <dbl>, yppl.rate.quart <dbl>,
    ## #   perc.smoke <dbl>, perc.smoke.95ci.low <dbl>, perc.smoke.95ci.high <dbl>,
    ## #   perc.smoke.quart <dbl>, perc.inactive <dbl>, perc.inactive.95ci.low <dbl>,
    ## #   perc.inactive.95ci.high <dbl>, perc.inactive.quart <dbl>,
    ## #   count.highschl <dbl>, county.pop <dbl>, perc.highschl <dbl>,
    ## #   perc.highscl.95ci.low <dbl>, perc.highscl.95ci.high <dbl>, …

``` r
county_health_full <- left_join(county_map,county_health_data,by="id")
county_health_full %>%
  head(6)
```

    ##        long      lat group order               id  region subregion  fips
    ## 1 -86.50517 32.34920     1     1 alabama, autauga alabama   autauga 01001
    ## 2 -86.53382 32.35493     1     2 alabama, autauga alabama   autauga 01001
    ## 3 -86.54527 32.36639     1     3 alabama, autauga alabama   autauga 01001
    ## 4 -86.55673 32.37785     1     4 alabama, autauga alabama   autauga 01001
    ## 5 -86.57966 32.38357     1     5 alabama, autauga alabama   autauga 01001
    ## 6 -86.59111 32.37785     1     6 alabama, autauga alabama   autauga 01001
    ##     state  county perc.obese perc.obese.95ci.low perc.obese.95ci.high
    ## 1 alabama autauga         33                  27                   39
    ## 2 alabama autauga         33                  27                   39
    ## 3 alabama autauga         33                  27                   39
    ## 4 alabama autauga         33                  27                   39
    ## 5 alabama autauga         33                  27                   39
    ## 6 alabama autauga         33                  27                   39
    ##   perc.obese.quart prev.hosp.rate prev.hosp.rate.quart county.deaths yppl.rate
    ## 1                1           6650                    4           787      7830
    ## 2                1           6650                    4           787      7830
    ## 3                1           6650                    4           787      7830
    ## 4                1           6650                    4           787      7830
    ## 5                1           6650                    4           787      7830
    ## 6                1           6650                    4           787      7830
    ##   yppl.rate.95ci.low yppl.rate.95ci.high yppl.rate.quart perc.smoke
    ## 1               6998                8662               1         20
    ## 2               6998                8662               1         20
    ## 3               6998                8662               1         20
    ## 4               6998                8662               1         20
    ## 5               6998                8662               1         20
    ## 6               6998                8662               1         20
    ##   perc.smoke.95ci.low perc.smoke.95ci.high perc.smoke.quart perc.inactive
    ## 1                  17                   23                1            31
    ## 2                  17                   23                1            31
    ## 3                  17                   23                1            31
    ## 4                  17                   23                1            31
    ## 5                  17                   23                1            31
    ## 6                  17                   23                1            31
    ##   perc.inactive.95ci.low perc.inactive.95ci.high perc.inactive.quart
    ## 1                     25                      37                   2
    ## 2                     25                      37                   2
    ## 3                     25                      37                   2
    ## 4                     25                      37                   2
    ## 5                     25                      37                   2
    ## 6                     25                      37                   2
    ##   count.highschl county.pop perc.highschl perc.highscl.95ci.low
    ## 1          33076      37367            89                    87
    ## 2          33076      37367            89                    87
    ## 3          33076      37367            89                    87
    ## 4          33076      37367            89                    87
    ## 5          33076      37367            89                    87
    ## 6          33076      37367            89                    87
    ##   perc.highscl.95ci.high perc.highscl.quart perc.childpov
    ## 1                     90                  1            16
    ## 2                     90                  1            16
    ## 3                     90                  1            16
    ## 4                     90                  1            16
    ## 5                     90                  1            16
    ## 6                     90                  1            16
    ##   perc.childpov.95ci.low perc.childpov.95ci.high perc.childpov.quart
    ## 1                     11                      21                   1
    ## 2                     11                      21                   1
    ## 3                     11                      21                   1
    ## 4                     11                      21                   1
    ## 5                     11                      21                   1
    ## 6                     11                      21                   1
    ##   income.ratio income.ratio.quart
    ## 1          5.1                  2
    ## 2          5.1                  2
    ## 3          5.1                  2
    ## 4          5.1                  2
    ## 5          5.1                  2
    ## 6          5.1                  2

``` r
ggplot(county_health_full,aes(x=long,y=lat,fill=perc.obese,group=group)) + 
  geom_polygon(color="white",linewidth=0.05) +
  coord_map(projection="albers",lat0=39,lat1=45) + 
  scale_fill_distiller(palette="Reds",trans="reverse",na.value="white") +
  labs(fill="Percent of Obese Adults, 2021") +
  xlab("") + 
  ylab("") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

``` r
ggplot(county_health_full,aes(x=long,y=lat,fill=perc.obese.quart,group=group)) + 
  geom_polygon(color="white",linewidth=0.05) +
  coord_map(projection="albers",lat0=39,lat1=45) + 
  scale_fill_distiller(palette="Reds",trans="reverse",na.value="white") +
  labs(fill="Quartiles of Obese Adults, 2021") +
  xlab("") + 
  ylab("") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

``` r
ggplot(county_health_full,aes(x=long,y=lat,fill=prev.hosp.rate,group=group)) + 
  geom_polygon(color="white",linewidth=0.05) +
  coord_map(projection="albers",lat0=39,lat1=45) + 
  scale_fill_distiller(palette="Reds",trans="reverse",na.value="white") +
  labs(fill="Preventable Hospitalization Rate, 2021") +
  xlab("") + 
  ylab("") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

``` r
ggplot(county_health_full,aes(x=long,y=lat,fill=yppl.rate,group=group)) + 
  geom_polygon(color="white",linewidth=0.05) +
  coord_map(projection="albers",lat0=39,lat1=45) + 
  scale_fill_distiller(palette="Reds",trans="reverse",na.value="white") +
  labs(fill="Years of Preventable Life Lost, 2021") +
  xlab("") + 
  ylab("") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

### Adding in USDA livestock data

``` r
county_livestock_data <- read_csv("/Users/kenjinchang/github/seasonality-of-us-meat-consumption/data/county_livestock_data.csv") %>%
  select(FIPSTEXT,y17_M110_valueNumeric,y17_M111_valueNumeric,y17_M113_valueNumeric,y17_M114_valueNumeric) %>%
  rename(fips=FIPSTEXT,chg_tot_cattle=y17_M110_valueNumeric,avg_num_cattle=y17_M111_valueNumeric,chg_milkcow=y17_M113_valueNumeric,chg_beefcow=y17_M114_valueNumeric)
```

    ## Rows: 3080 Columns: 41
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (27): FIPSTEXT, y17_M110_valueText, y17_M110_classRange, y17_M111_valueT...
    ## dbl (14): FIPS, y17_M110_valueNumeric, y17_M111_valueNumeric, y17_M112_value...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
county_livestock_data %>%
  head(6)
```

    ## # A tibble: 6 × 5
    ##   fips  chg_tot_cattle avg_num_cattle chg_milkcow chg_beefcow
    ##   <chr>          <dbl>          <dbl>       <dbl>       <dbl>
    ## 1 00000           1.18             10        44.1        1.03
    ## 2 01001           0.2              15        52.7       -0.99
    ## 3 01003           0.26             13        50.2       -0.58
    ## 4 01005           0.55             15        58.1       NA   
    ## 5 01007           0.78             14        54.5       NA   
    ## 6 01009           0.67             31        47.4       NA

### Spatial Joining USDA livestock data with USGS water use and map data

``` r
county_livestock_water_full <- left_join(county_livestock_data,county_water_full, by='fips')
county_livestock_water_full %>%
  head(6)
```

    ## # A tibble: 6 × 17
    ##   fips  chg_tot_c…¹ avg_n…² chg_m…³ chg_b…⁴  long   lat group order id    region
    ##   <chr>       <dbl>   <dbl>   <dbl>   <dbl> <dbl> <dbl> <dbl> <int> <chr> <chr> 
    ## 1 00000        1.18      10    44.1    1.03  NA    NA      NA    NA <NA>  <NA>  
    ## 2 01001        0.2       15    52.7   -0.99 -86.5  32.3     1     1 alab… alaba…
    ## 3 01001        0.2       15    52.7   -0.99 -86.5  32.4     1     2 alab… alaba…
    ## 4 01001        0.2       15    52.7   -0.99 -86.5  32.4     1     3 alab… alaba…
    ## 5 01001        0.2       15    52.7   -0.99 -86.6  32.4     1     4 alab… alaba…
    ## 6 01001        0.2       15    52.7   -0.99 -86.6  32.4     1     5 alab… alaba…
    ## # … with 6 more variables: subregion <chr>, state <chr>, county <chr>,
    ## #   acres <dbl>, groundwater <dbl>, freshwater <dbl>, and abbreviated variable
    ## #   names ¹​chg_tot_cattle, ²​avg_num_cattle, ³​chg_milkcow, ⁴​chg_beefcow

``` r
ggplot(county_livestock_water_full,aes(x=long,y=lat,fill=avg_num_cattle,group=group)) + 
  geom_polygon(color="white",linewidth=0.05) +
  coord_map(projection="albers",lat0=39,lat1=45) + 
  scale_fill_distiller(palette="Oranges",trans="reverse",na.value="white") +
  labs(fill="Average Number of Cattle and Calves per 100 Acres, 2017") +
  xlab("") + 
  ylab("") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

``` r
ggplot(county_livestock_water_full,aes(x=long,y=lat,fill=chg_milkcow,group=group)) + 
  geom_polygon(color="white",linewidth=0.05) +
  coord_map(projection="albers",lat0=39,lat1=45) + 
  scale_fill_distiller(palette="Oranges",trans="reverse",na.value="white") +
  labs(fill="Change in Milk Cow Inventory, 2012-2017") +
  xlab("") + 
  ylab("") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
ggplot(county_livestock_water_full,aes(x=long,y=lat,fill=chg_beefcow,group=group)) + 
  geom_polygon(color="white",linewidth=0.05) +
  coord_map(projection="albers",lat0=39,lat1=45) + 
  scale_fill_distiller(palette="Oranges",trans="reverse",na.value="white") +
  labs(fill="Change in Beef Cow Inventory, 2012-2017") +
  xlab("") + 
  ylab("") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

### Adding in USDA Economics Dataset

``` r
county_economics_data <- read_csv("/Users/kenjinchang/github/seasonality-of-us-meat-consumption/data/county_economics_data.csv") %>%
  select(FIPSTEXT,y17_M044_valueNumeric) %>%
  rename(fips=FIPSTEXT,feedexp_prop=y17_M044_valueNumeric)
```

    ## Rows: 3080 Columns: 152
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (97): FIPSTEXT, y17_M011_classRange, y17_M012_classRange, y17_M013_class...
    ## dbl (55): FIPS, y17_M011_valueText, y17_M011_valueNumeric, y17_M012_valueTex...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
county_economics_data %>%
  head(6)
```

    ## # A tibble: 6 × 2
    ##   fips  feedexp_prop
    ##   <chr>        <dbl>
    ## 1 00000        19.2 
    ## 2 01001        12.8 
    ## 3 01003         7.94
    ## 4 01005        38.4 
    ## 5 01007        17.7 
    ## 6 01009        56.6

### Spatial Joining USDA econnomics data with USDA livestock and USGS water use and map data

``` r
county_economics_livestock_water_full <- left_join(county_economics_data,county_livestock_water_full, by='fips')
county_economics_livestock_water_full %>%
  head(6)
```

    ## # A tibble: 6 × 18
    ##   fips  feedexp_…¹ chg_t…² avg_n…³ chg_m…⁴ chg_b…⁵  long   lat group order id   
    ##   <chr>      <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <dbl> <dbl> <dbl> <int> <chr>
    ## 1 00000       19.2    1.18      10    44.1    1.03  NA    NA      NA    NA <NA> 
    ## 2 01001       12.8    0.2       15    52.7   -0.99 -86.5  32.3     1     1 alab…
    ## 3 01001       12.8    0.2       15    52.7   -0.99 -86.5  32.4     1     2 alab…
    ## 4 01001       12.8    0.2       15    52.7   -0.99 -86.5  32.4     1     3 alab…
    ## 5 01001       12.8    0.2       15    52.7   -0.99 -86.6  32.4     1     4 alab…
    ## 6 01001       12.8    0.2       15    52.7   -0.99 -86.6  32.4     1     5 alab…
    ## # … with 7 more variables: region <chr>, subregion <chr>, state <chr>,
    ## #   county <chr>, acres <dbl>, groundwater <dbl>, freshwater <dbl>, and
    ## #   abbreviated variable names ¹​feedexp_prop, ²​chg_tot_cattle, ³​avg_num_cattle,
    ## #   ⁴​chg_milkcow, ⁵​chg_beefcow

``` r
ggplot(county_economics_livestock_water_full,aes(x=long,y=lat,fill=feedexp_prop,group=group)) + 
  geom_polygon(color="white",linewidth=0.05) +
  coord_map(projection="albers",lat0=39,lat1=45) + 
  scale_fill_distiller(palette="Purples",trans="reverse",na.value="white") +
  labs(fill="Proportion of 2017 expenses spent on livestock feed, 2017") +
  xlab("") + 
  ylab("") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

### loading in GADM level 2 US map data in prep for spatial join with WRI water stress data

``` r
gadm_map_data <- read_csv("/Users/kenjinchang/github/seasonality-of-us-meat-consumption/data/gadm_map_data.csv") %>%
  select(COUNTRY,NAME_1,NAME_2,GID_2,GID_1,GID_0) %>%
  rename(country=COUNTRY,name_1=NAME_1,name_2=NAME_2,gid_2=GID_2,gid_1=GID_1,gid_0=GID_0)
```

    ## Rows: 3148 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (10): GID_2, GID_0, COUNTRY, GID_1, NAME_1, NAME_2, VARNAME_2, TYPE_2, E...
    ## lgl  (3): NL_NAME_1, NL_NAME_2, CC_2
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
gadm_map_data %>%
  head(6)
```

    ## # A tibble: 6 × 6
    ##   country       name_1  name_2  gid_2     gid_1   gid_0
    ##   <chr>         <chr>   <chr>   <chr>     <chr>   <chr>
    ## 1 United States Alabama Autauga USA.1.1_1 USA.1_1 USA  
    ## 2 United States Alabama Baldwin USA.1.2_1 USA.1_1 USA  
    ## 3 United States Alabama Barbour USA.1.3_1 USA.1_1 USA  
    ## 4 United States Alabama Bibb    USA.1.4_1 USA.1_1 USA  
    ## 5 United States Alabama Blount  USA.1.5_1 USA.1_1 USA  
    ## 6 United States Alabama Bullock USA.1.6_1 USA.1_1 USA

``` r
us_wsi_data <- read_csv("/Users/kenjinchang/github/seasonality-of-us-meat-consumption/data/us_wsi_data.csv") %>%
  select(string_id,aq30_id,pfaf_id,gid_1,aqid,gid_0,name_0,name_1,area_km2,bws_raw,bws_score,bws_cat,bws_label)
```

    ## Rows: 3433 Columns: 260
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (58): string_id, gid_1, gid_0, name_0, name_1, bws_label, bwd_label, ia...
    ## dbl (202): aq30_id, pfaf_id, aqid, area_km2, bws_raw, bws_score, bws_cat, bw...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
us_wsi_data %>%
  head(6)
```

    ## # A tibble: 6 × 13
    ##   string_id      aq30_id pfaf_id gid_1  aqid gid_0 name_0 name_1 area_…¹ bws_raw
    ##   <chr>            <dbl>   <dbl> <chr> <dbl> <chr> <chr>  <chr>    <dbl>   <dbl>
    ## 1 355000-USA.2_…   29004  355000 USA.…  1047 USA   Unite… Alaska   782.    0.169
    ## 2 355000-USA.2_…   29005  355000 USA.…  1064 USA   Unite… Alaska   206.    0.169
    ## 3 355000-USA.2_…   29006  355000 USA.…  1079 USA   Unite… Alaska   255.    0.169
    ## 4 355000-USA.2_…   29007  355000 USA.…  1083 USA   Unite… Alaska   216.    0.169
    ## 5 355000-USA.2_…   29008  355000 USA.…  1085 USA   Unite… Alaska    60.9   0.169
    ## 6 355000-USA.2_…   29009  355000 USA.…  1092 USA   Unite… Alaska   271.    0.169
    ## # … with 3 more variables: bws_score <dbl>, bws_cat <dbl>, bws_label <chr>, and
    ## #   abbreviated variable name ¹​area_km2

``` r
gadm_wsi_full <- left_join(gadm_map_data,us_wsi_data, by='gid_1') %>%
  mutate(name_1.x=tolower(name_1.x),name_2=tolower(name_2)) %>%
  unite(id,c("name_1.x","name_2"),sep=", ",remove=FALSE)
gadm_wsi_full %>%
  head(6)
```

    ## # A tibble: 6 × 19
    ##   country id    name_…¹ name_2 gid_2 gid_1 gid_0.x strin…² aq30_id pfaf_id  aqid
    ##   <chr>   <chr> <chr>   <chr>  <chr> <chr> <chr>   <chr>     <dbl>   <dbl> <dbl>
    ## 1 United… alab… alabama autau… USA.… USA.… USA     732602…   53997  732602  1664
    ## 2 United… alab… alabama autau… USA.… USA.… USA     732605…   54007  732605  1400
    ## 3 United… alab… alabama autau… USA.… USA.… USA     732605…   54008  732605  1664
    ## 4 United… alab… alabama autau… USA.… USA.… USA     732606…   54012  732606  1400
    ## 5 United… alab… alabama autau… USA.… USA.… USA     732607…   54014  732607  1400
    ## 6 United… alab… alabama autau… USA.… USA.… USA     732607…   54015  732607  1414
    ## # … with 8 more variables: gid_0.y <chr>, name_0 <chr>, name_1.y <chr>,
    ## #   area_km2 <dbl>, bws_raw <dbl>, bws_score <dbl>, bws_cat <dbl>,
    ## #   bws_label <chr>, and abbreviated variable names ¹​name_1.x, ²​string_id

``` r
county_stress_economics_livestock_water_full <- left_join(gadm_wsi_full,county_economics_livestock_water_full, by='id') %>%
  filter(bws_label!=-9999) 
county_stress_economics_livestock_water_full %>%
  head(6)
```

    ## # A tibble: 6 × 36
    ##   country id    name_…¹ name_2 gid_2 gid_1 gid_0.x strin…² aq30_id pfaf_id  aqid
    ##   <chr>   <chr> <chr>   <chr>  <chr> <chr> <chr>   <chr>     <dbl>   <dbl> <dbl>
    ## 1 United… alab… alabama autau… USA.… USA.… USA     732602…   53997  732602  1664
    ## 2 United… alab… alabama autau… USA.… USA.… USA     732602…   53997  732602  1664
    ## 3 United… alab… alabama autau… USA.… USA.… USA     732602…   53997  732602  1664
    ## 4 United… alab… alabama autau… USA.… USA.… USA     732602…   53997  732602  1664
    ## 5 United… alab… alabama autau… USA.… USA.… USA     732602…   53997  732602  1664
    ## 6 United… alab… alabama autau… USA.… USA.… USA     732602…   53997  732602  1664
    ## # … with 25 more variables: gid_0.y <chr>, name_0 <chr>, name_1.y <chr>,
    ## #   area_km2 <dbl>, bws_raw <dbl>, bws_score <dbl>, bws_cat <dbl>,
    ## #   bws_label <chr>, fips <chr>, feedexp_prop <dbl>, chg_tot_cattle <dbl>,
    ## #   avg_num_cattle <dbl>, chg_milkcow <dbl>, chg_beefcow <dbl>, long <dbl>,
    ## #   lat <dbl>, group <dbl>, order <int>, region <chr>, subregion <chr>,
    ## #   state <chr>, county <chr>, acres <dbl>, groundwater <dbl>,
    ## #   freshwater <dbl>, and abbreviated variable names ¹​name_1.x, ²​string_id

``` r
ggplot(county_stress_economics_livestock_water_full,aes(x=long,y=lat,fill=bws_label,group=group)) + 
  geom_polygon(color="white",linewidth=0.01) +
  coord_map(projection="albers",lat0=39,lat1=45) + 
  scale_fill_manual(values=c('snow','honeydew1','honeydew2','honeydew3','honeydew4','slategrey'),na.value="white",breaks=c('Low (<10%)','Low - Medium (10-20%)','Medium - High (20-40%)','High (40-80%)','Extremely High (>80%)','Arid and Low Water Use')) +
  labs(fill="Water Stress Level") +
  xlab("") + 
  ylab("") +
  theme(legend.position="bottom",panel.grid=element_blank(),panel.background=element_blank(),axis.text=element_blank(),axis.ticks=element_blank())
```

![](analysis-script_files/figure-gfm/unnamed-chunk-50-1.png)<!-- -->

group_by(id,long,lat,group) %\>%
summarize(mean_bws_score=mean(bws_score,na.rm=TRUE)) group_by(id) %\>%
summarize(mean_bws_score=mean(bws_score,na.rm=TRUE)) %\>%

Next steps: need to add county health and county water data. Now looking
to pivot so that one project focuses on how county health aligns with
state retail patterns of red and processed meat and the other looks at
how agricultural subsidy payments align with water usage for irrigation
(and water scarcity), both at the county level

``` r
library(spdep)
```

    ## Loading required package: spData

    ## To access larger datasets in this package, install the spDataLarge
    ## package with: `install.packages('spDataLarge',
    ## repos='https://nowosad.github.io/drat/', type='source')`

    ## Loading required package: sf

    ## Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE

``` r
sorting_data <- county_stress_economics_livestock_water_full %>%
  select(id,fips,bws_raw,bws_score,bws_cat,bws_label,feedexp_prop,long,lat,group,order,state,county)
sorting_data %>%
  head(6)
```

    ## # A tibble: 6 × 13
    ##   id       fips  bws_raw bws_s…¹ bws_cat bws_l…² feede…³  long   lat group order
    ##   <chr>    <chr>   <dbl>   <dbl>   <dbl> <chr>     <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 alabama… 01001 0.00561       0       0 Low (<…    12.8 -86.5  32.3     1     1
    ## 2 alabama… 01001 0.00561       0       0 Low (<…    12.8 -86.5  32.4     1     2
    ## 3 alabama… 01001 0.00561       0       0 Low (<…    12.8 -86.5  32.4     1     3
    ## 4 alabama… 01001 0.00561       0       0 Low (<…    12.8 -86.6  32.4     1     4
    ## 5 alabama… 01001 0.00561       0       0 Low (<…    12.8 -86.6  32.4     1     5
    ## 6 alabama… 01001 0.00561       0       0 Low (<…    12.8 -86.6  32.4     1     6
    ## # … with 2 more variables: state <chr>, county <chr>, and abbreviated variable
    ## #   names ¹​bws_score, ²​bws_label, ³​feedexp_prop

resI \<-
localmoran(sorting_data\$feedexp_prop,nb2listw(paper.nb),na.action=na.omit)

gadm_wsi_full \<- left_join(gadm_map_data,us_wsi_data, by=‘gid_1’) %\>%
mutate(name_1.x=tolower(name_1.x),name_2=tolower(name_2)) %\>%
unite(id,c(“name_1.x”,“name_2”),sep=“,”,remove=FALSE) gadm_wsi_full %\>%
head(6)
