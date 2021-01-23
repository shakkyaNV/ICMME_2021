ICMME
================
eNVy
2021-01-23

``` r
library(tidyverse)
```

### Import all files

``` r
# import all %>% nest
all_df <- list.files(path = "../Data/", 
                                pattern = "*.csv", 
                                full.names = T)
nested_tbl <- map(all_df, read_csv)

index_list <- all_df %>% 
  str_remove(pattern = "../Data/") %>% 
  str_remove(".csv")

# index_list[13] <- index_list[str_detect(index_list, 
#                                         pattern = "imo")] %>% 
#   str_remove(pattern = "../Data/")
```

``` r
# assign names to dfs
for (i in 1:length(index_list)) {
  assign(index_list[i], nested_tbl[[i]])
} 

df <- imo_df %>% 
  select(c(1,2,13)) %>% 
  rename(Year = year, Code = country) # DF main

df %>% head()
```

    # A tibble: 6 x 3
       Year Code  award     
      <dbl> <chr> <chr>     
    1  1984 BGR   Gold medal
    2  1984 GDR   Gold medal
    3  1984 ROU   Gold medal
    4  1984 USS   Gold medal
    5  1984 USS   Gold medal
    6  1984 USS   Gold medal

### Test Merge

Conduct the following before full-on merge

-   Rename all dfs – “Year”, “Code”, “xxxindex” (?manual) –&gt; (done by
    hand)
-   Lambda with \~.fun()

``` r
test_left <- left_join(df, df_egg_consumption, 
                       by = c("Year", "Code"))  

test_left %>% head
```

    # A tibble: 6 x 5
       Year Code  award      Entity   egg_index
      <dbl> <chr> <chr>      <chr>        <dbl>
    1  1984 BGR   Gold medal Bulgaria      13.2
    2  1984 GDR   Gold medal <NA>          NA  
    3  1984 ROU   Gold medal Romania       14.6
    4  1984 USS   Gold medal <NA>          NA  
    5  1984 USS   Gold medal <NA>          NA  
    6  1984 USS   Gold medal <NA>          NA  

To do:

-   Find missing country codes
-   Remove if necessary

``` r
pull_country_codes <- function(df ){
  df %>% 
  filter(is.na(Entity)) %>% 
  pull(Code) %>% 
  unique() %>% list("CodesWithIssues" = .) # country codes without identifiers, 2491 such codes
}

pull_country_codes(df = test_left )
```

    $CodesWithIssues
     [1] "GDR" "USS" "UNK" "GER" "CZS" "HEL" "LUX" "YUG" "BEL" "ALG" "SGP" "PHI"
    [13] "POR" "BAH" "SUI" "DEN" "CIS" "SAF" "NCY" "CHI" "MAS" "PAR" "PRI" "BRU"
    [25] "SCG" "LIE" "UAE" "SYR" "KSV"

To do:

-   Find out if the codes are same for all index data sets
-   Check ![URL](https://laendercode.net/en/3-letter-list.html)

### Full-on merge

``` r
too_many_dfs <- ls(pattern = "df_") %>% noquote() %>% list()

full_merge <- function(x = df, by = c("Code" , "Year"), ...) {
  left_join(x, ..., by = by, copy = TRUE) %>% 
    select_at(vars(-contains("Entity"))) 
}

df_loser <- full_merge(y = df_egg_consumption) %>% 
  full_merge(y = df_entrace_age_pre_primary_education) %>% 
  full_merge(y = df_govt_expenditure_pre_primary_education) %>% 
  full_merge(y = df_life_exp_pre_primary_education) %>% 
  full_merge(y = df_lower_secondary_education) %>% 
  full_merge(y = df_meat_consumption) %>% 
  full_merge(y = df_milk_consumption) %>% 
  full_merge(y = df_per_capita_calory_consumption) %>% 
  full_merge(y = df_pre_primary_education) %>% 
  full_merge(y = df_protein_supply) %>% 
  full_merge(y = df_pupil_teacher_ratio) 
```

``` r
countryCodesWithIssues <- df_loser %>% 
    # select_at(Code, vars(contains("index"))) %>% head()
    filter_at(vars(contains("index")), any_vars(rlang::are_na(.))) %>% 
    pull(Code) %>% 
    unique() %>% 
    list("countryCodesWithIssues" = .) 

countryCodesWithIssues
```

    $countryCodesWithIssues
      [1] "BGR" "GDR" "ROU" "USS" "USA" "VNM" "HUN" "UNK" "GER" "MNG" "CZS" "AUS"
     [13] "FRA" "HEL" "AUT" "POL" "NLD" "NOR" "COL" "LUX" "YUG" "BRA" "CAN" "CYP"
     [25] "BEL" "MAR" "CUB" "ALG" "SWE" "TUN" "FIN" "ESP" "KWT" "ITA" "ISR" "IRN"
     [37] "CHN" "TUR" "ISL" "MEX" "PER" "URY" "PAN" "NIC" "SGP" "NZL" "HKG" "KOR"
     [49] "PHI" "IRL" "ARG" "IDN" "ECU" "IND" "THA" "POR" "VEN" "JPN" "PRK" "BAH"
     [61] "MAC" "SUI" "TTO" "DEN" "CIS" "RUS" "UKR" "TWN" "LVA" "ARM" "KAZ" "BLR"
     [73] "AZE" "LTU" "EST" "SAF" "CZE" "SVK" "GEO" "MKD" "SVN" "BIH" "HRV" "KGZ"
     [85] "MDA" "ALB" "NCY" "TKM" "CHI" "LKA" "MAS" "UZB" "BOL" "GTM" "PAR" "PRI"
     [97] "BRU" "SCG" "MOZ" "SAU" "CRI" "LIE" "PAK" "BGD" "SRB" "NGA" "KHM" "MNE"
    [109] "HND" "UAE" "SYR" "ZWE" "BEN" "MRT" "CIV" "KSV" "TJK" "SLV" "UGA" "BFA"
    [121] "GMB" "TZA" "GHA" "BWA" "JAM" "MMR" "KEN" "MDG" "EGY" "IRQ" "LAO" "NPL"

``` r
# remaining if na filtered
df_loser %>% select(-award) %>% na.omit() %>% dim()
```

    [1] 1858   13
