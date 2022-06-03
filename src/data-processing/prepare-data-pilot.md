Prepare Data Pilot Studies
================

-   [Required Packages &
    Reproducibility](#required-packages--reproducibility)
-   [Tidy Data](#tidy-data)
-   [Save Data for Analysis](#save-data-for-analysis)
-   [Visualization of Data](#visualization-of-data)
    -   [Dependent Variable](#dependent-variable)
    -   [Independent Variables](#independent-variables)

## Required Packages & Reproducibility

``` r
rm(list=ls())
source(here::here("src/lib/functions.R"))
#renv::snapshot()
```

## Tidy Data

This code chuck downloads the data from cleans the raw data. We impute
missing values based on the following criteria:

-   If 10% or less of the values on the dimension are missing, then we
    re-code the missing values to the overall mean.
-   If 11% or more of the values on the dimension are missing, then we
    re-code the missing values to a constant (for instance 0) and
    include a dummy variable indicating whether the response on the
    covariate was missing or not.

We recode the missing values of the conspiracy belief variables, age,
gender, and ideology to the mean value of the respective variables. For
the conspiracy belief variables as well as for the ideology variable, we
additionally add a dummy variable indicating whether this variable was
missing.

``` r
d1 <- fetch_survey(surveyID = "SV_6nXTxSvj5FOOZsW", 
                    verbose = TRUE, force_request = T,
                    label = FALSE, convert = FALSE) #anonymous message
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%

``` r
d2 <- fetch_survey(surveyID = "SV_8H3HjWDoT94ymRE", 
                    verbose = TRUE, force_request = T,
                    label = FALSE, convert = FALSE) #FvD message
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%

``` r
source(here("src/data-processing/pilot.R"))
#For the pilot, replace all missing values with mean/media
source(here("src/data-processing/missing-pilot.R"))
```

``` r
source(here("src/data-processing/scales.R"))

kbl(scales, booktabs =T, caption = "Reliability Scales") %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F, fixed_thead = T, position = "center") %>%
  column_spec(1, width = "6cm") %>%
  column_spec(2, width = "6cm")  
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Reliability Scales
</caption>
<thead>
<tr>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
alpha
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
variable
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;width: 6cm; ">
0.63
</td>
<td style="text-align:left;width: 6cm; ">
Political Vulnarability
</td>
</tr>
<tr>
<td style="text-align:right;width: 6cm; ">
0.71
</td>
<td style="text-align:left;width: 6cm; ">
Cultural Vulnarability
</td>
</tr>
<tr>
<td style="text-align:right;width: 6cm; ">
0.84
</td>
<td style="text-align:left;width: 6cm; ">
Relative Deprivation
</td>
</tr>
<tr>
<td style="text-align:right;width: 6cm; ">
0.35
</td>
<td style="text-align:left;width: 6cm; ">
Need for Cognition
</td>
</tr>
<tr>
<td style="text-align:right;width: 6cm; ">
0.57
</td>
<td style="text-align:left;width: 6cm; ">
Nativism
</td>
</tr>
<tr>
<td style="text-align:right;width: 6cm; ">
0.73
</td>
<td style="text-align:left;width: 6cm; ">
Collective Narcisism
</td>
</tr>
<tr>
<td style="text-align:right;width: 6cm; ">
0.68
</td>
<td style="text-align:left;width: 6cm; ">
Collective Nostalgia
</td>
</tr>
<tr>
<td style="text-align:right;width: 6cm; ">
0.64
</td>
<td style="text-align:left;width: 6cm; ">
Populist Attitudes
</td>
</tr>
<tr>
<td style="text-align:right;width: 6cm; ">
0.63
</td>
<td style="text-align:left;width: 6cm; ">
Pluralist Attitudes
</td>
</tr>
<tr>
<td style="text-align:right;width: 6cm; ">
0.09
</td>
<td style="text-align:left;width: 6cm; ">
Elitist Attitudes
</td>
</tr>
<tr>
<td style="text-align:right;width: 6cm; ">
0.40
</td>
<td style="text-align:left;width: 6cm; ">
Nationalism
</td>
</tr>
<tr>
<td style="text-align:right;width: 6cm; ">
-0.43
</td>
<td style="text-align:left;width: 6cm; ">
Agreeableness
</td>
</tr>
<tr>
<td style="text-align:right;width: 6cm; ">
0.84
</td>
<td style="text-align:left;width: 6cm; ">
Trust in Institutions
</td>
</tr>
<tr>
<td style="text-align:right;width: 6cm; ">
0.63
</td>
<td style="text-align:left;width: 6cm; ">
Collectiive Nostalgia
</td>
</tr>
</tbody>
</table>

``` r
rm(scales)
```

## Save Data for Analysis

``` r
save(d1, file = here("data/intermediate/pilot1.RData"))
save(d2, file = here("data/intermediate/pilot2.RData"))
```

## Visualization of Data

### Dependent Variable

<img src="../../report/figures/Dependent Variable Obs-1.png" style="display: block; margin: auto;" />

### Independent Variables

<img src="../../report/figures/Independent Variables Obs-1.png" style="display: block; margin: auto;" />
