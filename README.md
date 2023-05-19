## Functions for Processing and Modeling Delay Discounting Data From the ABCD Study

The R package **ddabcd** provides a collection of functions to help researchers extract, clean, and model data from the delay discounting task used in the [Adolescent Brain Cognitive Development (ABCD) Study&reg;](https://abcdstudy.org/).

### Table of contents

1. [Prerequisites](#section01)
2. [Installation](#section02)
3. [The delay discounting task](#section03)
4. [Usage](#section04)
5. [References](#REF)

### Prerequisites {#section01}

- [R (version >= 4.2.1)](https://cran.r-project.org/)
- [tidyr (version >= 1.2.0)](https://tidyr.tidyverse.org/)
- [bbmle (version >= 1.0.25)](https://cran.r-project.org/web/packages/bbmle/index.html)
- For easy installation it is recommended to have [devtools (version >= 2.4.3)](https://devtools.r-lib.org/)

### Installation {#section02}

```r
# Install development version from Github
devtools::install_github("rettopnivek/ddabcd")
```

### The delay discounting task {#section03}

Details on the delay discounting task used in the ABCD study are provided by [Luciana et al. (2018)](#REF). In brief, the ABCD study assessed delay discounting via a task using an adjusting-delay procedure. On each trial of the task, participants had to chose between two hypothetical rewards: a reward of \$100 after a delay or an immediate reward of varying magnitudes. The magnitude of the immediate reward was automatically adjusted between trials on a given block based on a participant's previous response. Participants completed a total of 42 trials over 7 randomized blocks (with 6 trials per block), where each block corresponded to a different delay interval for the future reward of \$100. Delay intervals for the future \$100 were: 6 hours, 1 day, 1 week, 1 month, 3 months, 1 year, and 5 years.

At each delay interval, an indifference score was calculated. The indifference score represents how much an \$100 reward is worth following a given delay. The indifference score can be thought of as the proportion that an immediate reward is worth following a delay. Indifference scores in the ABCD study varied from 99.21875 to 0.78125.

Participants also completed 3 validity questions in between blocks. Participants chose between \$100 now versus \$100 following a delay (e.g., 5 years), with the rational choice always being the immediate amount. Failure to pick the rational choice on all 3 questions was taken to indicate inattentive behavior.

### Usage {#section04}

After installation the package can be loaded via the standard library call:
```r
library(ddabcd)
```

The package comes with simulated data similar in format to the data provided by the ABCD study. Note that direct access to the ABCD data requires approval from the [NDA Data Access Committee](https://nda.nih.gov/abcd/). See [Saragosa-Harris et al. (2022)](#REF) for advice on using data from the ABCD study.

```r
# Example wide-form data set with simulated indifference scores
data(ddabcd_data_example_wide)
# Use data from 1st time point
dtf_wide <- ddabcd_data_example_wide %index% 
  list( '1_year_follow_up_y_arm_1', 'eventnames' )
```

ABCD study data are primarily formatted with separate rows per participant and time point and with multiple columns for each the indifference scores computed at the different delay durations. These data can be converted to long-form data (separate rows for each indifference score at each delay duration). In addition to this conversion, useful quality checks are computed. Indicators are provided for non-missing data, validity checks for irrational responding, whether data met the criteria of [Johnson & Bickel (2008)](#REF), and whether a one-sided Mann-Kendall test for monotonicity was statistically significant.

```r
dtf_long <- ddabcd_data_prep(dtf_wide)
```

Once data has been converted to long-form, 3 different models of discounting can be fit to each set of data per participant. Models that are fit are (1) a null model with an intercept only, (2) [Mazur's one-parameter (1987)](#REF) model, and (3) [Rachlin's two-parameter (2006)](#REF) model.

```r
# Fit data using non-linear least-squares method
lst_all_fits.nls <- ddabcd_fit_models_across_participants(
  dtf_long,
  ddabcd_fit_models_using_nls
)
```

Parameter estimates per participant can be added as a column to an existing data frame to allow for easy analysis of variables that covary with discounting model parameters.
```r
# Add discounting rate estimates from Mazur's (1987) model
dtf_wide <- ddabcd_post_fit_add_to_data( 
  dtf_wide, 
  lst_all_fits.nls, 
  'ddabcd_estimates_M1987_nls.lnk',
  fun_extract = function(x) x %pull_coef% 'lnk',
  chr_model = 'M1987',
  lgc_filtering = dtf_wide$eventname == '1_year_follow_up_y_arm_1'
)

# Add discounting rate estimates from Rachlin's (2006) model
dtf_wide <- ddabcd_post_fit_add_to_data( 
  dtf_wide, 
  lst_all_fits.nls, 
  'ddabcd_estimates_R2006_nls.lnk',
  fun_extract = function(x) x %pull_coef% 'lnk',
  chr_model = 'R2006',
  lgc_filtering = dtf_wide$eventname == '1_year_follow_up_y_arm_1'
)

# Predict discounting rates using biological sex
lst_pred_of_lnk <- 
  lm( ddabcd_estimates_M1987_nls.lnk ~ sex, data = dtf_wide )
```

The functions and examples here expand upon original work by [Kohler, Lichenstein, and Yip (2022)](#REF), who described how to fit [Mazur's (1987)](#REF) model to the ABCD study data using the base R function **nls** and reported analyses of how demographic, impulsivity, and substance use risk factor measures covary with the discounting rate parameter.

### References {#REF}

Johnson, M. W., & Bickel, W. K. (2008). An algorithm for identifying nonsystematic delay-discounting data. *Experimental and Clinical Psychopharmacology*, *16* (3), 264-274. https://doi.org/10.1037/1064-1297.16.3.264


Kohler, R. J., Lichenstein, S. D., & Yip, S. W. (2022). Hyperbolic discounting rates and risk for problematic alcohol use in youth enrolled in the Adolescent Brain and Cognitive Development Study. *Addiction Biology*, *27* (2), 1-13. https://doi.org/10.1111/adb.13160


Luciana, M., Bjork, J., Nagel, B., Barch, D. M., Gonzalex, R., Nixon, S. J., & Banich, M. T. (2018). Adolescent neurocognitive development and impacts of substance use: Overview of the Adolescent Brain Cognitive Development (ABCD) baseline neurocognition battery. *Developmental Cognitive Neuroscience*, *32*, 67-79. https://doi.org/10.1016/j.dcn.2018.02.006


Mazur, J. E. (1987). An adjusting procedure for studying delayed reinforcement]. In M. L. Commons, J. E. Mazur, J. A. Nevin, & H. Rachlin (Eds.), *The effect of delay and intervening events on reinforcement value* (pp. 55-73). Lawrence Erlbaum Associates, Inc.


Rachlin, H. (2006). Notes on discounting. *Journal of the Experimental Analysis of Behavior*, *85* (3), 425-435. https://doi.org/10.1901/jeab.2006.85-05


Saragosa-Harris, N. M., Chaku, N., MacSweeney, N., Williamson, V. G., Scheuplein, M., Feola, B., ..., & Mills, K. L. (2022). A practical guide for researchers and reviewers using the ABCD Study and other large longitudinal datasets. *Developmental Cognitive Neuroscience*, *55*, 1-11. https://doi.org/10.1016/j.dcn.2022.101115


