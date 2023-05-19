# Functions for processing/modeling ABCD delay discounting data
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2023-05-19

# Table of contents
# 1) ddabcd_data_example_wide
# 2) ddabcd_data_example_long
# 3) ddabcd_data_prep
#   3.1) Extract new column names
#   3.2) Check function inputs
#   3.3) Data quality checks
#   3.4) Create long-form data set
# 4) ddabcd_data_censor
# 5) ddabcd_data_merge_by_id_and_time_point
# 6) ddabcd_data_default_col
# 7) ddabcd_data_delay_durations

#### 1) ddabcd_data_example_wide ####
#' Example Wide-Form Data in Format of ABCD Study
#'
#' An example data set with simulated indifference
#' scores for 48 participants over two time points
#' formatted in the same style as data from the the
#' [ABCD study](https://abcdstudy.org/). Values
#' were simulated from Rachlin's (2006)
#' [model](https://doi.org/10.1901/jeab.2006.85-05)
#' with data following a logit-normal distribution.
#' The generating log discounting rate and sensitivity
#' parameters were specified to be weakly correlated with
#' biological sex.
#'
#' @format ## `ddabcd_data_example_wide`
#' A data frame with 96 rows and 15 columns:
#' \describe{
#'   \item{src_subject_id}{
#'     Participant ID
#'   }
#'   \item{eventname}{
#'     Study time point
#'   }
#'   \item{age}{
#'     Participant age in months
#'   }
#'   \item{sex}{
#'     Biological sex at birth for participant
#'   }
#'   \item{ddis_scr_val_indif_pnt_1da}{
#'     Indifference score for delay of 1 day
#'   }
#'   \item{ddis_scr_val_indif_pnt_1week}{
#'     Indifference score for delay of 1 day
#'   }
#'   \item{ddis_scr_val_indif_pnt_1mth}{
#'     Indifference score for delay of 1 month
#'   }
#'   \item{ddis_scr_val_indif_pnt_3mth}{
#'     Indifference score for delay of 3 months
#'   }
#'   \item{ddis_scr_val_indif_pnt_1yr}{
#'     Indifference score for delay of 1 year
#'   }
#'   \item{ddis_scr_val_indif_pnt_5yr}{
#'     Indifference score for delay of 5 years
#'   }
#'   \item{ddis_scr_val_immedcho}{
#'     Count of times participant picked rational immediate choice for 3
#'     validity check questions
#'   }
#'   \item{ddabcd_simulation_lnk}{
#'     The generating parameter value for the participant
#'     (log discounting rate)
#'   }
#'   \item{ddabcd_simulation_s}{
#'     The generating parameter value for the participant
#'     (sensitivity)
#'   }
#'   \item{ddabcd_simulation_lnd}{
#'     The generating parameter value for the participant
#'     (log standard deviation for logit-normal distribution)
#'   }
#' }
"ddabcd_data_example_wide"

#### 2) ddabcd_data_example_long ####
#' Example Long-Form Data
#'
#' An example data set with simulated indifference
#' scores for 48 participants over two time points
#' with separate rows for each delay duration, time
#' point, and participant. Data are the result of the
#' application of the [ddabcd::ddabcd_data_prep]
#' function to the [ddabcd::ddabcd_data_example_wide]
#' data set. Values were simulated from Rachlin's (2006)
#' [model](https://doi.org/10.1901/jeab.2006.85-05)
#' with data following a logit-normal distribution.
#' The generating log discounting rate and sensitivity
#' parameters were specified to be weakly correlated with
#' biological sex.
#'
#' @format ## `ddabcd_data_example_long`
#' A data frame with 672 rows and 21 columns:
#' \describe{
#'   \item{src_subject_id}{
#'     Participant ID
#'   }
#'   \item{eventname}{
#'     Study time point
#'   }
#'   \item{age}{
#'     Participant age in months
#'   }
#'   \item{sex}{
#'     Biological sex at birth for participant
#'   }
#'   \item{ddis_scr_val_immedcho}{
#'     Count of times participant picked rational immediate choice for 3
#'     validity check questions
#'   }
#'   \item{ddis_scr_val_immedcho}{
#'     Count of times participant picked rational immediate choice for 3
#'     validity check questions
#'   }
#'   \item{ddabcd_simulation_lnk}{
#'     The generating parameter value for the participant
#'     (log discounting rate)
#'   }
#'   \item{ddabcd_simulation_s}{
#'     The generating parameter value for the participant
#'     (sensitivity)
#'   }
#'   \item{ddabcd_simulation_lnd}{
#'     The generating parameter value for the participant
#'     (log standard deviation for logit-normal distribution)
#'   }
#'   \item{ddacbd_quality_no_missing_data}{
#'     Indicator of non-NA indifference scores
#'   }
#'   \item{ddabcd_quality_picked_rational_immediate_choices}{
#'     Indicator of when participant picked all 3 rational choices
#'     for validity check questions
#'   }
#'   \item{ddabcd_quality_variation_in_responses}{
#'     Indicator for when participant did not have the same
#'     indifference score across all delay durations
#'   }
#'   \item{ddabcd_quality_met_Johson_and_Bickel_2008_criteria }{
#'     Indicator for when participant's data met criteria of
#'     Johnson and Bickel (2008)
#'   }
#'   \item{ddabcd_quality_significant_Mann_Kendall_test}{
#'     Indicator for when participant's data had a significant
#'     one-sided Mann-Kendall test of monotonicity
#'   }
#'   \item{ddabcd_delay_columns}{
#'     The original column names for the indifference scores across
#'     different delay durations
#'   }
#'   \item{ddabcd_indifference_score_100}{
#'     Indifference scores ranging from 0 - 100
#'   }
#'   \item{ddabcd_delay_months}{
#'     Delay durations in months
#'   }
#'   \item{ddabcd_delay_log_months}{
#'     The log of the months for delay durations
#'   }
#'   \item{ddabcd_delay_negative_log_months}{
#'     The negative of the log of the months for delay durations
#'   }
#'   \item{ddabcd_indifference_score_1}{
#'     Indifference scores ranging from 0 - 1
#'   }
#'   \item{ddabcd_indifference_score_log_odds}{
#'     Indifference scores ranging from 0 - 1 converted to log-odds
#'   }
#'   \item{ddabcd_indifference_score_censored}{
#'     Indicator for when an indifference score (ranging from 0 - 100)
#'     was left-censored (value fixed to minimum of 0.78125) with a score
#'     of -1, right-censored (value fixed to maximum of 99.21875) with a
#'     score of 1, or not censored with a score of 0
#'   }
#' }
"ddabcd_data_example_long"

#### 3) ddabcd_data_prep ####
#' Prepare ABCD Data for Delay Discounting Modeling
#'
#' Function to convert wide-form ABCD data with
#' indifference scores from the delay discounting task
#' into a long-form data set ready for modeling.
#'
#' @param dtf_data A data frame with one row per
#'   each time point and ABCD participant.
#' @param chr_columns A character vector with the 7
#'   columns with the indifference score (ranging from
#'   0 - 100) at each delay duration (in order from the
#'   shortest delay to the longest delay).
#' @param chr_indifference A character vector with the
#'   4 new column names in the long-form data set
#'   for 1) the indifference score (0 - 100), 2) the
#'   indifference score proportion (0 - 1), 3) the
#'   log-odds of the indifference score proportion,
#'   and 4) an indicator for whether the log-odds were
#'   left or right censored (-1 = left, 1 = right, 0 = none).
#' @param chr_delay A character vector with the 4 new
#'   column names in the long-form data set for 1)
#'   the original column names at each delay duration,
#'   2) the delay duration in months, 3) the log of the
#'   delay duration in months, and 4) the negative of
#'   the log of the delay duration in months.
#' @param chr_quality A character vector with the 5 new
#'   column names in the long-form data set for
#'   1) an indicator of non-missing data by participant,
#'   2) an indicator for participants who picked all 3
#'   rational immediate choice responses for the
#'   delay discounting validity check, 3) an
#'   indicator for when participants did not make
#'   the same response across all delay durations,
#'   4) an indicator for when participant data met
#'   the Johnson & Bickel (2008) criteria for
#'   valid data, and 5) an indicator for when
#'   participant data exhibit a statistically
#'   significant monotonic downward trend based
#'   on a one-sided Mann-Kendall test.
#' @param num_delay An optional numeric vector with
#'   the 7 values to use for the delay duration
#'   (in order from shortest to longest). Otherwise
#'   defaults to duration in months for delays of
#'   6 hours, 1 day, 1 week, 1 month, 3 months,
#'   1 year, and 5 years.
#' @param chr_immediate_choice_score A character string
#'   the column name in \code{dtf_data} tracking the
#'   frequency in which participants chose up to 3
#'   rational immediate choice responses during a
#'   validity check.
#' @param num_alpha_for_MK_test A proportion, the
#'   false-positive rate for the one-sided Mann-Kendall
#'   test.
#'
#' @references
#' Johnson, M. W., & Bickel, W. K. (2008). An algorithm
#' for identifying nonsystematic delay-discounting data.
#' Experimental and Clinical Psychopharmacology, 16 (3),
#' 264 - 274.
#' https://doi.org/10.1037/1064-1297.16.3.264
#'
#' @returns A data frame.
#'
#' @examples
#' # Load in example data set
#' data("ddabcd_data_example_wide")
#'
#' # Process data set
#' dtf_delay <- ddabcd_data_prep(ddabcd_data_example_wide)
#'
#' @export

ddabcd_data_prep <- function(
    dtf_data,
    chr_columns = '',
    chr_indifference = '',
    chr_delay = '',
    chr_quality = '',
    num_delay = NULL,
    chr_immediate_choice_score = '',
    num_alpha_for_MK_test = 0.05 ) {

  #### 3.1) Extract new column names ####

  # Default column names for original indifference score columns
  if ( all( chr_columns == '' ) ) {

    chr_columns <- c(
      "ddis_scr_val_indif_point_6h",
      "ddis_scr_val_indif_pnt_1da",
      "ddis_scr_val_indif_pnt_1week",
      "ddis_scr_val_indif_pnt_1mth",
      "ddis_scr_val_indif_pnt_3mth",
      "ddis_scr_val_indif_pnt_1yr",
      "ddis_scr_val_indif_pnt_5yr"
    )

    # Close 'Default column names for original indifference score columns'
  }

  # Default column names for new indifference score columns
  if ( all( chr_indifference == '' ) ) {

    chr_indifference <- c(
      'ddabcd_indifference_score_100',
      'ddabcd_indifference_score_1',
      'ddabcd_indifference_score_log_odds',
      'ddabcd_indifference_score_censored'
    )

    # Close 'Default column names for new indifference score columns'
  }

  # Default column names for new delay columns
  if ( all( chr_delay == '' ) ) {

    chr_delay <- c(
      'ddabcd_delay_columns',
      'ddabcd_delay_months',
      'ddabcd_delay_log_months',
      'ddabcd_delay_negative_log_months'
    )

    # Close 'Default column names for new delay columns'
  }

  # Default column names for new quality check columns
  if ( all( chr_quality == '' ) ) {

    chr_quality <- c(
      'ddabcd_quality_no_missing_data',
      'ddabcd_quality_picked_rational_immediate_choices',
      'ddabcd_quality_variation_in_responses',
      'ddabcd_quality_met_Johson_and_Bickel_2008_criteria',
      'ddabcd_quality_significant_Mann_Kendall_test'
    )

    # Close 'Default column names for new quality check columns'
  }

  # Default column name for validity check column
  if ( all( chr_immediate_choice_score == '' ) ) {

    chr_immediate_choice_score <- 'ddis_scr_val_immedcho'

    # Close 'Default column names for new quality check columns'
  }

  # If no data set provided
  if ( is.null(dtf_data) ) {

    chr_output <- c(
      chr_indifference,
      chr_delay,
      chr_quality
    )
    chr_output <- chr_output[ chr_output != '' ]

    return( chr_output )

    # Close 'If no data set provided'
  }

  #### 3.2) Check function inputs ####

  if ( !is.data.frame(dtf_data) ) {

    chr_error <- paste0(
      "Argument 'dtf_data' must be data frame with ABCD study data."
    )
    stop( chr_error)

  }

  if ( !all( chr_columns %in% colnames(dtf_data) ) ) {

    chr_error <- paste0(
      "Columns for indifference scores per each delay ",
      "duration not found in 'dtf_data'."
    )
    stop( chr_error)

  }

  if ( !chr_immediate_choice_score %in% colnames(dtf_data) ) {

    chr_error <- paste0(
      "Column specifed by argument 'chr_immediate_choice_score' ",
      "not found in 'dtf_data'."
    )
    stop( chr_error)

  }

  if ( length( chr_columns ) != 7 ) {

    chr_error <- paste0(
      "Must provide all 7 columns (one for each delay duration) for ",
      "indifference scores, in order from shortest delay to longest delay."
    )
    stop( chr_error)

  }

  if ( length(chr_indifference) != 4 | !is.character(chr_indifference) ) {

    chr_error <- paste0(
      "Argument 'chr_indifference' must be a character vector giving new ",
      "column names of long-form variables for ",
      "[1] indifference scores ranging from 0 - 100, ",
      "[2] indifference scores ranging from 0 - 1, ",
      "[3] log-odds of indifference scores, ",
      "[4] censoring status of indifference scores.",
    )
    stop( chr_error)

  }

  if ( length(chr_quality) != 5 | !is.character(chr_quality) ) {

    chr_error <- paste0(
      "Argument 'chr_quality' must be a character vector giving new ",
      "column names of long-form variables for ",
      "[1] indicator for non-missing data for indifference scores, ",
      "[2] indicator for when participants picked all 3 rational ",
      " immediate choice responses for the validity check questions, ",
      "[3] indicator for when participants did not give same",
      " response across all delay durations, ",
      "[4] indicator for when data met criteria of Johnson & Bickel (2008), ",
      "[5] indicator for when data had statistically significant result for",
      " one-sided Mann-Kendall test of monotonicity."
    )
    stop( chr_error)

  }

  #### 3.3) Data quality checks ####

  mat_quality <- apply(
    dtf_data[, c( chr_columns, chr_immediate_choice_score)],
    1, function(x) {
      x <- unlist(x)
      lgc_checks <- rep( FALSE, 5)

      # If non-missing data
      if ( all( !is.na(x[1:7]) ) ) {

        lgc_checks[1] <- TRUE # No missing data
        lgc_checks[2] <- x[8] %in% 3 # Rational immediate choice
        lgc_checks[3] <- !all(x[1:7] == x[1]) # Variation in data

        # Criteria from Johnson & Bickel (2008)
        lgc_checks[4] <-
          ( x[2] < (x[1] + 20) &
              x[7] < (x[1] - 10) )

        # One-sided Mann-Kendall test for monotonicity
        if ( lgc_checks[3] ) {

          lst_MK_test <- Kendall::MannKendall( x[1:7] )

          lgc_checks[5] <- (
            lst_MK_test$tau < 0 &
              lst_MK_test$sl/2 < num_alpha_for_MK_test
          )

          # Close 'One-sided Mann-Kendall test for monotonicity'
        }

        # Close 'If non-missing data'
      }

      return( lgc_checks )
    } ) |> t()
  colnames( mat_quality ) <- chr_quality
  dtf_delay <- cbind( dtf_data, mat_quality )

  #### 3.4) Create long-form data set ####

  dtf_delay <- tidyr::pivot_longer(
    dtf_delay,
    cols = tidyr::all_of( chr_columns ),
    names_to = chr_delay[1],
    values_to = chr_indifference[1]
  ) |> data.frame()
  int_rows <- nrow(dtf_delay)

  # If no custom numeric values provided for delay
  if ( is.null( num_delay ) ) {

    num_average_days_per_month <- 30.457

    num_delay <- c(
      (6/24) / num_average_days_per_month, # Delay: 6 hours
      1/num_average_days_per_month, # Delay: 1 day
      7/num_average_days_per_month, # Delay: 1 week
      1, # Delay: 1 month
      3, # Delay: 3 months
      12, # Delay: 1 year
      60 # Delay: 5 years
    )

  }

  mat_indifference <- matrix( NA, int_rows, 3 )
  colnames( mat_indifference ) <- chr_indifference[-1]
  mat_delay <- matrix( NA, int_rows, 3 )
  colnames( mat_delay ) <- chr_delay[-1]

  # Loop over delay durations
  for (d in seq_along( chr_columns ) ) {

    # Subset of rows for current delay duration
    lgc_rows <-
      dtf_delay[[ chr_delay[1] ]] %in% chr_columns[d]

    # Indifference scores as proportion
    mat_indifference[lgc_rows, chr_indifference[2]] <-
      dtf_delay[[chr_indifference[1]]][lgc_rows] / 100
    # Log-odds for indifference score
    mat_indifference[lgc_rows, chr_indifference[3]] <- log(
      mat_indifference[lgc_rows, chr_indifference[2]] /
        (1 - mat_indifference[lgc_rows, chr_indifference[2]])
    )

    # Numeric values for delays
    mat_delay[lgc_rows, chr_delay[2]] <-
      num_delay[d]
    mat_delay[lgc_rows, chr_delay[3]] <-
      log(num_delay[d])
    mat_delay[lgc_rows, chr_delay[4]] <-
      -log(num_delay[d])

    # Close 'Loop over delay durations'
  }

  # Determine if data are censored
  num_range <- range(
    mat_indifference[, chr_indifference[3]], na.rm = TRUE
  )

  lgc_left_censored <-
    mat_indifference[, chr_indifference[3]] %in% num_range[1]
  lgc_right_censored <-
    mat_indifference[, chr_indifference[3]] %in% num_range[2]
  lgc_no_censoring <-
    !is.na( mat_indifference[, chr_indifference[3]] ) &
    !lgc_left_censored &
    !lgc_right_censored

  mat_indifference[lgc_left_censored, chr_indifference[4]] <- -1
  mat_indifference[lgc_right_censored, chr_indifference[4]] <- 1
  mat_indifference[lgc_no_censoring, chr_indifference[4]] <- 0

  # Add new columns
  dtf_delay <- cbind(
    dtf_delay,
    mat_delay,
    mat_indifference
  )

  return( dtf_delay )
}

#### 4) ddabcd_data_censor ####
#' Function to Left and Right-Censor Data
#'
#' Function to left and right-censor numeric
#' values in a column in a data frame.
#'
#' @param obj_data Either a vector of values
#'   or a data frame.
#' @param chr_outcome An optional character string,
#'   the column in \code{obj_data} to left and
#'   right-censor if \code{obj_data} is a data frame.
#' @param lgc_indicator Logical; if \code{TRUE}
#'   returns an indicator for whether values
#'   were censored (-1 for left-censored, 1 for
#'   right-censored, and 0 otherwise) rather than
#'   the censored data itself.
#' @param num_limits A numeric vector with the lower
#'   and upper limits below and above which to
#'   censor values, respectively. By default is
#'   set to the lowest and highest possible
#'   indifference scores found in the
#'   [ABCD study](https://abcdstudy.org/) data set.
#' @param num_scaling A numeric value, a scaling
#'   constant by which to adjust the numeric values
#'   (useful, for instance, if data is in the
#'   range of 0 - 1 but the limits are in the range
#'   of 0 - 100).
#'
#' @return Either a numeric vector of censored value,
#' or if \code{lgc_indicator} is \code{TRUE}, an
#' integer vector with values of -1 for left-censored
#' data, 1 for right-censored data, and 0 otherwise.
#'
#' @examples
#' # Example data set
#' dtf_data <-
#'   data.frame( ddabcd_indifference_score_1 = c( 0, 1, .5 ) )
#'
#' # Indicator for censored values
#' ddabcd_data_censor( dtf_data, lgc_indicator = TRUE )
#' # Censored values
#' ddabcd_data_censor( dtf_data )
#'
#' # Vector of values
#' ddabcd_data_censor( c(-5, 0, 5), chr_outcome = '', num_scaling = 1)
#'
#' @export

ddabcd_data_censor <- function(
    obj_data,
    chr_outcome = 'ddabcd_indifference_score_1',
    lgc_indicator = FALSE,
    num_limits = c(0.78125, 99.21875),
    num_scaling = 100) {

  # Initialize output
  num_output <- rep( NA, nrow(dtf_data) )

  # If no column name provided
  if ( chr_outcome == '' ) {

    num_values <- obj_data

    # Close 'If no column name provided'
  } else {

    num_values <- dtf_data[[ chr_outcome ]]

    # Close else for 'If no column name provided'
  }

  lgc_under <-
    !is.na( num_values ) &
    ( num_values * num_scaling ) < num_limits[1]
  lgc_over <-
    !is.na( num_values ) &
    ( num_values * num_scaling ) > num_limits[2]
  lgc_in_range <-
    !is.na( num_values ) &
    !lgc_under & !lgc_over

  # If to return indicator for censored data
  if ( lgc_indicator ) {

    int_indicator <- num_output
    int_indicator[lgc_under] <- -1
    int_indicator[lgc_in_range] <- 0
    int_indicator[lgc_over] <- 1

    return(int_indicator)

    # Close 'If to return indicator for censored data'
  } else {

    num_output[lgc_in_range] <- num_values[lgc_in_range]
    num_output[lgc_under] <- num_limits[1]/num_scaling
    num_output[lgc_over] <- num_limits[2]/num_scaling

    return( num_output )

    # Close else for 'If to return indicator for censored data'
  }

}

#### 5) ddabcd_data_merge_by_id_and_time_point ####
#' Add Columns to a Data Frame by Participant ID and Time Point
#'
#' Function to add columns from one data frame to another,
#' matching over participants and time points shared across
#' data sets. Useful, for example, to combine the different
#' data sets provided by the [ABCD study](https://abcdstudy.org/).
#'
#' @param dtf_main A data frame to which columns should be added.
#' @param dtf_to_add A data frame from which columns should be taken.
#' @param chr_participant A character string, the column for
#'   participant IDs to match over. This column must exist in both
#'   \code{dtf_main} and \code{dtf_to_add}.
#' @param chr_time_point An optional character string, the column
#'   for time points to match over. If specified, this column must
#'   exist in both \code{dtf_main} and \code{dtf_to_add}.
#' @param num_default The default value to use for missing data.
#'
#' @return A data frame, \code{dtf_main} with additional columns.
#'
#' @examples
#' # Example data set
#' data(ddabcd_data_example_wide)
#'
#' # Main data set just has ID and time point
#' dtf_main <- ddabcd_data_example_wide[, 1:2]
#' # Want to add columns for demographics (age and sex)
#' dtf_to_add <- ddabcd_data_example_wide[, 1:4]
#' # Merge only by ID (duplicate values over time points)
#' dtf_main <- ddabcd_data_merge_by_id_and_time_point(
#'   dtf_main, dtf_to_add, chr_time_point = ''
#' )
#' head( dtf_main, n = 4 )
#'
#' # Want to add indifference score at delay of 6 hours
#' # by participant and time point
#' dtf_to_add <- ddabcd_data_example_wide[, c( 1:2, 5)]
#' dtf_main <- ddabcd_data_merge_by_id_and_time_point(
#'   dtf_main, dtf_to_add, chr_time_point = 'eventname'
#' )
#' head( dtf_main, n = 4 )
#'
#' @export

ddabcd_data_merge_by_id_and_time_point <- function(
    dtf_main,
    dtf_to_add,
    chr_participant = 'src_subject_id',
    chr_time_point = 'eventname',
    num_default = NA ) {

  # Check that column for participant IDs found (dtf_main)
  if ( !chr_participant %in% colnames(dtf_main) ) {

    chr_error <- paste0(
      "Column specified by 'chr_participant' not found in 'dtf_main'"
    )
    stop( chr_error )

    # Close 'Check that column for participant IDs found (dtf_main)'
  }

  # Check that column for participant IDs found (dtf_to_add)
  if ( !chr_participant %in% colnames(dtf_to_add) ) {

    chr_error <- paste0(
      "Column specified by 'chr_participant' not found in 'dtf_to_add'"
    )
    stop( chr_error )

    # Close 'Check that column for participant IDs found (dtf_to_add)'
  }

  # If column for time points specified
  if ( chr_time_point != '' ) {

    # Check that column for time points found (dtf_main)
    if ( !chr_time_point %in% colnames(dtf_main) ) {

      chr_error <- paste0(
        "Column specified by 'chr_time_point' not found in 'dtf_main'"
      )
      stop( chr_error )

      # Close 'Check that column for time points found (dtf_main)'
    }

    # Check that column for time points found (dtf_to_add)
    if ( !chr_time_point %in% colnames(dtf_to_add) ) {

      chr_error <- paste0(
        "Column specified by 'chr_time_point' not found in 'dtf_to_add'"
      )
      stop( chr_error )

      # Close 'Check that column for time points found (dtf_to_add)'
    }

    # Close 'If column for time points specified'
  }

  # Identify columns to add to 'dtf_main'
  chr_columns_to_add <- colnames(dtf_to_add)[
    !colnames(dtf_to_add) %in% colnames(dtf_main)
  ]
  # If no new columns to add
  if ( length( chr_columns_to_add) == 0 ) {

    stop( "Found no new columns to add to 'dtf_main'" )

    # Close 'If no new columns to add'
  }

  # If column for time points specified
  if ( chr_time_point != '' ) {

    mat_indices <- sapply( 1:nrow(dtf_main), function(r) {

      # Match by participant and time point
      lgc_rows_for_dtf_to_add <-
        dtf_to_add[[ chr_participant ]] %in%
        dtf_main[[ chr_participant ]][r] &
        dtf_to_add[[ chr_time_point ]] %in%
        dtf_main[[ chr_time_point ]][r]

      if ( any( lgc_rows_for_dtf_to_add ) ) {
        int_output <- c(r, which(lgc_rows_for_dtf_to_add)[1] )
      } else {
        int_output <- c(r, NA)
      }

    } )

    # Close 'If column for time points specified'
  } else {

    mat_indices <- sapply( 1:nrow(dtf_main), function(r) {

      # Match by participant
      lgc_rows_for_dtf_to_add <-
        dtf_to_add[[ chr_participant ]] %in%
        dtf_main[[ chr_participant ]][r]

      if ( any( lgc_rows_for_dtf_to_add ) ) {
        int_output <- c(r, which(lgc_rows_for_dtf_to_add)[1] )
      } else {
        int_output <- c(r, NA)
      }

    } )

    # Close else for 'If column for time points specified'
  }

  # If no matches
  if ( all( is.na( mat_indices[2, ] ) ) ) {

    stop( "No matching rows between data frames" )

    # Close 'If no matches'
  }

  mat_indices <- mat_indices[, !is.na(mat_indices[2, ])]

  # Loop over columns to add
  for ( k in seq_along( chr_columns_to_add ) ) {

    vec_values <- rep( num_default, nrow(dtf_main) )
    vec_values[ mat_indices[1, ] ] <-
      dtf_to_add[[ chr_columns_to_add[k] ]][ mat_indices[2, ] ]

    dtf_main[[ chr_columns_to_add[k] ]] <- vec_values

    # Close 'Loop over columns to add'
  }

  return( dtf_main )
}

#### 6) ddabcd_data_default_col ####
#' Default Column Names for ABCD Study Delay Discounting Data
#'
#' Convenience function that returns the default column names
#' used by the ABCD study and the \code{ddabcd} package
#' for delay discounting variables.
#'
#' @param lgc_long Logical; if \code{TRUE} returns
#'  column names for the long-form data set, otherwise
#'  returns column names for the wide-form data set.
#'
#' @return A character vector.
#'
#' @export

ddabcd_data_default_col <- function(
    lgc_long = TRUE ) {

  # Default column names for long-form data set
  if ( lgc_long ) {

    chr_output <- c(
      id = 'src_subject_id',
      tp = 'eventname',
      is100 = "ddabcd_indifference_score_100",
      is1 = "ddabcd_indifference_score_1",
      isLO = "ddabcd_indifference_score_log_odds",
      isC = "ddabcd_indifference_score_censored",
      dlc = "ddabcd_delay_columns",
      dlm = "ddabcd_delay_months",
      dllm = "ddabcd_delay_log_months",
      dlnlm = "ddabcd_delay_negative_log_months",
      qlnm = "ddabcd_quality_no_missing_data",
      qlrc = "ddabcd_quality_picked_rational_immediate_choices",
      qlvr = "ddabcd_quality_variation_in_responses",
      qlJB = "ddabcd_quality_met_Johson_and_Bickel_2008_criteria",
      qlMK = "ddabcd_quality_significant_Mann_Kendall_test"
    )

    # Close 'Default column names for long-form data set'
  } else {

    chr_output <- c(
      id = 'src_subject_id',
      tp = 'eventname',
      isd1 = "ddis_scr_val_indif_point_6h",
      isd2 = "ddis_scr_val_indif_pnt_1da",
      isd3 = "ddis_scr_val_indif_pnt_1week",
      isd4 = "ddis_scr_val_indif_pnt_1mth",
      isd5 = "ddis_scr_val_indif_pnt_3mth",
      isd6 = "ddis_scr_val_indif_pnt_1yr",
      isd7 = "ddis_scr_val_indif_pnt_5yr",
      qlic = "ddis_scr_val_immedcho"
    )

    # Close else for 'Default column names for long-form data set'
  }

  return( chr_output )
}

#### 7) ddabcd_data_delay_durations ####
#' Delay Durations for the ABCD Study Delay Discounting Task
#'
#' Convenience function that returns the 7 delay durations
#' in order used by the ABCD study.
#'
#' @param chr_units A character string, either \code{'months'}
#'   or \code{'log months'}, the units for the delay durations.
#'
#' @return A numeric vector.
#'
#' @export

ddabcd_data_delay_durations <- function(
    chr_units = "log months" ) {

  lst_units <- list(
    months = c(
      "months",
      "Months"
    ),
    log_months = c(
      "log_months",
      "log months",
      "Log months",
      "Log Months",
      "log(months)",
      "log(Months)",
      "Log(months)",
      "Log(Months)",
      "ln(months)",
      "ln(Months)",
      "ln(delay - months)",
      "ln(Delay - months)",
      "ln(delay - Months)",
      "ln(Delay - Months)",
      "log(delay - months)",
      "log(Delay - months)",
      "log(delay - Months)",
      "Log(Delay - months)",
      "Log(delay - Months)",
      "Log(Delay - Months)"
    )
  )

  num_average_days_per_month <- 30.457

  num_delay <- c(
    (6/24) / num_average_days_per_month, # Delay: 6 hours
    1/num_average_days_per_month, # Delay: 1 day
    7/num_average_days_per_month, # Delay: 1 week
    1, # Delay: 1 month
    3, # Delay: 3 months
    12, # Delay: 1 year
    60 # Delay: 5 years
  )

  if ( chr_units %in% lst_units$months ) {
    num_delay <- num_delay
  }

  if ( chr_units %in% lst_units$log_months ) {
    num_delay <- log( num_delay )
  }

  return( num_delay )
}

