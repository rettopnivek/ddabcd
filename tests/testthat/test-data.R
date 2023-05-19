# Tests for data functions
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2023-05-19

# Table of contents
# 1) Setup
# 2) Tests
#  2.1) ddabcd_data_prep
#  2.2) ddabcd_data_censor
#  2.3) ddabcd_data_merge_by_id_and_time_point

#### 1) Setup ####

dtf_wide <- ddabcd_data_example_wide

dtf_main <- dtf_wide[1:4, 1:2]
dtf_to_add <- dtf_wide[c(1, 3), 1:4]
dtf_to_add_v2 <- dtf_wide[1:4, c(1:2, 5)]
dtf_main_v2 <- dtf_main
colnames(dtf_main_v2) <- c( 'IDS.CHR.Subject',
                            'SSS.CHR.Time_point' )
dtf_to_add_v3 <- dtf_to_add_v2
colnames(dtf_to_add_v3) <- c( 'IDS.CHR.Subject',
                              'SSS.CHR.Time_point',
                              'OUT.DBL.Delay_of_6_hours' )

#### 2) Tests ####

#### 1.1) ddabcd_data_prep ####

test_that("function returns default columns", {
  expect_equal(
    ddabcd_data_prep( NULL ),
    c(
      "ddabcd_indifference_score_100",
      "ddabcd_indifference_score_1",
      "ddabcd_indifference_score_log_odds",
      "ddabcd_indifference_score_censored",
      "ddabcd_delay_columns",
      "ddabcd_delay_months",
      "ddabcd_delay_log_months",
      "ddabcd_delay_negative_log_months",
      "ddabcd_quality_no_missing_data",
      "ddabcd_quality_picked_rational_immediate_choices",
      "ddabcd_quality_variation_in_responses",
      "ddabcd_quality_met_Johson_and_Bickel_2008_criteria",
      "ddabcd_quality_significant_Mann_Kendall_test"
    )
  )
})

test_that("function converts to long form", {
  expect_equal(
    ddabcd_data_prep( dtf_wide[c(1, 3), ] )[, 'src_subject_id'],
    rep( dtf_wide$src_subject_id[c(1, 3)], each = 7)
  )
})

test_that("function adds quality checks", {
  expect_equal(
    ddabcd_data_prep( dtf_wide[95, ] )[1, c(
      'ddabcd_quality_no_missing_data',
      'ddabcd_quality_picked_rational_immediate_choices',
      'ddabcd_quality_variation_in_responses',
      'ddabcd_quality_met_Johson_and_Bickel_2008_criteria',
      'ddabcd_quality_significant_Mann_Kendall_test'
    )] |> unlist(),
    c(
      ddabcd_quality_no_missing_data = TRUE,
      ddabcd_quality_picked_rational_immediate_choices = FALSE,
      ddabcd_quality_variation_in_responses = FALSE,
      ddabcd_quality_met_Johson_and_Bickel_2008_criteria = FALSE,
      ddabcd_quality_significant_Mann_Kendall_test = FALSE
    )
  )
})

#### 1.2) ddabcd_data_censor ####

test_that("function censors data correctly", {
  expect_equal(
    ddabcd_data_censor(
      data.frame(
        ddabcd_indifference_score_1 = c(
          NA, 0, 0.5, 1
        )
      )
    ),
    c( NA, 0.78125/100, 0.5, 99.21875/100 )
  )
})

#### 1.3) ddabcd_data_merge_by_id_and_time_point ####

test_that("function adds columns and propagates correctly", {
  expect_equal(
    ddabcd_data_merge_by_id_and_time_point(
      dtf_main,
      dtf_to_add,
      chr_time_point = ''
    ),
    cbind( dtf_main, dtf_to_add[c(1, 1, 2, 2), -(1:2)] )
  )
})

test_that("function adds columns correctly", {
  expect_equal(
    ddabcd_data_merge_by_id_and_time_point(
      dtf_main,
      dtf_to_add_v2
    ),
    cbind( dtf_main, ddis_scr_val_indif_point_6h = dtf_to_add_v2[, 3] )
  )
})

test_that("function uses custom names correctly", {
  expect_equal(
    ddabcd_data_merge_by_id_and_time_point(
      dtf_main_v2,
      dtf_to_add_v3,
      chr_participant = 'IDS.CHR.Subject',
      chr_time_point = 'SSS.CHR.Time_point'
    ),
    cbind( dtf_main_v2,
           OUT.DBL.Delay_of_6_hours = dtf_to_add_v3[, 3] )
  )
})

