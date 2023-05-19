# Tests for model functions
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2023-05-19

# Table of contents
# 1) Setup
# 2) Tests
#   2.1) ddabcd_model_distributions
#   2.2) ddabcd_model_predictions
#   2.3) ddabcd_estimation_priors
#   2.4) ddabcd_estimation_sum_of_log_likelihoods
#   2.5) ddabcd_fit_models_using_nls
#   2.6) ddabcd_fit_models_using_mle2

#### 1) Setup ####

dtf_delay <- ddabcd_data_example_long
num_delay <- c( 1, 3, 12 )

#### 2) Tests ####

#### 2.1) ddabcd_model_distributions ####

test_that("indicator for distribution works (Normal)", {
  expect_equal(
    ddabcd_model_distributions( 'Normal' ),
    c( Normal = TRUE, Beta = FALSE, Logitnormal = FALSE )
  )
})

test_that("indicator for distribution works (Beta)", {
  expect_equal(
    ddabcd_model_distributions( 'Beta' ),
    c( Normal = FALSE, Beta = TRUE, Logitnormal = FALSE )
  )
})

test_that("indicator for distribution works (Logit-normal)", {
  expect_equal(
    ddabcd_model_distributions( 'Logit-normal' ),
    c( Normal = FALSE, Beta = FALSE, Logitnormal = TRUE )
  )
})

#### 2.2) ddabcd_model_predictions ####

test_that("model predictions are correct (Null v1)", {
  expect_equal(
    ddabcd_model_predictions(
      c( b0 = 0.5 ), num_delay
    ),
    rep( 0.5, 3 )
  )
})

test_that("model predictions are correct (Null v2)", {
  expect_equal(
    ddabcd_model_predictions(
      c( a0 = 0 ), num_delay
    ),
    rep( 0.5, 3 )
  )
})

test_that("model predictions are correct (M1987 v1)", {
  expect_equal(
    ddabcd_model_predictions(
      c( k = 0.14 ), num_delay
    ) |> round(3),
    round( 1/( 1 + 0.14*c(1, 3, 12 ) ), 3 )
  )
})

test_that("model predictions are correct (M1987 v2)", {
  expect_equal(
    ddabcd_model_predictions(
      c( lnk = -1.55 ), log( num_delay )
    ) |> round(3),
    round( 1/( 1 + exp( -1.55 + log( c(1, 3, 12 ) ) ) ), 3 )
  )
})

test_that("model predictions are correct (M1987 v3)", {
  expect_equal(
    ddabcd_model_predictions(
      c( nlnk = 1.55 ), -log( num_delay )
    ) |> round(3),
    round( 1/( 1 + exp( -(1.55 - log( num_delay )) ) ), 3 )
  )
})

test_that("model predictions are correct (R2006 v1)", {
  expect_equal(
    ddabcd_model_predictions(
      c( k = 0.14, s = 0.6 ), num_delay
    ) |> round(3),
    round( 1/( 1 + 0.14*( c(1, 3, 12 )^0.6 ) ), 3 )
  )
})

test_that("model predictions are correct (R2006 v2)", {
  expect_equal(
    ddabcd_model_predictions(
      c( lnk = -1.55, s = 0.6 ), log( num_delay )
    ) |> round(3),
    round( 1/( 1 + exp( -1.55 + log( c(1, 3, 12 ) )*0.6 ) ), 3 )
  )
})

test_that("model predictions are correct (R2006 v3)", {
  expect_equal(
    ddabcd_model_predictions(
      c( nlnk = 1.55, s = 0.6 ), -log( num_delay )
    ) |> round(3),
    round( 1/( 1 + exp( -(1.55 - log( num_delay )*0.6) ) ), 3 )
  )
})

test_that("model simulates values correct (Beta)", {
  expect_equal({
    set.seed(230519) # For reproducibility
    vec_p <- rep( NA, 3 )

    # Check for Null model (Logit-normal)
    num_param <- c( a0 = 0, lnd = log(4) )
    mat_sim <- sapply(
      1:1000, function(i) {
        ddabcd_model_predictions(
          num_param,
          num_delay = ddabcd_data_delay_durations(),
          chr_distribution = 'Beta'
        )
      }
    )

    # Should have p > .0167
    int_row <- 1
    lst_ks <- ks.test(
      mat_sim[int_row, ],
      y = pbetamp,
      mean = num_param['a0'] %transform% 'logistic',
      prec = exp( num_param['lnd'] )
    )
    vec_p[1] <- lst_ks$p.value

    # Check for Mazur's (1987) model (Logit-normal)
    num_param <- c( lnk = log(0.14), lnd = log(4) )
    num_pred <- ddabcd_model_predictions(
      num_param[-2],
      num_delay = ddabcd_data_delay_durations()
    )

    mat_sim <- sapply(
      1:1000, function(i) {
        ddabcd_model_predictions(
          num_param,
          num_delay = ddabcd_data_delay_durations(),
          chr_distribution = 'Beta'
        )
      }
    )

    # Should have p > .0167
    # Check middle values to avoid over and underflow issues
    int_row <- 5
    lst_ks <- ks.test(
      unique( mat_sim[int_row, ] ),
      y = pbetamp,
      mean = num_pred[int_row], prec = exp( num_param['lnd'] )
    )
    vec_p[2] <- lst_ks$p.value

    # Check for Rachlin's (2006) model (Logit-normal)
    num_param <- c( lnk = log(0.14), s = 0.8, lnd = log(4) )
    num_pred <- ddabcd_model_predictions(
      num_param[-3],
      num_delay = ddabcd_data_delay_durations()
    )

    mat_sim <- sapply(
      1:1000, function(i) {
        ddabcd_model_predictions(
          num_param,
          num_delay = ddabcd_data_delay_durations(),
          chr_distribution = 'Beta'
        )
      }
    )

    # Should have p > .0167
    # Check middle values to avoid over and underflow issues
    int_row <- 5
    lst_ks <- ks.test(
      unique( mat_sim[int_row, ] ),
      y = pbetamp,
      mean = num_pred[int_row], prec = exp( num_param['lnd'] )
    )
    vec_p[3] <- lst_ks$p.value

    vec_p > .0167
  },
  rep( TRUE, 3 )
  )
})

test_that("model simulates values correct (Logit-normal)", {
  expect_equal({
    set.seed(230519) # For reproducibility
    vec_p <- rep( NA, 5 )

    # Check for Null model (Logit-normal)
    mat_sim <- sapply(
      1:1000, function(i) {
        ddabcd_model_predictions(
          c( a0 = 0, lnd = log(1) ),
          num_delay = ddabcd_data_delay_durations(),
          chr_distribution = 'Logit-normal'
        )
      }
    )

    # Should have p > .01
    lst_ks <- ks.test(
      mat_sim[1, ],
      y = plogitnorm,
      mean = 0, sd = 1
    )
    vec_p[1] <- lst_ks$p.value

    # Check for Mazur's (1987) model (Logit-normal)
    num_pred <- ddabcd_model_predictions(
      c( lnk = log(.14) ),
      num_delay = ddabcd_data_delay_durations()
    )

    mat_sim <- sapply(
      1:1000, function(i) {
        ddabcd_model_predictions(
          c( lnk = log(.14), lnd = log(1) ),
          num_delay = ddabcd_data_delay_durations(),
          chr_distribution = 'Logit-normal'
        )
      }
    )

    # Should have p > .01
    # Check highest value
    lst_ks <- ks.test(
      mat_sim[1, ],
      y = plogitnorm,
      mean = num_pred[1] %transform% 'logit', sd = 1
    )
    vec_p[2] <- lst_ks$p.value
    # Check lowest value
    lst_ks <- ks.test(
      mat_sim[7, ],
      y = plogitnorm,
      mean = num_pred[7] %transform% 'logit', sd = 1
    )
    vec_p[3] <- lst_ks$p.value

    # Check for Rachlin's (2006) model (Logit-normal)
    num_pred <- ddabcd_model_predictions(
      c( lnk = log(.14), s = .8 ),
      num_delay = ddabcd_data_delay_durations()
    )

    mat_sim <- sapply(
      1:1000, function(i) {
        ddabcd_model_predictions(
          c( lnk = log(.14), s = .8, lnd = log(1) ),
          num_delay = ddabcd_data_delay_durations(),
          chr_distribution = 'Logit-normal'
        )
      }
    )

    # Should have p > .01
    # Check highest value
    lst_ks <- ks.test(
      mat_sim[1, ],
      y = plogitnorm,
      mean = num_pred[1] %transform% 'logit', sd = 1
    )
    vec_p[4] <- lst_ks$p.value
    # Check lowest value
    lst_ks <- ks.test(
      mat_sim[7, ],
      y = plogitnorm,
      mean = num_pred[7] %transform% 'logit', sd = 1
    )
    vec_p[5] <- lst_ks$p.value

    vec_p > .01
  },
  rep( TRUE, 5 )
  )
})


#### 2.3) ddabcd_estimation_priors ####

test_that("function returns log-likelihood", {
  expect_equal(
    ddabcd_estimation_priors(
      c( a0 = 0 ),
      lst_priors = list(
        a0 = c( 0, 1 )
      )
    ) %>% round(3),
    dnorm( 0, 0, 1, log = TRUE ) |> round(3)
  )
})

#### 2.4) ddabcd_estimation_sum_of_log_likelihoods ####

test_that("function returns correct sum of log-likelihoods", {
  expect_equal(
    ddabcd_estimation_sum_of_log_likelihoods(
      c( a0 = 0, lnd = 0 ),
      dtf_data = data.frame(
        indifference = c( .5, .5, .5 ),
        delay = c( 1, 3, 12 )
      ),
      chr_measures = c(
        'indifference',
        'delay'
      ),
      chr_distribution = 'Logit-normal'
    ) |> round(4),
    -sum( dlogitnorm( rep( .5, 3), rep( 0, 3), 1, log = TRUE ) ) |>
      round(4)
  )
})

test_that("function incorporates priors correctly", {
  expect_equal(
    ddabcd_estimation_sum_of_log_likelihoods(
      c( a0 = 0, lnd = 0 ),
      dtf_data = data.frame(
        indifference = c( .5, .5, .5 ),
        delay = c( 1, 3, 12 )
      ),
      chr_measures = c(
        'indifference',
        'delay'
      ),
      chr_distribution = 'Logit-normal',
      lst_priors = list(
        a0 = c( 0, 1 ),
        lnd = c( 0, 1 )
      ),
      fun_priors = ddabcd_estimation_priors
    ) |> round(4),
    -( sum( dlogitnorm( rep( .5, 3), rep( 0, 3), 1, log = TRUE ) ) +
        dnorm( 0, 0, 1, log = TRUE ) +
        dnorm( 0, 0, 1, log = TRUE ) ) |> round(4)
  )
})

#### 2.5) ddabcd_fit_models_using_nls ####

test_that("models fit successfully using nls", {
  expect_equal(
    ddabcd_fit_models_using_nls(
      dtf_delay[1:7, ]
    ) |> sapply( function(l) !is.null(l) ) |> all(),
    TRUE
  )
})

#### 2.6) ddabcd_fit_models_using_mle2 ####

test_that("models fit successfully using mle2 (Normal)", {
  expect_equal(
    ddabcd_fit_models_using_mle2(
      dtf_delay[1:7, ],
      chr_distribution = 'Normal'
    ) |> sapply( function(l) !is.null(l) ) |> all(),
    TRUE
  )
})


