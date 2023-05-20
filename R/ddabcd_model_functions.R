# Modeling functions
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2023-05-20

# Table of contents
# 1) Probability distribution functions
#   1.1) betamp functions
#     1.1.1) dbetamp
#     1.1.2) pbetamp
#     1.1.3) qbetamp
#     1.1.4) rbetamp
#   1.2) logitnorm functions
#     1.2.1) dlogitnorm
#     1.2.2) plogitnorm
#     1.2.3) qlogitnorm
#     1.2.4) rlogitnorm
# 2) Model functions
#   2.1) ddabcd_model_distributions
#   2.1) ddabcd_model_predictions
# 3) Utility functions for estimation
#   3.1) ddabcd_estimation_transform
#   3.2) ddabcd_estimation_priors
#   3.3) ddabcd_estimation_sum_of_log_likelihoods
#   3.4) ddabcd_estimation_sum_of_log_tobit_likelihoods
#   3.5) ddabcd_estimation_succeeded
# 4) Model fit functions
#   4.1) ddabcd_fit_models_using_nls
#   4.2) ddabcd_fit_models_using_mle2
#   4.3) ddabcd_fit_models_using_tl_mle2
#   4.4) ddabcd_fit_models_across_participants
# 5) Post-estimation functions
#   5.1) Custom operators for extracting elements
#     5.1.1) `%pull_coef%`
#     5.1.2) `%index%`
#     5.1.3) `%col%`
#   5.2) Additional measures for model fit
#     5.2.1) ddabcd_post_fit_compute_rmse
#     5.2.2) ddabcd_post_fit_report
#     5.2.3) ddabcd_post_fit_sample_parameters
#   5.?) ddabcd_post_fit_bootstrapped_prediction_interval_mle2
#   5.3) Tools for extracting/incorporating results
#     5.3.1) ddabcd_post_fit_extract
#     5.3.2) ddabcd_post_fit_attr
#     5.3.3) ddabcd_post_fit_if_success
#     5.3.4) ddabcd_post_fit_add_to_data
#     5.3.5) ddabcd_post_fit_convergence

#### 1) Probability distribution functions ####

#### 1.1) betamp functions ####

#### 1.1.1) dbetamp ####
#' The Beta Distribution Using Mean and Precision
#'
#' Density, distribution function, quantile function, and random
#' generation for the beta distribution with mean equal to
#' \code{mean} and precision equal to \code{prec} (lower values
#' correspond to higher variance).
#'
#' @param x,q Vector of quantiles
#'   (\code{0 >=} \code{x},\code{q} \code{<= 1}).
#' @param p Vector of probabilities
#' @param n Number of observations. If \code{length(n) > 1}, the
#'   length is taken to be the number required.
#' @param mean Vector of means for the beta distribution, where
#'   \code{0 > mean > 1}.
#' @param prec Vector of dispersion parameters for the beta
#'   distribution, where \code{prec > 0}.
#' @param log,log.p Logical; if \code{TRUE}, probabilities p
#'   are given as log(p).
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities
#'   are P(X less than or equal to x) otherwise, P(X greater than x).
#'
#' @details
#' The mean and precision for the beta distribution can be converted
#' back to the standard positive shape parameters via
#' \code{shape1 = mean x prec} and \code{shape2 = (1-mean) x prec}.
#'
#' See [stats::dbeta] for more details.
#'
#' @return \code{dbetamp} gives the density, \code{pbetamp} gives the
#' distribution function, \code{qbetamp} gives the quantile function,
#' and \code{rbetamp} generates random deviates. The length of the
#' result is determined by \code{n} for \code{rbetamp}, and is the
#' maximum of the lengths of the numerical arguments for the other
#' functions.
#'
#' @examples
#' x <- c(.25, .5, .75)
#' dbetamp( x, .5, 4)
#' # Same as 'dbeta' function
#' dbetamp( x, .5, 4) == dbeta( x, 2, 2)
#' pbetamp( x, .5, 4)
#' qbetamp( pbetamp( x, .5, 4), .5, 4)
#' rbeta( 3, .5, 4)
#'
#' @export

dbetamp <- function(x, mean, prec, log = FALSE) {

  shape1 <- mean*prec
  shape2 <- (1-mean)*prec

  return(
    dbeta(
      x, shape1, shape2, log = log
    )
  )

}

#### 1.1.2) pbetamp ####
#' @rdname dbetamp
#' @export

pbetamp <- function(q, mean, prec, lower.tail = TRUE, log.p = FALSE) {

  shape1 <- mean*prec
  shape2 <- (1-mean)*prec

  return(
    pbeta(
      q, shape1, shape2,
      lower.tail = lower.tail, log.p = log.p
    )
  )

}

#### 1.1.3) qbetamp ####
#' @rdname dbetamp
#' @export

qbetamp <- function(p, mean, prec, lower.tail = TRUE, log.p = FALSE) {

  shape1 <- mean*prec
  shape2 <- (1-mean)*prec

  return(
    qbeta(
      p, shape1, shape2,
      lower.tail = lower.tail, log.p = log.p
    )
  )

}

#### 1.1.4) rbetamp ####
#' @rdname dbetamp
#' @export

rbetamp <- function(n, mean, prec) {

  shape1 <- mean*prec
  shape2 <- (1-mean)*prec

  return(
    rbeta(
      n, shape1, shape2
    )
  )

}

#### 1.2) logitnorm functions ####

#### 1.2.1) dlogitnorm ####
#' The Logit-Normal Distribution
#'
#' Density, distribution function, quantile function, and random
#' generation for the logit-normal distribution with mean on the
#' logit-scale equal to \code{mean} and standard deviation on on
#' the logit-scale equal to \code{sd}.
#'
#' @param x,q Vector of quantiles
#'   (\code{0 >=} \code{x},\code{q} \code{<= 1}).
#' @param p Vector of probabilities
#' @param n Number of observations. If \code{length(n) > 1}, the
#'   length is taken to be the number required.
#' @param mean Vector of means on the logit-scale.
#' @param prec Vector of standard deviations on the logit-scale,
#'   where \code{sd > 0}.
#' @param log,log.p Logical; if \code{TRUE}, probabilities p
#'   are given as log(p).
#' @param lower.tail Logical; if \code{TRUE} (default), probabilities
#'   are P(X less than or equal to x) otherwise, P(X greater than x).
#'
#' @details
#' If \code{mean} or \code{sd} are not specified they assume the
#' default values of 0 and 1.
#'
#' See [stats::dnorm] for more details.
#'
#' @return \code{dlogitnorm} gives the density, \code{plogitnorm} gives the
#' distribution function, \code{qlogitnorm} gives the quantile function,
#' and \code{rlogitnorm} generates random deviates. The length of the
#' result is determined by \code{n} for \code{rbetamp}, and is the
#' maximum of the lengths of the numerical arguments for the other
#' functions.
#'
#' @examples
#' x <- c(.25, .5, .75)
#' dlogitnorm( x, 0, 1)
#' plogitnorm( x, 0, 1)
#' qlogitnorm( plogitnorm( x, 0, 1), 0, 1)
#' rlogitnorm( 3, 0, 1)
#'
#' @export

dlogitnorm <- function(x, mean, sd, log = FALSE) {

  lx <- log( x/(1 - x) )

  return(
    dnorm(
      lx, mean = mean, sd = sd, log = log
    )
  )

}

#### 1.2.2) plogitnorm ####
#' @rdname dlogitnorm
#' @export

plogitnorm <- function(q, mean, sd, lower.tail = TRUE, log.p = FALSE) {

  lq <- log( q/(1 - q) )

  return(
    pnorm(
      lq, mean = mean, sd = sd,
      lower.tail = lower.tail, log.p = log.p
    )
  )

}

#### 1.2.3) qlogitnorm ####
#' @rdname dlogitnorm
#' @export

qlogitnorm <- function(p, mean, sd, lower.tail = TRUE, log.p = FALSE) {

  return(
    1/(1 + exp(
      -qnorm(
        p, mean = mean, sd = sd,
        lower.tail = lower.tail, log.p = log.p
      )
    ) )
  )

}

#### 1.2.4) rlogitnorm ####
#' @rdname dlogitnorm
#' @export

rlogitnorm <- function(n, mean, sd) {

  return(
    1/(1 + exp(
      -rnorm(
        n, mean = mean, sd = sd
      )
    ) )
  )

}

#### 2) Model functions ####

#### 2.1) ddabcd_model_distributions ####
#' Determine Model Distribution
#'
#' Function that returns a logical vector
#' indicating whether a normal, beta, or
#' logit-normal distribution should be used
#' for modeling purposes. Primarily for
#' internal use with estimation functions.
#'
#' @param chr_distribution A character string,
#'   the name of the distribution to use.
#'
#' @return A named logical vector with the
#' value for the corresponding distribution
#' equal to \code{TRUE}.
#'
#' @examples
#' ddabcd_model_distributions('Beta')
#'
#' @export

ddabcd_model_distributions <- function(
    chr_distribution ) {

  lst_distribution <- list(
    Normal = c(
      'Normal',
      'normal',
      'dnorm',
      'Gaussian',
      'gaussian'
    ),
    Beta = c(
      'Beta',
      'beta',
      'dbetamp'
    ),
    Logitnormal = c(
      'Logitnormal',
      'logitnormal',
      'Logit-normal',
      'logit-normal',
      'dlogitnorm'
    )
  )

  lgc_match <- sapply( 1:3, function(i) {
    chr_distribution[1] %in% lst_distribution[[i]]
  } )
  names( lgc_match ) <- c( 'Normal', 'Beta', 'Logitnormal' )

  if ( !any(lgc_match) ) {
    stop(
      "Check specification of 'chr_distribution' argument; ",
      "Options are 'Normal', 'Beta', or 'Logit-normal'."
    )
  }

  return( lgc_match )
}


#### 2.1) ddabcd_model_predictions ####
#' Predictions and Simulations From Delay Discounting Models
#'
#' Generates predicted or simulated indifference scores for
#' different delay discounting models. Supported models are
#' 1) a intercept-only model, 2) Mazur's (1987) model, and
#' 3) Rachlin's (2006) model.
#'
#' @param num_param A named numeric vector of parameter values.
#'   Parameter names can be \code{b0} for the intercept-only
#'   model, \code{a0} for the intercept-only model (logit-scale),
#'   \code{k} for the discounting parameter, \code{lnk} for the
#'   log discounting parameter, \code{nlnk} for the negative
#'   log discounting parameter, \code{s} for the sensitivity
#'   parameter (Rachlin, 2006), \code{d} for the dispersion
#'   parameter, and \code{lnd} for the log of the dispersion
#'   parameter. If \code{d} or \code{lnd} are provided,
#'   function generates random deviates from the model, otherwise
#'   it produces model predictions. The interpretation of
#'   \code{d} and \code{lnd} depend on the specification of
#'   \code{chr_error}.
#' @param num_delay A numeric vector of delay durations
#'   (for \code{num_param['k']}), log delay durations
#'   (for \code{num_param['lnk']}), or minus log durations
#'   (for \code{num_param['nlnk']}).
#' @param chr_error The distribution for the dispersion around
#'   model predictions. Options are \code{'Normal'}, \code{'Beta'},
#'   or \code{'Logit-normal'}.
#' @param lgc_logit Logical; if \code{TRUE}
#' @param num_scaling A numeric value, rescales results if
#'   \code{lgc_logit} is \code{FALSE}.
#'
#' @details
#' For an indifference score I scaled to be between (0 - 1),
#' Mazur's (1987) model is I = 1/(1 + k*delay). Rachlin's (2006)
#' model is I = 1/(1 + k*(delay^s)). Mazur's model therefore is
#' a special case of Rachlin's when s = 1. The models can be
#' rewritten as a linear equation on the logit-scale:
#' logit(I) = -ln(k) - ln(delay)*s.
#'
#' @references
#' Mazur, J. E. (1987). An adjusting procedure for studying
#' delayed reinforcement. In M. L. Commons, J. E. Mazur,
#' J. A. Nevin, & H. Rachlin (Eds.), The effect of delay and
#' intervening events on reinforcement value (pp. 55-73).
#' Lawrence Erlbaum Associates, Inc.
#'
#' Rachlin, H. (2006). Notes on discounting. Journal of the
#' Experimental Analysis of Behavior, 85 (3), 425-435.
#' https://doi.org/10.1901/jeab.2006.85-05
#'
#' @returns A vector of predicted or simulated indifference scores.
#'
#' @examples
#' # Delay duration in months
#' x <- c( 1, 3, 12 )
#'
#' # Mazur's (1987) model
#' k <- 0.14
#' # Predictions given different parameterizations
#' ddabcd_model_predictions( c( k = k ), x )
#' ddabcd_model_predictions( c( lnk = log(k) ), log(x) )
#' ddabcd_model_predictions( c( nlnk = -log(k) ), -log(x) )
#'
#' # Rachlin's (2006) model
#' s = .60
#' ddabcd_model_predictions( c( k = k, s = s ), x )
#' ddabcd_model_predictions( c( lnk = log(k), s = s ), log(x) )
#' ddabcd_model_predictions( c( nlnk = -log(k), s = s ), -log(x) )
#'
#' # Random deviates
#' # Normal model
#' ddabcd_model_predictions(
#'   c( k = k, d = .3 ), x, chr_distribution = 'Normal'
#' )
#' # Beta model
#' ddabcd_model_predictions(
#'   c( k = k, d = 4 ), x, chr_distribution = 'Beta'
#' )
#' # Logit-normal model
#' ddabcd_model_predictions(
#'   c( k = k, d = 0.3 ), x, chr_distribution = 'Logit-normal'
#' )
#'
#' @export

ddabcd_model_predictions <- function(
    num_param,
    num_delay,
    chr_distribution = 'Normal',
    lgc_logit = FALSE,
    num_scaling = 1) {

  # Check named inputs for parameters
  chr_parameter_names <- c(
    'k', 'lnk', 'nlnk',
    's',
    'b0',
    'a0',
    'd', 'lnd'

  )

  # If correct inputs not found
  if ( !all( names(num_param) %in% chr_parameter_names ) ) {

    chr_error <- paste0(
      "Argument 'num_param' must be a named numeric vector where ",
      "num_param['k'] = delayed discounting parameter, ",
      "num_param['lnk'] = log of the delayed discounting parameter, ",
      "num_param['nlnk'] = minus log of the delayed discounting parameter, ",
      "num_param['b0'] = intercept for the null model, ",
      "num_param['a0'] = intercept for the null model on the logit-scale, ",
      "num_param['d'] = dispersion parameter for residuals, and ",
      "num_param['lnd'] = log of the dispersion parameter for residuals."
    )

    stop(
      chr_error
    )

    # Close 'If correct inputs not found'
  }

  # Initialize output for predictions
  num_predicted <- rep( NA, length(num_delay) )
  chr_param_names <- names(num_param)
  int_obs <- length(num_delay)

  if ( any( chr_param_names %in% c( 'b0', 'a0' ) ) ) {
    chr_model <- 'Null'
  }

  if ( any( chr_param_names %in% c( 'k', 'lnk', 'nlnk' ) ) ) {
    chr_model <- 'M1987'
  }

  if ( any( chr_param_names %in% c( 's' ) ) ) {
    chr_model <- 'R2006'
  }

  # Null model (Intercept only)
  if ( chr_model %in% 'Null' ) {

    # Intercept-only
    if ('b0' %in% chr_param_names) {

      num_predicted <-
        rep( num_param['b0'], int_obs )

      # Close 'Intercept-only'
    }

    # Intercept-only on the logit-scale
    if ('a0' %in% chr_param_names) {

      num_predicted <-
        1/( 1 + exp(-rep( num_param['a0'], int_obs )) )

      # Close 'Intercept-only on the logit-scale'
    }

    names( num_predicted ) <- NULL

    if ( lgc_logit ) {
      num_predicted <- log( num_predicted / (1 - num_predicted ) )
    }

    # Close 'Null model (Intercept only)'
  }


  # Model of Mazur (1987)
  if ( chr_model %in% 'M1987' ) {

    # Standard parameterization
    if ( 'k' %in% chr_param_names ) {

      num_predicted <-
        1/( 1 + num_param['k']*num_delay )

      if ( lgc_logit ) {
        num_predicted <- log( num_predicted / (1 - num_predicted ) )
      }

      # Close 'Standard parameterization'
    }

    # Log discounting and log delay
    if ( 'lnk' %in% names(num_param) ) {

      num_predicted <-
        1/( 1 + exp(num_param['lnk'] + num_delay) )

      if ( lgc_logit ) {
        num_predicted <- log( num_predicted / (1 - num_predicted ) )
      }

      # Close 'Log discounting and log delay'
    }

    # Minus log discounting and minus log delay
    if ( 'nlnk' %in% names(num_param) ) {

      num_predicted <-
        num_param['nlnk'] + num_delay

      if ( !lgc_logit ) {
        num_predicted <- 1/(1 + exp(-num_predicted) )
      }

      # Close 'Minus log discounting and minus log delay'
    }

    # Close 'Model of Mazur (1987)'
  }

  # Model of Rachlin (2006)
  if ( chr_model %in% 'R2006' ) {

    # Standard parameterization
    if ( 'k' %in% names(num_param) ) {

      num_predicted <-
        1/( 1 + num_param['k']*( num_delay^num_param['s'] ) )

      if ( lgc_logit ) {
        num_predicted <- log( num_predicted / (1 - num_predicted ) )
      }

    }

    # Log discounting and log delay
    if ( 'lnk' %in% names(num_param) ) {

      num_predicted <-
        1/( 1 + exp( num_param['lnk'] + (num_delay*num_param['s']) ) )

      if ( lgc_logit ) {
        num_predicted <- log( num_predicted / (1 - num_predicted ) )
      }

      # Close 'Log discounting and log delay'
    }

    # Minus log discounting and minus log delay
    if ( 'nlnk' %in% names(num_param) ) {

      num_predicted <- num_param['nlnk'] + num_delay*num_param['s']

      if ( !lgc_logit ) {
        num_predicted <- 1/(1 + exp(-num_predicted) )
      }

      # Close 'Minus log discounting and minus log delay'
    }

    # Close 'Model of Rachlin (2006)'
  }

  # Simulate from model
  if ( any( c( 'd', 'lnd' ) %in% names( num_param ) ) ) {

    lgc_distribution <- ddabcd_model_distributions(
      chr_distribution
    )

    num_simulated <- rep( NA, length( num_predicted ) )

    # If log dispersion provided
    if ( 'lnd' %in% names(num_param) ) {

      num_disp <- exp( num_param['lnd'] )

      # Close 'If log dispersion provided'
    } else {

      num_disp <- num_param['d']

      # Close else for 'If log dispersion provided'
    }

    # Data follow a normal distribution
    if (lgc_distribution['Normal']) {

      if (lgc_logit) {
        num_predicted <- 1/(1 + exp(-num_predicted))
      }

      num_simulated <- rnorm(
        length(num_predicted),
        mean = num_predicted,
        sd = num_disp
      )

      # Close 'Data follow a normal distribution'
    }

    # Data follows a beta distribution
    if (lgc_distribution['Beta']) {

      if (lgc_logit) {
        num_predicted <- 1/(1 + exp(-num_predicted))
      }

      num_simulated <- rbetamp(
        length(num_predicted),
        mean = num_predicted,
        prec = num_disp
      )

      # Close 'Data follow a beta distribution'
    }

    # Data follow a logit-normal distribution
    if (lgc_distribution['Logitnormal']) {

      if ( !lgc_logit ) {
        num_predicted <- log(num_predicted/(1 - num_predicted))
      }

      num_simulated <- rlogitnorm(
        length(num_predicted),
        mean = num_predicted,
        sd = num_disp
      )

      # Close 'Data follow a logit-normal distribution'
    }

    # Log-odds transform
    if (lgc_logit) {

      return( log( num_simulated/(1 - num_simulated) ) )

      # Close 'Log-odds transform'
    } else {

      num_simulated <- num_scaling * num_simulated

      return( num_simulated )

      # Close else for 'Log-odds transform'
    }

    # Close 'Simulate from model'
  } else {

    # Rescale predictions
    if (!lgc_logit) {
      num_predicted <- num_scaling * num_predicted
    }

    return( num_predicted )

    # Close else for 'Simulate from model'
  }

}

#### 3) Utility functions for estimation ####

#### 3.1) `%transform%` ####
#' Apply Transformations to Vector of Values
#'
#' Operator to apply a specified transformation to
#' a vector of numeric values, such as the logit
#' or logistic transform.
#'
#' @param num_values A numeric vector.
#' @param chr_transform A character string, the
#'   transform to apply. Options include
#'   \code{'logit'}, \code{'logistic'},
#'   \code{'probit'}, or \code{'inverse-probit'}.
#'
#' @return A numeric vector of transformed values.
#'
#' @examples
#' c(.25, .5, .75) %transform% 'logit' |> round(3)
#' c(-1.099, 0, 1.099) %transform% 'logistic' |> round(3)
#'
#' @export

`%transform%` <- function(
    num_values,
    chr_transform ) {

  # Initialize output
  num_output <- rep( NA, length(num_values) )

  if ( chr_transform == 'logit' ) {
    return( log(num_values/(1-num_values)) )
  }

  if ( chr_transform == 'logistic' ) {
    return( 1/(1 + exp(-num_values)) )
  }

  if ( chr_transform == 'probit' ) {
    return( pnorm(num_values) )
  }

  if ( chr_transform == 'inverse-probit' ) {
    return( qnorm(num_values) )
  }

  if ( all( is.na( num_output ) ) ) {
    "Check specification of 'chr_transform' argument"
  }

}

#### 3.2) ddabcd_estimation_priors ####
#' Compute Priors Over Delay Discounting Model Parameters
#'
#' Function to compute priors over delay
#' discounting model parameters.
#'
#' @param num_param A numeric named vector of parameter values,
#'   where \code{num_param['a0']} is the intercept only (logit-scale),
#'   \code{num_param['lnk']} is for log discounting,
#'   \code{num_param['s']} is for sensitivity, and
#'   \code{num_param['lnd']} is log dispersion.
#' @param lst_priors A named list of vectors specifying the
#'   mean and standard deviation, respectively, for the
#'   prior on each parameter.
#' @param chr_distribution A character string specifying
#'   the distribution for the data. Options are
#'   \code{'Normal'}, \code{'Beta'}, or \code{'Logit-normal'}.
#'
#' @details
#' The following parameters are given normal priors with
#' user-specified means and standard deviations:
#' \itemize{
#'   \item{"a0"}{intercept only on logit-scale}
#'   \item{"lnk"}{log discounting}
#'   \item{"s"}{sensitivity}
#'   \item{"lnd"}{log dispersion}
#' }
#'
#' The interpretation of the log dispersion parameter
#' depends on the distribution for the data. For
#' normal and logit-normal distributions lower values
#' indicate lower variance, whereas for the beta
#' distribution, higher values indicate lower variance.
#'
#' @return A numeric value, a sum of log-likelihoods.
#'
#' @examples
#' # Example normal prior with mean of 0 and
#' # sd of 1 on intercept on logit-scale
#' ddabcd_estimation_priors(
#'   c( a0 = 0 ), lst_priors = list( a0 = c( 0, 1 ) )
#' )
#'
#' @export

ddabcd_estimation_priors <- function(
    num_param,
    lst_priors,
    chr_distribution ) {

  # Default priors
  if ( is.null( lst_priors ) ) {

    lst_priors <- list(
      a0 = c( 0, 2.5 ),
      lnk = c( -1.55, 2.5 ),
      s = c( 0.6, 0.3 )
    )

    lgc_distribution <- ddabcd_model_distributions(
      chr_distribution
    )

    if ( lgc_distribution['Normal'] ) {
      lst_priors$lnd <- c( 0.3, 0.3 )
    }

    if ( lgc_distribution['Beta'] ) {
      lst_priors$lnd <- c( log(4), 1 )
    }

    if ( lgc_distribution['Logitnormal'] ) {
      lst_priors$lnd <- c( 1, 1 )
    }

    # Close 'Default priors'
  }

  num_priors <- rep( 0, length(num_param) )
  chr_param <- names(num_param)
  names(num_priors) <- chr_param

  if ( 'a0' %in% chr_param ) {
    num_priors['a0'] <- dnorm(
      num_param['a0'], lst_priors$a0[1], lst_priors$a0[2], log = TRUE
    )
  }

  if ( 'lnk' %in% chr_param ) {
    num_priors['lnk'] <- dnorm(
      num_param['lnk'], lst_priors$lnk[1], lst_priors$lnk[2], log = TRUE
    )
  }

  if ( 's' %in% chr_param ) {
    num_priors['s'] <- dnorm(
      num_param['s'], lst_priors$s[1], lst_priors$s[2], log = TRUE
    )
  }

  if ( 'lnd' %in% chr_param ) {
    num_priors['lnd'] <- dnorm(
      num_param['lnd'], lst_priors$lnd[1], lst_priors$lnd[2], log = TRUE
    )
  }

  return( sum(num_priors) )
}


#### 3.3) ddabcd_estimation_sum_of_log_likelihoods ####
#' Compute Sum of Log Likelihoods for Delay Discounting Models
#'
#' Function to compute the sum of the log-likelihoods for
#' one of three delay discounting models: 1) an intercept-only
#' model, 2) Mazur's (1987) model, or 3) Rachlin's (2006) model.
#' Can be passed to optimization routines like
#' [stats::optim] or [bbmle::mle2] to conduct maximum likelihood
#' estimation.
#'
#' @param num_param A numeric named vector of parameter values,
#'   where \code{num_param['a0']} is the intercept only (logit-scale),
#'   \code{num_param['lnk']} is for log discounting,
#'   \code{num_param['s']} is for sensitivity, and
#'   \code{num_param['lnd']} is log dispersion.
#' @param dtf_data A data frame with the indifference scores
#'   (ranging from 0 to 1) and the log of the delay duration.
#' @param chr_distribution A character string, the type of
#'   probability distribution the data are assumed to follow.
#'   Options include \code{'Normal'}, \code{'Beta'}, or
#'   \code{'Logit-normal'}.
#' @param chr_measures A character vector giving the
#'   column names in \code{dtf_data} for 1) the
#'   indifference scores, and 2) the log delays.
#' @param num_fnscale An numeric value of either 1 or -1
#'   adjusting the sum of the log-likelihoods (by default
#'   is -1 to ensure compatibility with functions like
#'   [bbmle::mle2]).
#' @param lst_priors An optional named list with the
#'   hyperparameters for the priors on each parameter.
#' @param fun_prior An optional function that takes the
#'   vector of parameters and a list of hyperparameters
#'   to compute a prior on the parameters. Allows for
#'   penalized maximum likelihood estimation
#'   (see [ddabcd::ddabcd_estimation_priors]).
#'
#' @return A numeric value, the sum of the log-likelihoods
#' adjusted by \code{num_fnscale}.
#'
#' @examples
#' # Example data set
#' data(ddabcd_data_example_long)
#' # Take data from 1st participant
#' dtf_delay <- ddabcd_data_example_long[1:7, ]
#'
#' # Intercept-only model
#' # Data follow a normal distribution
#' num_param <- c( a0 = 2.2, lnd = log(.3) )
#' ddabcd_estimation_sum_of_log_likelihoods(
#'   num_param, dtf_delay, 'Normal'
#' )
#'
#' # Mazur's (1987) model
#' # Data follow a beta distribution
#' num_param <- c( lnk = log(0.14), lnd = log(4) )
#' ddabcd_estimation_sum_of_log_likelihoods(
#'   num_param, dtf_delay, 'Beta'
#' )
#'
#' # Rachlin's (2006) model
#' # Data follow a logit-normal distribution
#' num_param <- c( lnk = log(0.14), s = 0.6, lnd = log(4) )
#' ddabcd_estimation_sum_of_log_likelihoods(
#'   num_param, dtf_delay, 'Logit-normal'
#' )
#'
#' @export

ddabcd_estimation_sum_of_log_likelihoods <- function(
    num_param,
    dtf_data,
    chr_distribution,
    chr_measures = c(
      'ddabcd_indifference_score_1',
      'ddabcd_delay_log_months'
    ),
    num_fnscale = -1,
    lst_priors = NULL,
    fun_priors = NULL ) {

  # Check if columns in data frame
  if ( !all( chr_measures %in% colnames(dtf_data) ) ) {

    chr_error <- paste0(
      "Columns specified in 'chr_measures' not ",
      "found in 'dtf_data'."
    )

    stop(chr_measures)

    # Close 'Check if columns in data frame'
  }

  num_observed <- dtf_data[[ chr_measures[1] ]]
  num_delay <- dtf_data[[ chr_measures[2] ]]

  # Check for correct parameter names
  if ( !all( names( num_param ) %in% c( 'a0', 'lnk', 's', 'lnd' ) ) ) {

    chr_error <- paste0(
      "Argument 'num_param' must be a named vector where ",
      "num_param['a0'] = intercept only (logit-scale), ",
      "num_param['lnk'] = log discounting, ",
      "num_param['s'] = sensitivity, and ",
      "num_param['lnd'] = log dispersion."
    )

    stop( chr_error )

    # Close 'Check for correct parameter names'
  }

  num_predicted <- ddabcd::ddabcd_model_predictions(
    num_param[ names(num_param) != 'lnd' ],
    num_delay = num_delay,
    lgc_logit = FALSE
  )

  num_disp <- exp( num_param['lnd'] )
  lgc_distribution <- ddabcd_model_distributions(
    chr_distribution
  )

  if ( lgc_distribution['Normal'] ) {
    log_likelihood <-
      dnorm( num_observed, num_predicted, num_disp, log = TRUE )
  }

  if ( lgc_distribution['Beta'] ) {
    log_likelihood <-
      ddabcd::dbetamp( num_observed, num_predicted, num_disp, log = TRUE )
  }

  if ( lgc_distribution['Logitnormal'] ) {
    num_predicted <- log(num_predicted/(1 - num_predicted))
    log_likelihood <-
      ddabcd::dlogitnorm( num_observed, num_predicted, num_disp, log = TRUE )
  }

  num_priors <- 0

  if ( !is.null(fun_priors) ) {
    num_priors <- fun_priors(
      num_param, lst_priors, chr_distribution
    )
  }

  num_output <-
    num_fnscale*( sum( log_likelihood ) + num_priors )

  return( num_output )
}

#### 3.4) ddabcd_estimation_sum_of_log_tobit_likelihoods ####
#' Compute Sum of Log Tobit Likelihoods for Delay Discounting Models
#'
#' Function to compute the sum of the log-likelihoods for
#' one of three delay discounting models: 1) an intercept-only
#' model, 2) Mazur's (1987) model, or 3) Rachlin's (2006) model.
#' Can be passed to optimization routines like
#' [stats::optim] or [bbmle::mle2] to conduct maximum likelihood
#' estimation. The outcome is required to be on the logit scale
#' and is assumed to be both left and right-censored, and the
#' tobit likelihood function (CITATION) is used as a result.
#'
#' @param num_param A numeric named vector of parameter values,
#'   where \code{num_param['a0']} is the intercept only (logit-scale),
#'   \code{num_param['nlnk']} is for minus log discounting,
#'   \code{num_param['s']} is for sensitivity, and
#'   \code{num_param['lnd']} is log dispersion.
#' @param dtf_data A data frame with the log-odds of the indifference
#'   scores, the minus log of the delay duration, and an indicator
#'   for whether the data are left-censored (-1), right-censored (1),
#'   or uncensored (0).
#' @param chr_measures A character vector giving the column names
#'   in \code{dtf_data} for 1) the log-odds of the indifference
#'   scores, 2) the minus log delays, and 3) the censoring indicator.
#' @param num_fnscale An numeric value of either 1 or -1
#'   adjusting the sum of the log-likelihoods (by default
#'   is -1 to ensure compatibility with functions like
#'   [bbmle::mle2]).
#' @param lst_priors An optional named list with the
#'   hyperparameters for the priors on each parameter.
#' @param fun_prior An optional function that takes the
#'   vector of parameters and a list of hyperparameters
#'   to compute a prior on the parameters. Allows for
#'   penalized maximum likelihood estimation.
#'
#' @return A numeric value, the sum of the log-tobit-likelihoods
#' adjusted by \code{num_fnscale}.
#'
#' @examples
#' # Examples
#'
#' @export

ddabcd_estimation_sum_of_log_tobit_likelihoods <- function(
    num_param,
    dtf_data,
    chr_measures = c(
      'ddabcd_indifference_score_log_odds',
      'ddabcd_delay_negative_log_months',
      'ddabcd_indifference_score_censored'
    ),
    num_limits = NULL,
    num_fnscale = -1,
    lst_priors = NULL,
    fun_priors = NULL ) {

  # Check if columns in data frame
  if ( !all( chr_measures %in% colnames(dtf_data) ) ) {

    chr_error <- paste0(
      "Columns specified in 'chr_measures' not ",
      "found in 'dtf_data'."
    )

    stop(chr_measures)

    # Close 'Check if columns in data frame'
  }

  num_log_odds <- dtf_data[[ chr_measures[1] ]]
  num_neg_log_delay <- dtf_data[[ chr_measures[2] ]]
  int_censored <- dtf_data[[ chr_measures[3] ]]

  # Default limits for log-odds of outcome
  if ( is.null( num_limits ) ) {

    num_limits = c(0.78125, 99.21875)/100
    num_limits <- log( num_limits/(1 - num_limits) )

    # Close 'Default limits for log-odds of outcome'
  }

  # Check for correct parameter names
  if ( !all( names( num_param ) %in% c( 'a0', 'nlnk', 's', 'lnd' ) ) ) {

    chr_error <- paste0(
      "Argument 'num_param' must be a named vector where ",
      "num_param['a0'] = intercept only (logit-scale), ",
      "num_param['nlnk'] = minus log discounting, ",
      "num_param['s'] = sensitivity, and ",
      "num_param['lnd'] = log dispersion."
    )

    stop( chr_error )

    # Close 'Check for correct parameter names'
  }

  num_sigma <- exp( num_param['lnd'] )
  num_gamma <- 1/num_sigma

  num_mu <- ddabcd::ddabcd_model_predictions(
    num_param[ names(num_param) != 'lnd' ],
    num_delay = num_neg_log_delay,
    lgc_logit = TRUE
  )*num_gamma

  num_in_range <-
    log(num_gamma) +
    dnorm( num_gamma*num_log_odds - num_mu, log = TRUE) *
    as.numeric( int_censored == 0 )
  num_under <-
    pnorm( num_gamma*num_limits[1] - num_mu, log.p = TRUE ) *
    as.numeric( int_censored == -1 )
  num_over <-
    pnorm( num_gamma*num_limits[2] - num_mu,
           log.p = TRUE, lower.tail = FALSE ) *
    as.numeric( int_censored == 1 )

  num_priors <- 0

  if ( !is.null(fun_priors) ) {
    num_priors <- fun_priors(
      num_param, lst_priors, chr_distribution
    )
  }

  num_output <-
    num_fnscale*( sum(
      num_under + num_in_range + num_over
    ) + num_priors )

  return( num_output )
}

#### 3.5) ddabcd_estimation_succeeded ####
#' Check if Model Estimation Succeeded
#'
#' Convenience function that checks the
#' class of a model fit object. Fail-states
#' for model fit result in a simple
#' character string reporting failure rather
#' than the complex class with results,
#' allowing confirmation of success if an
#' object is not a character string.
#'
#' @param obj_fit An R object, model fit output.
#'
#' @returns A logical value, \code{TRUE} if
#' \code{obj_fit} is not a character string
#' (indicating successful fit) or \code{FALSE}
#' otherwise.
#'
#' @export

ddabcd_estimation_succeeded <- function(
    obj_fit ) {

  return( !class( obj_fit ) %in% 'character' )

}

#### 4) Model fit functions ####

#### 4.1) ddabcd_fit_models_using_nls ####
#' Fit Delay Discounting Models via Non-Linear Least Squares
#'
#' Fits several delay discounting models to a long-form
#' data set using R's built-in function [stats::nls].
#' This approach was originally used by Kohler et al. (2022)
#' to fit Mazur's (1987) model to the delay discounting data
#' from the ABCD study.
#'
#' @param dtf_data A data frame with separate rows for each
#'   delay duration and participant. Must have a column
#'   with the log of the delay durations.
#' @param chr_outcome A character string, the column name
#'   for the indifference scores to fit.
#' @param chr_log_delay A character string, the column name
#'   for the log of the delay durations.
#' @param num_scaling A scaling constant (the numerator for
#'   the Mazur and Rachlin models). Typically the maximum
#'   possible indifference score.
#' @param lst_start A named list consisting of the elements
#'   \code{'Null'}, \code{'M1987'}, and \code{'R2006'}.
#'   Each list element should be a named vector with the
#'   starting values for the parameter estimates
#'   (\code{'a0'} for \code{lst_start$Null}, \code{'lnk'}
#'   for \code{lst_start$M1987}, and \code{'lnk'} and \code{'s'}
#'   for \code{lst_start$R2006}).
#' @param lgc_warnings Logical; if \code{TRUE} displays a warning
#'   if any of the estimations failed.
#' @param lst_control A list of control settings for [stats::nls].
#' @param ... Additional parameters for [stats::nls].
#'
#' @return A list with the [stats::nls] output for the null model,
#'   Mazur's (1987) model, and Rachlin's (2006) model.
#'
#' @references
#' Kohler, R. J., Lichenstein, S. D., & Yip, S. W. (2022).
#' Hyperbolic discounting rates and risk for problematic
#' alcohol use in youth enrolled in the Adolescent
#' Brain and Cognitive Development study. Addiction Biology,
#' 27 (2), 1-13.
#' https://doi.org/10.1111/adb.13160
#'
#' Mazur, J. E. (1987). An adjusting procedure for studying
#' delayed reinforcement. In M. L. Commons, J. E. Mazur,
#' J. A. Nevin, & H. Rachlin (Eds.), The effect of delay and
#' intervening events on reinforcement value (pp. 55-73).
#' Lawrence Erlbaum Associates, Inc.
#'
#' Rachlin, H. (2006). Notes on discounting. Journal of the
#' Experimental Analysis of Behavior, 85 (3), 425-435.
#' https://doi.org/10.1901/jeab.2006.85-05
#'
#' @examples
#' # Example data set
#' data(ddabcd_data_example_long)
#' # Take data from 1st participant
#' dtf_delay <- ddabcd_data_example_long[1:7, ]
#'
#' # Fit simulated data for 1st participant in example data set
#' lst_fits.nls <- ddabcd_fit_models_using_nls(
#'   dtf_delay
#' )
#'
#' @export

ddabcd_fit_models_using_nls <- function(
    dtf_data,
    chr_outcome = 'ddabcd_indifference_score_1',
    chr_log_delay = 'ddabcd_delay_log_months',
    num_scaling = 1,
    lst_start = NULL,
    lgc_warnings = FALSE,
    lst_control = list( maxit = 2e4 ),
    ... ) {

  chr_formula.Null <- paste0(
    chr_outcome, ' ~ ',
    num_scaling,
    '/(1 + exp(-a0))'
  )

  chr_formula.M1987 <- paste0(
    chr_outcome, ' ~ ',
    num_scaling, '/( 1 + exp(lnk + ',
    chr_log_delay,
    ') )'
  )

  chr_formula.R2006 <- paste0(
    chr_outcome, ' ~ ',
    num_scaling, '/( 1 + exp(lnk + ',
    chr_log_delay,
    '*s) )'
  )

  # Default starting values
  if ( is.null( lst_start) ) {

    lst_start <- list(
      Null = c( a0 = 0.00 ),
      M1987 = c( lnk = -1.96 ),
      R2006 = c( lnk = -1.55, s = 0.60 )
    )

    # Close 'Default starting values'
  }

  # Check list of starting values
  if ( !all( names( lst_start ) %in% c( 'Null', 'M1987', 'R2006' ) ) ) {

    chr_error <- paste0(
      "Must specify a named list with the elements 'Null', 'M1987', ",
      "and 'R2006' for the argment 'lst_start'."
    )
    stop( chr_error )

    # Close 'Check list of starting values'
  }

  # Check parameter names [Null]
  if ( names( lst_start$Null ) != 'a0' ) {

    chr_error <- paste0(
      "lst_start$Null must be a named vector with a value for 'a0'."
    )
    stop( chr_error )

    # Close 'Check parameter names [Null]'
  }

  # Check parameter names [M1987]
  if ( names( lst_start$M1987 ) != 'lnk' ) {

    chr_error <- paste0(
      "lst_start$M1987 must be a named vector with a value for 'lnk'."
    )
    stop( chr_error )

    # Close 'Check parameter names [M1987]'
  }

  # Check parameter names [R2006]
  if ( any( names( lst_start$R2006 ) != c( 'lnk', 's' ) ) ) {

    chr_error <- paste0(
      "lst_start$M1987 must be a named vector with a value for ",
      "'lnk' and 's'."
    )
    stop( chr_error )

    # Close 'Check parameter names [R2006]'
  }

  # Initialize list for model fits
  lst_fits.nls <- list(
    Null = 'Estimation failed',
    M1987 = 'Estimation failed',
    R2006 = 'Estimation failed\nNo variation in data'
  )

  lst_fits.nls$Null <- tryCatch(
      stats::nls(
        as.formula( chr_formula.Null ),
        start = lst_start$Null,
        data = dtf_data,
        control = lst_control,
        ...
      ),
      error = function(e) return(
        paste0( 'Estimation failed', '\n',
                paste( as.character(e), collapse = '\n' ), '\n' )
      )
    )
  lst_fits.nls$M1987 = tryCatch(
      stats::nls(
        as.formula( chr_formula.M1987 ),
        start = lst_start$M1987,
        data = dtf_data,
        control = lst_control,
        ...
      ),
      error = function(e) return(
        paste0( 'Estimation failed', '\n',
                paste( as.character(e), collapse = '\n' ), '\n' )
      )
    )

  # Check for no variation in data
  lgc_no_variation <-
    all( dtf_data[[ chr_outcome ]] == dtf_data[[ chr_outcome ]][1] &
           !is.na( dtf_data[[ chr_outcome ]] ) ) |
    any( is.na( dtf_data[[ chr_outcome ]] ) )


  # If model of Rachlin (2006) can be fit
  if ( !lgc_no_variation ) {

    lst_fits.nls$R2006 = tryCatch(
      stats::nls(
        as.formula( chr_formula.R2006 ),
        start = lst_start$R2006,
        data = dtf_data,
        control = lst_control,
        ...
      ),
      error = function(e) return(
        paste0( 'Estimation failed', '\n',
                paste( as.character(e), collapse = '\n' ), '\n' )
      )
    )

    # Close 'If model of Rachlin (2006) can be fit'
  }

  # Check for failed estimations
  if (lgc_warnings) {

    lgc_failed <- sapply(
      c( 'Null', 'M1987', 'R2006'), function(m) {
        !ddabcd_estimation_succeeded( lst_fits.nls[[m]] )
      }
    )

    # Display warning for failed estimation
    if ( any( lgc_failed ) ) {

      chr_warning <- paste0(
        'Estimation failed for ',
        paste( names(lgc_failed)[lgc_failed], collapse = ', ' )
      )
      warning( chr_warning )

      # Close 'Display warning for failed estimation'
    }

    # Close 'Check for failed estimations'
  }

  # Loop over models
  for ( m in 1:3 ) {

    # If model fit successfully
    if ( ddabcd_estimation_succeeded( lst_fits.nls[[m]] ) ) {

      lst_attr <-
        attributes( lst_fits.nls[[m]] )
      lst_attr$ddabcd_attr <- list(
        observed = dtf_data[[ chr_outcome ]],
        delay = dtf_data[[ chr_log_delay ]],
        predicted = predict( lst_fits.nls[[m]] ),
        residuals = as.numeric( residuals( lst_fits.nls[[m]] ) ),
        rmse = sqrt(
          mean( as.numeric( residuals( lst_fits.nls[[m]] ) )^2 )
        ),
        loglikelihood = as.numeric( logLik( lst_fits.nls[[m]] ) ),
        AIC = AIC( lst_fits.nls[[m]] ),
        BIC = BIC( lst_fits.nls[[m]] ),
        nobs = nrow( dtf_data ),
        dof = attributes( logLik( lst_fits.nls[[m]] ) )$df
      )
      attributes( lst_fits.nls[[m]] ) <- lst_attr

      # Close 'If model fit successfully'
    }

    # Close 'Loop over models'
  }

  return( lst_fits.nls )
}

#### 4.2) ddabcd_fit_models_using_mle2 ####
#' Fit Delay Discounting Models via Maximum Likelihood
#'
#' Fits several delay discounting models to a long-form
#' data set using the [bbmle::mle2] function from the
#' \code{'bbmle'} package.
#'
#' @param dtf_data A data frame with separate rows for each
#'   delay duration and participant. Must have a column
#'   with the log of the delay durations.
#' @param chr_outcome A character string, the column name
#'   for the indifference scores to fit.
#' @param chr_log_delay A character string, the column name
#'   for the log of the delay durations.
#' @param lst_priors An optional named list of vectors
#'   specifying the mean and standard deviation, respectively,
#'   for the prior on each parameter.
#' @param fun_prior An optional function that takes the
#'   vector of parameters and a list of hyperparameters
#'   to compute a prior on the parameters. Allows for
#'   penalized maximum likelihood estimation.
#' @param lst_start A named list consisting of the elements
#'   \code{'Null'}, \code{'M1987'}, and \code{'R2006'}.
#'   Each list element should be a named vector with the
#'   starting values for the parameter estimates (\code{'lnd'}
#'   for all models, \code{'a0'} for \code{lst_start$Null},
#'   \code{'lnk'} for \code{lst_start$M1987}, and \code{'lnk'}
#'   and \code{'s'} for \code{lst_start$R2006}).
#' @param lgc_warnings Logical; if \code{TRUE} displays a warning
#'   if any of the estimations failed.
#' @param ... Additional parameters for the [bbmle::mle2]
#'   function.
#'
#' @return A list with the [bbmle::mle2] output for the null model,
#'   Mazur's (1987) model, and Rachlin's (2006) model.
#'
#' @references
#' Mazur, J. E. (1987). An adjusting procedure for studying
#' delayed reinforcement. In M. L. Commons, J. E. Mazur,
#' J. A. Nevin, & H. Rachlin (Eds.), The effect of delay and
#' intervening events on reinforcement value (pp. 55-73).
#' Lawrence Erlbaum Associates, Inc.
#'
#' Rachlin, H. (2006). Notes on discounting. Journal of the
#' Experimental Analysis of Behavior, 85 (3), 425-435.
#' https://doi.org/10.1901/jeab.2006.85-05
#'
#' @examples
#' # Example data set
#' data(ddabcd_data_example_long)
#' # Take data from 1st participant
#' dtf_delay <- ddabcd_data_example_long[1:7, ]
#'
#' # Fit simulated data for 1st participant in example data set
#' lst_fit.mle2 <- ddabcd_fit_models_using_mle2(
#'   dtf_delay, 'Beta', control = list( maxit = 2e4 )
#' )
#'
#' @export

ddabcd_fit_models_using_mle2 <- function(
    dtf_data,
    chr_distribution,
    chr_outcome = 'ddabcd_indifference_score_1',
    chr_log_delay = 'ddabcd_delay_log_months',
    lst_priors = NULL,
    fun_priors = NULL,
    lst_start = NULL,
    lgc_warnings = FALSE,
    ... ) {

  lgc_distribution <- ddabcd_model_distributions(
    chr_distribution
  )
  chr_measures <- c(
    chr_outcome, chr_log_delay
  )

  # Default starting values
  if ( is.null( lst_start) ) {

    if ( lgc_distribution['Normal'] ) {

      lst_start <- list(
        Null = c( a0 = .50, lnd = log(0.3) ),
        M1987 = c( lnk = -1.96, lnd = log(0.3) ),
        R2006 = c( lnk = -1.55, s = 0.60, lnd = log(0.3) )
      )

    }

    if ( lgc_distribution['Beta'] ) {

      lst_start <- list(
        Null = c( a0 = 0.5, lnd = log(4) ),
        M1987 = c( lnk = -1.96, lnd = log(4) ),
        R2006 = c( lnk = -1.55, s = 0.60, lnd = log(4) )
      )

    }

    if ( lgc_distribution['Logitnormal'] ) {

      lst_start <- list(
        Null = c( a0 = 0.5, lnd = log(1) ),
        M1987 = c( lnk = -1.96, lnd = log(1) ),
        R2006 = c( lnk = -1.55, s = 0.60, lnd = log(1) )
      )

    }

    # Close 'Default starting values'
  }

  # If no priors specified
  if ( is.null(lst_priors) ) {

    lst_priors <- list(
      Null = NULL,
      M1987 = NULL,
      R2006 = NULL
    )

    # Close 'If no priors specified'
  }

  # Check list of starting values
  if ( !all( names( lst_start ) %in% c( 'Null', 'M1987', 'R2006' ) ) ) {

    chr_error <- paste0(
      "Must specify a named list with the elements 'Null', 'M1987', ",
      "and 'R2006' for the argument 'lst_start'."
    )
    stop( chr_error )

    # Close 'Check list of starting values'
  }

  # Check list of starting values
  if ( !all( names( lst_priors ) %in% c( 'Null', 'M1987', 'R2006' ) ) ) {

    chr_error <- paste0(
      "Must specify a named list with the elements 'Null', 'M1987', ",
      "and 'R2006' for the argument 'lst_priors'."
    )
    stop( chr_error )

    # Close 'Check list of starting values'
  }

  # Check parameter names [Null]
  if ( any( !names( lst_start$Null ) %in% c( 'a0', 'lnd' ) ) ) {

    chr_error <- paste0(
      "lst_start$Null must be a named vector with ",
      "values for 'a0' and 'lnd'."
    )
    stop( chr_error )

    # Close 'Check parameter names [Null]'
  }

  # Check parameter names [M1987]
  if ( any( !names( lst_start$M1987 ) %in% c( 'lnk', 'lnd' ) ) ) {

    chr_error <- paste0(
      "lst_start$M1987 must be a named vector with ",
      "values for 'lnk' and 'lnd'."
    )
    stop( chr_error )

    # Close 'Check parameter names [M1987]'
  }

  # Check parameter names [R2006]
  if ( any( !names( lst_start$R2006 ) %in% c( 'lnk', 's', 'lnd' ) ) ) {

    chr_error <- paste0(
      "lst_start$R2006 must be a named vector with a value for ",
      "'lnk', 's', and 'lnd'."
    )
    stop( chr_error )

    # Close 'Check parameter names [R2006]'
  }

  lst_fits.mle2 <- list(
    Null = 'Estimation failed',
    M1987 = 'Estimation failed',
    R2006 = 'Estimation failed\nNo variation in data'
  )

  fun_nsll <- ddabcd::ddabcd_estimation_sum_of_log_likelihoods
  bbmle::parnames( fun_nsll ) <- c( 'a0', 'lnd' )

  lst_fits.mle2$Null <- tryCatch(
    bbmle::mle2(
      fun_nsll,
      start = lst_start$Null,
      data = list(
        dtf_data = dtf_data,
        chr_distribution = chr_distribution,
        chr_measures = chr_measures,
        lst_priors = lst_priors$Null,
        fun_priors = fun_priors
      ),
      vecpar = TRUE,
      ...
    ),
    error = function(e) return(
      paste0( 'Estimation failed', '\n',
              paste( as.character(e), collapse = '\n' ), '\n' )
    )
  )

  fun_nsll <- ddabcd::ddabcd_estimation_sum_of_log_likelihoods
  bbmle::parnames( fun_nsll ) <- c( 'lnk', 'lnd' )

  lst_fits.mle2$M1987 <- tryCatch(
    bbmle::mle2(
      fun_nsll,
      start = lst_start$M1987,
      data = list(
        dtf_data = dtf_data,
        chr_distribution = chr_distribution,
        chr_measures = chr_measures,
        lst_priors = lst_priors$M1987,
        fun_priors = fun_priors
      ),
      vecpar = TRUE,
      ...
    ),
    error = function(e) return(
      paste0( 'Estimation failed', '\n',
              paste( as.character(e), collapse = '\n' ), '\n' )
    )
  )

  # Check for no variation in data
  lgc_no_variation <-
    all( dtf_data[[ chr_outcome ]] == dtf_data[[ chr_outcome ]][1] &
           !is.na( dtf_data[[ chr_outcome ]] ) ) |
    any( is.na( dtf_data[[ chr_outcome ]] ) )

  # If model of Rachlin (2006) can be fit
  if ( !lgc_no_variation ) {

    fun_nsll <- ddabcd::ddabcd_estimation_sum_of_log_likelihoods
    bbmle::parnames( fun_nsll ) <- c( 'lnk', 's', 'lnd' )

    lst_fits.mle2$R2006 <- tryCatch(
      bbmle::mle2(
        fun_nsll,
        start = lst_start$R2006,
        data = list(
          dtf_data = dtf_data,
          chr_distribution = chr_distribution,
          chr_measures = chr_measures,
          lst_priors = lst_priors$R2006,
          fun_priors = fun_priors,
          ...
        ),
        vecpar = TRUE
      ),
      error = function(e) return(
        paste0( 'Estimation failed', '\n',
                paste( as.character(e), collapse = '\n' ), '\n' )
      )
    )

    # Close 'If model of Rachlin (2006) can be fit'
  }

  # Check for failed estimations
  if (lgc_warnings) {

    lgc_failed <- sapply(
      c( 'Null', 'M1987', 'R2006'), function(m) {
        !ddabcd_estimation_succeeded( lst_fits.mle2[[m]] )
      }
    )

    # Display warning for failed estimation
    if ( any( lgc_failed ) ) {

      chr_warning <- paste0(
        'Estimation failed for ',
        paste( names(lgc_failed)[lgc_failed], collapse = ', ' )
      )
      warning( chr_warning )

      # Close 'Display warning for failed estimation'
    }

    # Close 'Check for failed estimations'
  }

  # Loop over models
  for ( m in 1:3 ) {

    # If model fit successfully
    if ( ddabcd_estimation_succeeded( lst_fits.mle2[[m]] ) ) {

      num_param <- coef( lst_fits.mle2[[m]] )

      num_predicted <- ddabcd::ddabcd_model_predictions(
        num_param[ names(num_param) != 'lnd' ],
        dtf_data[[ chr_log_delay ]]
      )
      num_residuals <-
        dtf_data[[ chr_outcome ]] - num_predicted

      lst_attr <-
        attributes( lst_fits.mle2[[m]] )
      lst_attr$ddabcd_attr <- list(
        observed = dtf_data[[ chr_outcome ]],
        delay = dtf_data[[ chr_log_delay ]],
        predicted = num_predicted,
        residuals = num_residuals,
        rmse = sqrt(
          mean( num_residuals^2 )
        ),
        loglikelihood = as.numeric( logLik( lst_fits.mle2[[m]] ) ),
        AIC = AIC( lst_fits.mle2[[m]] ),
        BIC = BIC( lst_fits.mle2[[m]] ),
        nobs = nrow( dtf_data ),
        dof = attributes( logLik( lst_fits.mle2[[m]] ) )$df
      )
      attributes( lst_fits.mle2[[m]] ) <- lst_attr

      # Close 'If model fit successfully'
    }

    # Close 'Loop over models'
  }

  return( lst_fits.mle2 )
}

#### 4.3) ddabcd_fit_models_using_tl_mle2 ####
#' Fit Delay Discounting Models via Maximum Tobit Likelihood
#'
#' Fits several delay discounting models to a long-form
#' data set using the [bbmle::mle2] function from the
#' \code{'bbmle'} package and a tobit likelihood function
#' (CITATION) to deal with censored data.
#'
#' @param dtf_data A data frame with separate rows for each
#'   delay duration and participant. Must have a column
#'   with the log of the delay durations.
#' @param chr_outcome A character string, the column name
#'   for the log-odds of the indifference scores to fit.
#' @param chr_minus_log_delay A character string, the
#'   column name for the log of the delay durations.
#' @param chr_censored A character string, the
#'   column name for the indicator of censored data.
#' @param num_limits A numeric vector with the lower
#'   and upper limits below and above which data are
#'   censored.
#' @param lst_priors An optional named list of vectors
#'   specifying the hyperparameters for the prior on each
#'   parameter.
#' @param fun_prior An optional function that takes the
#'   vector of parameters and a list of hyperparameters
#'   to compute a prior on the parameters. Allows for
#'   penalized maximum likelihood estimation.
#' @param lst_start A named list consisting of the elements
#'   \code{'Null'}, \code{'M1987'}, and \code{'R2006'}.
#'   Each list element should be a named vector with the
#'   starting values for the parameter estimates (\code{'lnd'}
#'   for all models, \code{'a0'} for \code{lst_start$Null},
#'   \code{'nlnk'} for \code{lst_start$M1987}, and \code{'nlnk'}
#'   and \code{'s'} for \code{lst_start$R2006}).
#' @param lgc_warnings Logical; if \code{TRUE} displays a warning
#'   if any of the estimations failed.
#' @param ... Additional parameters for the [bbmle::mle2]
#'   function.
#'
#' @return A list with the [bbmle::mle2] output for the null model,
#'   Mazur's (1987) model, and Rachlin's (2006) model.
#'
#' @references
#' Mazur, J. E. (1987). An adjusting procedure for studying
#' delayed reinforcement. In M. L. Commons, J. E. Mazur,
#' J. A. Nevin, & H. Rachlin (Eds.), The effect of delay and
#' intervening events on reinforcement value (pp. 55-73).
#' Lawrence Erlbaum Associates, Inc.
#'
#' Rachlin, H. (2006). Notes on discounting. Journal of the
#' Experimental Analysis of Behavior, 85 (3), 425-435.
#' https://doi.org/10.1901/jeab.2006.85-05
#'
#' @examples
#' # Example data set
#' data(ddabcd_data_example_long)
#' # Take data from 1st participant
#' dtf_delay <- ddabcd_data_example_long[1:7, ]
#'
#' # Fit simulated data for 1st participant in example data set
#' lst_fit.mle2 <- ddabcd_fit_models_using_tl_mle2(
#'   dtf_delay, control = list( maxit = 2e4 )
#' )
#'
#' @export

ddabcd_fit_models_using_tl_mle2 <- function(
    dtf_data,
    chr_outcome = 'ddabcd_indifference_score_log_odds',
    chr_minus_log_delay = 'ddabcd_delay_negative_log_months',
    chr_censored = 'ddabcd_indifference_score_censored',
    num_limits = NULL,
    lst_priors = NULL,
    fun_priors = NULL,
    lst_start = NULL,
    lgc_warnings = FALSE,
    ... ) {

  chr_measures <- c(
    chr_outcome, chr_minus_log_delay, chr_censored
  )

  # Default starting values
  if ( is.null( lst_start) ) {

    lst_start <- list(
      Null = c( a0 = 0.5, lnd = log(1) ),
      M1987 = c( nlnk = 1.96, lnd = log(1) ),
      R2006 = c( nlnk = 1.55, s = 0.60, lnd = log(1) )
    )

    # Close 'Default starting values'
  }

  # If no priors specified
  if ( is.null(lst_priors) ) {

    lst_priors <- list(
      Null = NULL,
      M1987 = NULL,
      R2006 = NULL
    )

    # Close 'If no priors specified'
  }

  # Check list of starting values
  if ( !all( names( lst_start ) %in% c( 'Null', 'M1987', 'R2006' ) ) ) {

    chr_error <- paste0(
      "Must specify a named list with the elements 'Null', 'M1987', ",
      "and 'R2006' for the argument 'lst_start'."
    )
    stop( chr_error )

    # Close 'Check list of starting values'
  }

  # Check list of starting values
  if ( !all( names( lst_priors ) %in% c( 'Null', 'M1987', 'R2006' ) ) ) {

    chr_error <- paste0(
      "Must specify a named list with the elements 'Null', 'M1987', ",
      "and 'R2006' for the argument 'lst_priors'."
    )
    stop( chr_error )

    # Close 'Check list of starting values'
  }

  # Check parameter names [Null]
  if ( any( !names( lst_start$Null ) %in% c( 'a0', 'lnd' ) ) ) {

    chr_error <- paste0(
      "lst_start$Null must be a named vector with ",
      "values for 'a0' and 'lnd'."
    )
    stop( chr_error )

    # Close 'Check parameter names [Null]'
  }

  # Check parameter names [M1987]
  if ( any( !names( lst_start$M1987 ) %in% c( 'nlnk', 'lnd' ) ) ) {

    chr_error <- paste0(
      "lst_start$M1987 must be a named vector with ",
      "values for 'nlnk' and 'lnd'."
    )
    stop( chr_error )

    # Close 'Check parameter names [M1987]'
  }

  # Check parameter names [R2006]
  if ( any( !names( lst_start$R2006 ) %in% c( 'nlnk', 's', 'lnd' ) ) ) {

    chr_error <- paste0(
      "lst_start$R2006 must be a named vector with a value for ",
      "'nlnk', 's', and 'lnd'."
    )
    stop( chr_error )

    # Close 'Check parameter names [R2006]'
  }

  lst_fits.mle2 <- list(
    Null = 'Estimation failed',
    M1987 = 'Estimation failed',
    R2006 = 'Estimation failed\nNo variation in data'
  )

  fun_nsll <- ddabcd::ddabcd_estimation_sum_of_log_tobit_likelihoods
  bbmle::parnames( fun_nsll ) <- c( 'a0', 'lnd' )

  lst_fits.mle2$Null <- tryCatch(
    bbmle::mle2(
      fun_nsll,
      start = lst_start$Null,
      data = list(
        dtf_data = dtf_data[, chr_measures],
        chr_measures = chr_measures,
        num_limits = num_limits,
        lst_priors = lst_priors$Null,
        fun_priors = fun_priors
      ),
      vecpar = TRUE,
      ...
    ),
    error = function(e) return(
      paste0( 'Estimation failed', '\n',
              paste( as.character(e), collapse = '\n' ), '\n' )
    )
  )

  fun_nsll <- ddabcd::ddabcd_estimation_sum_of_log_tobit_likelihoods
  bbmle::parnames( fun_nsll ) <- c( 'nlnk', 'lnd' )

  lst_fits.mle2$M1987 <- tryCatch(
    bbmle::mle2(
      fun_nsll,
      start = lst_start$M1987,
      data = list(
        dtf_data = dtf_data[, chr_measures],
        chr_measures = chr_measures,
        num_limits = num_limits,
        lst_priors = lst_priors$M1987,
        fun_priors = fun_priors
      ),
      vecpar = TRUE,
      ...
    ),
    error = function(e) return(
      paste0( 'Estimation failed', '\n',
              paste( as.character(e), collapse = '\n' ), '\n' )
    )
  )

  # Check for no variation in data
  lgc_no_variation <-
    all( dtf_data[[ chr_outcome ]] == dtf_data[[ chr_outcome ]][1] &
           !is.na( dtf_data[[ chr_outcome ]] ) ) |
    any( is.na( dtf_data[[ chr_outcome ]] ) )


  # If model of Rachlin (2006) can be fit
  if ( !lgc_no_variation ) {

    fun_nsll <- ddabcd::ddabcd_estimation_sum_of_log_tobit_likelihoods
    bbmle::parnames( fun_nsll ) <- c( 'nlnk', 's', 'lnd' )

    lst_fits.mle2$R2006 <- tryCatch(
      bbmle::mle2(
        fun_nsll,
        start = lst_start$R2006,
        data = list(
          dtf_data = dtf_data[, chr_measures],
          chr_measures = chr_measures,
          num_limits = num_limits,
          lst_priors = lst_priors$R2006,
          fun_priors = fun_priors,
          ...
        ),
        vecpar = TRUE
      ),
      error = function(e) return('Estimation failed')
    )

    # Close 'If model of Rachlin (2006) can be fit'
  }

  # Check for failed estimations
  if (lgc_warnings) {

    lgc_failed <- sapply(
      c( 'Null', 'M1987', 'R2006'), function(m) {
        ddabcd::ddabcd_estimation_succeeded( lst_fits.mle2[[m]] )
      }
    )

    # Display warning for failed estimation
    if ( any( lgc_failed ) ) {

      chr_warning <- paste0(
        'Estimation failed for ',
        paste( names(lgc_failed)[lgc_failed], collapse = ', ' )
      )
      warning( chr_warning )

      # Close 'Display warning for failed estimation'
    }

    # Close 'Check for failed estimations'
  }

  # Loop over models
  for ( m in 1:3 ) {

    # If model fit successfully
    if ( ddabcd_estimation_succeeded( lst_fits.mle2[[m]] ) ) {

      num_param <- coef( lst_fits.mle2[[m]] )

      num_predicted <- ddabcd::ddabcd_model_predictions(
        num_param[ names(num_param) != 'lnd' ],
        dtf_data[[ chr_minus_log_delay ]],
        lgc_logit = TRUE
      )
      num_predicted[ num_predicted < num_limits[1] ] <-
        num_limits[1]
      num_predicted[ num_predicted > num_limits[2] ] <-
        num_limits[2]
      num_residuals <-
        dtf_data[[ chr_outcome ]] - num_predicted

      lst_attr <-
        attributes( lst_fits.mle2[[m]] )
      lst_attr$ddabcd_attr <- list(
        observed = dtf_data[[ chr_outcome ]],
        delay = dtf_data[[ chr_minus_log_delay ]],
        predicted = num_predicted,
        residuals = num_residuals,
        rmse = sqrt(
          mean( num_residuals^2 )
        ),
        loglikelihood = as.numeric( logLik( lst_fits.mle2[[m]] ) ),
        AIC = AIC( lst_fits.mle2[[m]] ),
        BIC = BIC( lst_fits.mle2[[m]] ),
        nobs = nrow( dtf_data ),
        dof = attributes( logLik( lst_fits.mle2[[m]] ) )$df
      )
      attributes( lst_fits.mle2[[m]] ) <- lst_attr

      # Close 'If model fit successfully'
    }

    # Close 'Loop over models'
  }

  return( lst_fits.mle2 )
}

#### 4.4) ddabcd_fit_models_across_participants ####

#' Fit Models Across Participants
#'
#' Function to fit models separately to each set of
#' data per participant.
#'
#' @param dtf_data A data frame with separate rows for each
#'   delay duration and participant. Must have columns for
#'   the indifference scores (ranging from 0 to 1) and for
#'   log of the delay durations.
#' @param fun_to_fit_models A function to fit the delay
#'   discounting models to the data (see
#'   [ddabcd::ddabcd_fit_models_using_nls] for an example).
#' @param chr_participants A character string, the column name
#'   for each participant's identifier.
#' @param lgc_progress Logical; if \code{TRUE} displays a
#'   progress bar for the function run time.
#' @param ... Additional parameters for the
#'   \code{fun_to_fit_models} function.
#'
#' @returns A list of lists, the model fit results for each
#' participant.
#'
#' @examples
#' # Example data set
#' data(ddabcd_data_example_long)
#' dtf_delay <- ddabcd_data_example_long[
#'   ddabcd_data_example_long$eventname == "1_year_follow_up_y_arm_1",
#' ]
#'
#' # Fit models using nls
#' lst_all_fits.nls <- ddabcd_fit_models_across_participants(
#'   dtf_delay,
#'   ddabcd_fit_models_using_nls,
#'   lgc_progress = FALSE
#' )
#'
#' @export

ddabcd_fit_models_across_participants <- function(
    dtf_data,
    fun_to_fit_models,
    chr_participant = 'src_subject_id',
    lgc_progress = TRUE,
    ... ) {

  chr_participants <- unique( dtf_data[[ chr_participant ]] )
  int_participants <- length( chr_participants )

  lst_all_fits <- rep( list(NULL), int_participants )
  names( lst_all_fits ) <- chr_participants

  # Loop over participants
  for ( p in seq_along( chr_participants ) ) {

    dat_start <- Sys.time()

    lgc_rows <- dtf_data[[ chr_participant ]] %in% chr_participants[p]

    lst_all_fits[[p]] <- fun_to_fit_models(
      dtf_data[lgc_rows, ],
      ...
    )

    dat_end <- Sys.time()

    # First participant
    if ( p == 1 ) {

      # Track progress
      if ( lgc_progress ) {

        message( 'Predicted run time:' )
        message( paste0(
          '  ', round(
            difftime( dat_end, dat_start, units = 'mins') *
              int_participants, 2
          ), ' minutes'
        ) )
        message('')

        # Create a progress bar using a base R function
        pb <- txtProgressBar( min = 1, max = int_participants, style = 3 )

        # Close 'Track progress'
      }

      # Close 'First participant'
    }

    # Update the progress bar
    if ( lgc_progress ) {
      setTxtProgressBar(pb,p)
      # Close 'Update the progress bar'
    }

    # Close 'Loop over participants'
  }
  # Close out progress bar
  if (lgc_progress) {

    close(pb)
    rm(pb)

    # Close 'Close out progress bar'
  }

  return( lst_all_fits )
}

#### 5) Post-estimation functions ####

#### 5.1) Custom operators for extracting elements ####

#### 5.1.1) `%pull_coef%` ####
#' Extract Coefficient Point Estimates
#'
#' Custom operator to extract point estimates for
#' parameters from an R object compatible with the
#' \code{coef} method.
#'
#' @param obj_fit An R object with a \code{coef} method.
#' @param obj_param Either an integer or character
#'   vector specifying the subset of the \code{coef}
#'   output to keep.
#'
#' @return A numeric vector.
#'
#' @examples
#' # Example using 'lm'
#' set.seed( 20230517 ) # For reproducibility
#' dtf_data <- data.frame( x = rnorm(30) )
#' dtf_data$y <- dtf_data$x * .5 + rnorm(30)
#' lst_lm <- lm( y ~ x, data = dtf_data)
#' lst_lm %pull_coef% '(Intercept)'
#' lst_lm %pull_coef% 'x'
#'
#' @export

`%pull_coef%` <- function(
    obj_fit,
    obj_param ) {

  if ( ddabcd::ddabcd_estimation_succeeded(obj_fit) ) {

    return( coef( obj_fit )[obj_param] )

  } else {

    return( rep( NA, length(obj_param) ) )

  }

}

#### 5.1.2) `%index%` ####
#' Index Rows of a Data Frame
#'
#' Convenience operator that matches a user-supplied
#' index (either an integer or character string)
#' against the elements in a data frame column and
#' returns the corresponding set of rows. Useful,
#' for example, to isolate rows for a specific
#' participant in a long-form data set.
#'
#' @param dtf_data A data frame.
#' @param obj_index Either an integer vector, character
#'   vector, or a list with the integer/character vector
#'   and a character string specifying the column to
#'   match over. If \code{obj_index} is an integer,
#'   the operator uses the first column of \code{dtf_data}
#'   to match over.
#'
#' @return A data frame, the subset of \code{dtf_data}
#' that had successful matches.
#'
#' @examples
#' # Create example data set
#' dtf_data <- data.frame(
#'   V1 = rep( LETTERS[1:3], each = 2 ), V2 = c( 1:3, 1:3 )
#' )
#'
#' # Return rows for first unique element of 'V1'
#' dtf_data %index% 1
#' # Return rows for first unique element of 'V2'
#' dtf_data %index% list( 1, 'V2' )
#' # Return rows for 'V1' == 'B'
#' dtf_data %index% 'B'
# Return rows for 'V2' == 3
#' dtf_data %index% list( '3', 'V2' )
#'
#' @export

`%index%` <- function(
    dtf_data,
    obj_index ) {

  # Check if data frame
  if ( !is.data.frame( dtf_data ) ) {

    stop( "First argument must be a data frame." )

    # Close 'Check if data frame'
  }

  # If not a list
  if ( is.numeric( obj_index ) | is.character( obj_index ) ) {

    # If numeric index
    if ( is.numeric( obj_index ) ) {

      obj_to_match <- unique( dtf_data[[1]] )[obj_index]

      # Close 'If numeric index'
    } else {

      obj_to_match <- obj_index

      # Close else for 'If numeric index'
    }

    lgc_rows <- dtf_data[[1]] %in% obj_to_match

    if ( !any( lgc_rows ) ) {
      stop(
        "No matches found with 'obj_index'"
      )
    }

    return( dtf_data[lgc_rows, ] )

    # Close 'If not a list'
  }

  # If list
  if ( is.list( obj_index ) ) {

    # If numeric index
    if ( is.numeric( obj_index[[1]] ) ) {

      obj_to_match <-
        unique( dtf_data[[ obj_index[[2]] ]] )[obj_index[[1]]]

      # Close 'If numeric index'
    } else {

      obj_to_match <- obj_index[[1]]

      # Close else for 'If numeric index'
    }

    lgc_rows <- dtf_data[[ obj_index[[2]] ]] %in% obj_to_match

    if ( !any( lgc_rows ) ) {
      stop(
        "No matches found with 'obj_index[[1]]'"
      )
    }

    return( dtf_data[lgc_rows, ] )

    # Close 'If list'
  }

  chr_error <- paste0(
    "Argument 'obj_index' should be a numeric index, ",
    "a character vector to match, or a list with the
    index/character vector and column name."
  )

  stop( chr_error )

}

#### 5.1.3) `%col%` ####
#' Extract Columns Matching Partial Strings
#'
#' Convenience operator that takes a vector
#' of partial strings and determines the columns
#' that match the strings. Also allows specifying
#' elements that must be included or excluded
#' when matching.
#'
#' @param dtf_data A data frame.
#' @param chr_column_parts A character vector, the
#'   partial strings to match column names against.
#'   Columns that match any of the strings are returned.
#'   However, if a string starts with \code{'+'}, all
#'   columns must include that string. If a string
#'   starts with \code{'-'}, no columns can include
#'   that string. Note at least one element of
#'   \code{chr_column_parts} cannot start with
#'   either \code{'+'} or \code{'-'}.
#'
#' @return A data frame, the subset of columns meeting
#' the criteria specified by \code{chr_column_parts}.
#'
#' @examples
#' dtf_data <- data.frame( A1 = 1:3, A2 = 4:6, B1 = 7:9, B2 = 10:12 )
#'
#' # Return all columns with an 'A'
#' dtf_data %col% 'A'
#' # Return all columns with an 'A1' or 'B1'
#' dtf_data %col% c( 'A1', 'B1' )
#' # Return columns with a '1' and either 'A' or 'B'
#' dtf_data %col% c( '+1', 'A', 'B' )
# Return columns without a '1' and either 'A' or 'B'
#' dtf_data %col% c( '-1', 'A', 'B' )
#'
#' @export

`%col%` <- function(
    dtf_data,
    chr_column_parts ) {

  # Check if data frame
  if ( !is.data.frame( dtf_data ) ) {

    stop( "First argument must be a data frame." )

    # Close 'Check if data frame'
  }

  chr_all_columns <- colnames( dtf_data )

  lgc_modifiers <-
    substr( chr_column_parts, 1, 1 ) %in% c( '+', '-' )

  if ( all(lgc_modifiers) ) {
    stop( "Must provide at least one argument without '+' or '-'" )
  }

  chr_base <- chr_column_parts[!lgc_modifiers]

  obj_match <- sapply(
    chr_base, function(s) {
      grepl( s, chr_all_columns, fixed = TRUE )
    }
  )

  if ( is.matrix( obj_match ) ) {
    lgc_match <- apply( obj_match, 1, any )
  } else {
    lgc_match <- obj_match
  }

  if ( !any(lgc_match) ) {
    stop( "No matching columns found" )
  }

  chr_columns_keep <- chr_all_columns[lgc_match]

  if ( any( lgc_modifiers ) ) {

    lgc_must_have <-
      substr( chr_column_parts, 1, 1) == '+'

    if ( any( lgc_must_have ) ) {

      chr_must_have <- gsub(
        '+', '', chr_column_parts[lgc_must_have], fixed = TRUE
      )

      obj_match <- sapply(
        chr_must_have, function(s) {
          grepl( s, chr_columns_keep, fixed = TRUE )
        }
      )

      if ( is.matrix( obj_match ) ) {
        lgc_match <- apply( obj_match, 1, all )
      } else {
        lgc_match <- obj_match
      }

      if ( !any(lgc_match) ) {
        stop( "No matching columns found" )
      }

      chr_columns_keep <- chr_columns_keep[lgc_match]

    }

    lgc_must_not_have <-
      substr( chr_column_parts, 1, 1) == '-'

    if ( any( lgc_must_not_have ) ) {

      chr_must_not_have <- gsub(
        '-', '', chr_column_parts[lgc_must_not_have], fixed = TRUE
      )

      obj_match <- sapply(
        chr_must_not_have, function(s) {
          grepl( s, chr_columns_keep, fixed = TRUE )
        }
      )

      if ( is.matrix( obj_match ) ) {
        lgc_match <- apply( obj_match, 1, all )
      } else {
        lgc_match <- obj_match
      }

      if ( all(lgc_match) ) {
        stop( "All columns excluded" )
      }

      chr_columns_keep <- chr_columns_keep[!lgc_match]

    }


  }

  if ( length( chr_columns_keep ) > 1 ) {
    return( dtf_data[, chr_columns_keep] )
  } else {

    dtf_subset <- data.frame(
      V1 = dtf_data[[chr_columns_keep]]
    )
    colnames( dtf_subset ) <- chr_columns_keep

    return( dtf_subset )
  }

}

#### 5.2) Additional measures for model fit

#### 5.2.1) ddabcd_post_fit_compute_rmse ####
#' Compute Root-Mean-Square-Error (RMSE)
#'
#' Given a set of predicted and observed values,
#' computes the root-mean-square-error (also
#' known as the root-mean-square-deviation). Note
#' predicted values must be on the same scale as
#' the observed values.
#'
#' @param num_predicted A numeric vector matching in
#'   length to \code{num_observed}.
#' @param num_observed A numeric vector matching in
#'   length to \code{num_predicted}.
#'
#' @details
#' Given \code{n} predicted values \code{p} and observed
#' values \code{o}, the RMSE is computed as
#' \code{sqrt( sum( (p - o)^2 )/n )}.
#'
#' @return A numeric value, the RMSE.
#'
#' @examples
#' # Example using 'lm'
#' set.seed( 20230517 ) # For reproducibility
#' dtf_data <- data.frame( x = rnorm(30) )
#' dtf_data$y <- dtf_data$x * .5 + rnorm(30)
#' lst_lm <- lm( y ~ x, data = dtf_data)
#' # Compute RMSE
#' ddabcd_post_fit_compute_rmse(
#'   predict(lst_lm), dtf_data$y
#' )
#' # Equivalent to...
#' sqrt( mean( residuals(lst_lm)^2 ) )
#'
#' @export

ddabcd_post_fit_compute_rmse <- function(
    num_predicted,
    num_observed ) {

  if ( !is.numeric(num_predicted) |
       !is.numeric(num_observed) ) {
    stop( "Inputs must be numeric vectors of matching length" )
  }

  if ( length(num_predicted) != length(num_observed) ) {
    stop( "Length of 'y' and 'yhat' must match" )
  }

  int_size <- length( num_observed )
  num_rmse <- sqrt(
    sum( (num_predicted - num_observed)^2 )/int_size
  )

  return( num_rmse )
}


#### 5.2.2) ddabcd_post_fit_report ####
#' Brief Report on Delay Discounting Model Fit
#'
#' Function that provides a summary of (a) the number of
#' participants for which models where successfully fit,
#' and (b) means and uncertainty intervals for the
#' root-mean-square-error (RMSE).
#'
#' @param lst_all_fits A list of lists with model fits
#' (see [ddabcd::dabcd_fit_models_across_participants]).
#'
#' @returns A message to the console.
#'
#' @examples
#' # Example data
#' data(ddabcd_data_example_long)
#' # Fit first time point and first 3 participants
#' dtf_long <- ddabcd_data_example_long %index% 1:4
#' dtf_long <- dtf_long %index% list( 1, 'eventname' )
#' lst_all_fits.nls <- ddabcd_fit_models_across_participants(
#'   dtf_long, ddabcd_fit_models_using_nls, lgc_progress = FALSE
#' )
#'
#' ddabcd_post_fit_report( lst_all_fits.nls )
#'
#' @export

ddabcd_post_fit_report <- function(
    lst_all_fits ) {

  int_total <- length(lst_all_fits)

  int_successful_fits <- sapply(
    c( 'Null', 'M1987', 'R2006' ), function(m) {
      ddabcd::ddabcd_post_fit_extract(
        lst_all_fits,
        fun_extract = function(l) ddabcd::ddabcd_estimation_succeeded(l),
        chr_model = m
      ) |> sum()
    }
  )

  num_rmse_mean <- sapply(
    c( 'Null', 'M1987', 'R2006' ), function(m) {
      ddabcd::ddabcd_post_fit_extract(
        lst_all_fits,
        fun_extract = function(l) {
          ddabcd::ddabcd_post_fit_if_success(
            l,
            fun_call = ddabcd::ddabcd_post_fit_attr,
            chr_attr = 'rmse'
          )
        },
        chr_model = m
      ) |> mean( na.rm = TRUE )
    }
  )

  num_rmse_sd <- sapply(
    c( 'Null', 'M1987', 'R2006' ), function(m) {
      ddabcd::ddabcd_post_fit_extract(
        lst_all_fits,
        fun_extract = function(l) {
          ddabcd::ddabcd_post_fit_if_success(
            l,
            fun_call = ddabcd::ddabcd_post_fit_attr,
            chr_attr = 'rmse'
          )
        },
        chr_model = m
      ) |> sd( na.rm = TRUE )
    }
  )

  num_rmse_se <-
    num_rmse_sd / sqrt( int_successful_fits )
  num_rmse_ui_lb <-
    num_rmse_mean + qt( .025, int_successful_fits - 1 ) * num_rmse_se
  num_rmse_ui_ub <-
    num_rmse_mean + qt( .975, int_successful_fits - 1 ) * num_rmse_se

  chr_models <- c( ' Null', 'M1987', 'R2006' )

  for ( m in 1:3 ) {

    message(
      paste0(
        'Model: ', chr_models[m]
        )
      )
    message(
      paste0(
        '  Successful estimation: ',
        int_successful_fits[m], '/', int_total, ' (',
        format(
          round( 100*int_successful_fits[m]/int_total, 1 ),
          nsmall = 1
        ),
        '%)'
      )
    )
    message(
      paste0(
        '  RMSE: Mean = ',
        format( round( num_rmse_mean[m], 2 ), nsmall = 2 ),
        ', 95% UI = ',
        format( round( num_rmse_ui_lb[m], 2 ), nsmall = 2 ),
        ' to ',
        format( round( num_rmse_ui_ub[m], 2 ), nsmall = 2 )
      )
    )
    message('')

  }

}

#### 5.2.3) ddabcd_post_fit_sample_parameters ####
#' Draw Samples For Parameter Estimates
#'
#' Function that conducts a parametric bootstrap
#' assuming parameter estimates follow a multivariate
#' normal distribution.
#'
#' @param obj_fit A model fit object that has methods
#'   for \code{coef} and \code{vcov}.
#' @param int_samples The numer of samples to draw
#'   from the multivariate normal distribution.
#' @param lgc_suppress_error Logical; if \code{TRUE}
#'   suppress error messages and return a matrix
#'   of \code{NA} values instead.
#' @param chr_labels The column names for the matrix
#'   of \code{NA} values if samples cannot be drawn.
#'
#' @returns A matrix.
#'
#' @examples
#' # Example data
#' data( ddabcd_data_example_long )
#' dtf_long <- ddabcd_data_example_long %index% 1
#' dtf_long <- dtf_long %index% list( 1, 'eventname' )
#'
#' lst_fits.mle2 <- ddabcd_fit_models_using_mle2(
#'   dtf_long, chr_distribution = 'Normal'
#' )
#'
#' ddabcd_post_fit_sample_parameters(
#'   lst_fits.mle2$M1987, int_samples = 3
#' )
#'
#' @export

ddabcd_post_fit_sample_parameters <- function(
    obj_fit,
    int_samples = 1000,
    lgc_suppress_error = FALSE,
    chr_labels = c( 'a0', 'lnk', 's', 'lnd' ) ) {

  # If successful fit
  if ( ddabcd_post_fit_if_success( obj_fit ) ) {

    num_coef <- coef( obj_fit )
    mat_VC <- vcov( obj_fit )

    mat_samples <- MASS::mvrnorm(
      n = int_samples,
      mu = num_coef,
      Sigma = mat_VC
    )
    colnames( mat_samples ) <- names( num_coef )

    # Close 'If successful fit'
  } else {

    # Do not stop on error
    if (lgc_suppress_error) {

      mat_samples <- matrix( NA, int_samples, length( chr_labels ) )
      colnames(mat_samples) <- c(
        chr_labels
      )

      # Close 'Do not stop on error'
    } else {

      stop("Unable to draw parameter samples")

      # Close else for 'Do not stop on error'
    }

    # Close else for 'If successful fit'
  }

  return( mat_samples )
}

#### 5.2.4) ddabcd_post_fit_bootstrapped_prediction_interval_mle2 ####
#' Bootstrapped Prediction Intervals for Models fit by mle2
#'
#' Compute bootstrapped prediction intervals via simulating
#' 1) possible parameter values from a multivariate normal
#' distribution, and 2) possible observed data values for
#' delay discounting model at each set of parameter values.
#'
#' @param obj_fit A model fit object that has methods
#'   for \code{coef} and \code{vcov}.
#' @param int_samples The numer of samples to draw
#'   from the multivariate normal distribution.
#' @param lgc_suppress_error Logical; if \code{TRUE}
#'   suppress error messages and return a matrix
#'   of \code{NA} values instead.
#' @param chr_labels The column names for the matrix
#'   of \code{NA} values if samples cannot be drawn.
#' @param num_width A numeric value between 0 and 1, the
#'   width of the prediction interval.
#' @param num_limits An optional vector giving the lower
#'   and upper limits below and above which data should
#'   be censored.
#'
#' @return A 2 x N matrix whose rows give the lower and
#' upper bootstrapped prediction intervals for the N observations.
#'
#' @examples
#' # Example data
#' data( ddabcd_data_example_long )
#' dtf_long <- ddabcd_data_example_long %index% 1
#' dtf_long <- dtf_long %index% list( 1, 'eventname' )
#'
#' lst_fits.mle2 <- ddabcd_fit_models_using_mle2(
#'   dtf_long, chr_distribution = 'Logit-normal'
#' )
#'
#' ddabcd_post_fit_bootstrapped_prediction_intervals_mle2(
#'   lst_fits.mle2$R2006
#' )
#'
#' @export

ddabcd_post_fit_bootstrapped_prediction_intervals_mle2 <- function(
    obj_fit,
    int_samples = 1000,
    lgc_suppress_error = FALSE,
    chr_labels = c( 'a0', 'lnk', 's', 'lnd' ),
    num_width = .95,
    num_limits = NULL,
    ... ) {

  mat_param_samples <- ddabcd::ddabcd_post_fit_sample_parameters(
    obj_fit,
    int_samples = int_samples,
    lgc_suppress_error = lgc_suppress_error
  )

  chr_distribution <- attributes( obj_fit )$data$chr_distribution
  num_delay <- attributes( obj_fit )$data$dtf_data[[
    attributes( obj_fit )$data$chr_measures[2]
  ]]

  mat_simulated <- apply(
    mat_param_samples, 1, function(num_param) {
      ddabcd_model_predictions(
        num_param = num_param,
        num_delay = num_delay,
        chr_distribution = chr_distribution,
        ...
      )
    }
  )

  num_intervals <-
    .5 + num_width*c( -.5, .5)

  # If simulated data should be censored
  if ( !is.null( num_limits ) ) {

    # Loop over columns
    for ( k in 1:ncol( mat_simulated ) ) {

      mat_simulated[, k] <- ddabcd_data_censor(
        mat_simulated[, k], chr_outcome = '',
        num_limits = num_limits,
        num_scaling = 1
      )

      # Close 'Loop over columns'
    }

    # Close 'If simulated data should be censored'
  }

  mat_predicted <-
    apply( mat_simulated, 1, quantile, prob = num_intervals )

  return( mat_predicted )
}

#### 5.3) Tools for extracting/incorporating results ####

#### 5.3.1) ddabcd_post_fit_extract ####
#' Extract Elements From List of Model Fits
#'
#' Function to extract a specified element
#' (e.g., a parameter estimate) from a list of
#' lists of model fits (see
#' [ddabcd::ddabcd_fit_models_across_participants]
#' for an example).
#'
#' @param lst_all_fits A list of lists with model fits.
#' @param fun_extract A function that extracts a
#'   specified element from the model fit (defaults to
#'   the \code{coef} function).
#' @param chr_model A character string, the specific
#'   model from which elements should be extracted.
#'   Options include \code{'Null'}, \code{'M1987'},
#'   or \code{'R2006'}.
#' @param ... Additional arguments to the
#'   \code{fun_extract} function.
#'
#' @returns Output from the \code{sapply} function
#' after applying \code{fun_extract} to each
#' element of \code{lst_all_fits}.
#'
#' @export

ddabcd_post_fit_extract <- function(
    lst_all_fits,
    fun_extract = NULL,
    chr_model = 'M1987',
    ... ) {

  # Default extract function
  if ( is.null( fun_extract ) ) {

    # Extract point estimate for first parameter
    fun_extract <- function(obj_fit) {
      if ( ddabcd::ddabcd_post_fit_if_success(obj_fit) ) {
        return( coef(obj_fit)[1] )
      } else {
        return(NA)
      }
    }

    # Close 'Default extract function'
  }

  obj_output <- sapply(
    seq_along( lst_all_fits ), function(p) {
      fun_extract( lst_all_fits[[p]][[chr_model]], ... )
    }
  )

  # If output is list
  if ( is.list(obj_output) ) {

    names( obj_output ) <- names( lst_all_fits )

    # Close 'If output is list'
  }

  # If output is vector
  if ( is.vector(obj_output) ) {

    names( obj_output ) <- names( lst_all_fits )

    # Close 'If output is vector'
  }

  # If output is matrix
  if ( is.matrix(obj_output) ) {

    colnames( obj_output ) <- names( lst_all_fits )

    # Close 'If output is matrix'
  }

  return( obj_output )
}

#### 5.3.2) ddabcd_post_fit_attr ####
#' Extract 'ddabcd_attr' Component
#'
#' Utility function to extract elements from the
#' \code{'ddabcd_attr'} component of the
#' attributes for model fits from
#' [ddabcd::ddabcd_fit_models_using_nls]
#' and [ddabcd::ddabcd_fit_models_using_mle2].
#'
#' @param obj_fit An R object for model fit results.
#' @param chr_attr A character string, the
#'   specific attribute to access. Options include
#'   \code{'predicted'}, \code{'residuals'},
#'   \code{'rmse'}, \code{'loglikelihood'},
#'   \code{'AIC'}, \code{'BIC'}, \code{'nobs'},
#'   or \code{'dof'}.
#' @param obj_default Value to return if attributes
#'   aren't found.
#'
#' @return A list of elements or a specific element
#' from the \code{'ddabcd_attr'} attribute.
#'
#' @examples
#' # Example data
#' data(ddabcd_data_example_long)
#' # Use data for first time point and first 3 participants
#' dtf_long <- ddabcd_data_example_long %index% 1:3
#' dtf_long <- dtf_long %index% list( 1, 'eventname' )
#'
#' # Fit data using 'nls'
#' lst_all_fits.nls <- ddabcd_fit_models_across_participants(
#'   dtf_long, ddabcd_fit_models_using_nls, lgc_progress = FALSE
#' )
#'
#' ddabcd_post_fit_attr( lst_all_fits.nls[[1]]$M1987 )['rmse']
#'
#' @export

ddabcd_post_fit_attr <- function(
    obj_fit,
    chr_attr = '',
    obj_default = NA ) {

  lst_attr <- attributes( obj_fit )$ddabcd_attr

  # If attributes present
  if ( !is.null( lst_attr ) ) {

    # If specific attribute requested
    if ( chr_attr == '' ) {

      return( lst_attr )

      # Close 'If specific attribute requested'
    } else {

      return( lst_attr[[ chr_attr ]] )

      # Close else for 'If specific attribute requested'
    }

    # Close 'If attributes present'
  } else {

    return( obj_default )

    # Close else for 'If attributes present'
  }

}

#### 5.3.3) ddabcd_post_fit_if_success ####
#' Call Function on Successful Model Fit
#'
#' Applies a specified function to a model fit
#' object only if estimation succeeded.
#'
#' @param obj_fit An R object with model fit results.
#' @param fun_call A function to apply to
#'   \code{obj_fit}.
#' @param obj_default The output if \code{obj_fit}
#'   is a character string (i.e., estimation failed).
#' @param ... Additional parameters to \code{fun_call}.
#'
#' @returns The results of \code{fun_call}, or if
#' \code{obj_fit} is a character string, \code{obj_default}
#' instead.
#'
#' @examples
#' # Example data
#' data(ddabcd_data_example_long)
#' # Fit first time point and first and last participant
#' dtf_long <- ddabcd_data_example_long %index% c( 1, 48 )
#' dtf_long <- dtf_long %index% list( 1, 'eventname' )
#'
#' # Fit data using 'nls'
#' lst_all_fits.nls <- ddabcd_fit_models_across_participants(
#'   dtf_long, ddabcd_fit_models_using_nls, lgc_progress = FALSE
#' )
#'
#' # Estimation fails for Rachlin's (2006) model for last subject
#' # due to no variation in data to identify 2nd parameter
#' sapply( 1:2, function(i) {
#'   ddabcd_post_fit_if_success(
#'     lst_all_fits.nls[[i]]$R2006, return(TRUE), FALSE )
#' } )
#'
#' @export

ddabcd_post_fit_if_success <- function(
    obj_fit,
    fun_call,
    obj_default = NA,
    ... ) {

  if ( ddabcd_estimation_succeeded(obj_fit) ) {
    return( fun_call(obj_fit, ...) )
  } else {
    return( obj_default )
  }

}

#### 5.3.4) ddabcd_post_fit_add_to_data ####
#' Add Extracted Elements From Model Fit to Data
#'
#' Function to extract elements from a list of model
#' fits to different participants and add them
#' to a data frame.
#'
#' @param dtf_data A data frame. Must contain the column
#'   specified by \code{chr_participant}.
#' @param lst_all_fits A list of model fits per participant
#'   (see [ddabcd::ddabcd_fit_models_across_participants]).
#' @param chr_column A character string, the column to
#'   either add to or update for \code{dtf_data}.
#' @param lgc_filtering A logical vector matching in
#'   length to the number of rows of \code{dtf_data}
#'   specifying the subset of rows to update.
#' @param num_default The default value to use when
#'   elements can't be extracted from \code{lst_all_fits}.
#' @param chr_participant A character string, the column
#'   name with the participant IDs to match across
#'   \code{dtf_data} and \code{lst_all_fits}.
#' @param ... Additional parameters for the
#'   [ddabcd::ddabcd_post_fit_extract] function.
#'
#' @return A data frame, \code{dtf_data} with an updated column.
#'
#' @examples
#' # Example data
#' data(ddabcd_data_example_wide)
#' # Use data for first time point and first 3 participants
#' dtf_wide <- ddabcd_data_example_wide %index% 1:3
#' dtf_wide <- dtf_wide %index% list( 1, 'eventname' )
#' # Create long-form data
#' dtf_long <- ddabcd_data_prep( dtf_wide )
#'
#' # Fit data using 'nls'
#' lst_all_fits.nls <- ddabcd_fit_models_across_participants(
#'   dtf_long, ddabcd_fit_models_using_nls, lgc_progress = FALSE
#' )
#'
#' # Add estimate of log discounting to wide-form data set
#' dtf_wide <- ddabcd_post_fit_add_to_data(
#'   dtf_wide, lst_all_fits.nls, 'ddabcd_estimates_M1987_nls.lnk'
#' )
#' # Add estimate of log discounting to wide-form data set
#' # for Rachlin's (2006) model
#' dtf_wide <- ddabcd_post_fit_add_to_data(
#'   dtf_wide, lst_all_fits.nls, 'ddabcd_estimates_R2006_nls.lnk',
#'   fun_extract = function(l) coef(l)['lnk'],
#'   chr_model = 'R2006'
#' )
#' # Add estimate of sensitivity to wide-form data set
#' # for Rachlin's (2006) model
#' dtf_wide <- ddabcd_post_fit_add_to_data(
#'   dtf_wide, lst_all_fits.nls, 'ddabcd_estimates_R2006_nls.s',
#'   fun_extract = function(l) coef(l)['s'],
#'   chr_model = 'R2006'
#' )
#'
#' @export

ddabcd_post_fit_add_to_data <- function(
    dtf_data,
    lst_all_fits,
    chr_column,
    lgc_filtering = NULL,
    num_default = NA,
    chr_participant = 'src_subject_id',
    ... ) {

  # By default include all rows
  if ( is.null( lgc_filtering ) ) {

    lgc_filtering <- rep( TRUE, nrow( dtf_data ) )

    # Close 'By default include all rows'
  }

  # Extract elements from list of fits
  vec_value <- ddabcd_post_fit_extract(
    lst_all_fits,
    ...
  )

  # Check output
  if ( !is.vector( vec_value ) ) {

    chr_error <- paste0(
      "The function 'ddabcd_post_fit_extract' must produce ",
      "a vector as output"
    )

    stop( chr_error )

    # Close 'Check output'
  }

  # Adapt vector of outputs to add to data frame
  vec_to_add <- sapply(
    dtf_data[[ chr_participant ]], function(chr_id) {

      num_output <- num_default

      lgc_match <-
        names(vec_value) %in% chr_id

      # If participant has output
      if ( any(lgc_match) ) {

        num_output <- vec_value[lgc_match]

        # Close 'If participant has output'
      }

      return(num_output)

    } )

  # If column already exists
  if ( chr_column %in% colnames(dtf_data) ) {

    dtf_data[[ chr_column]][lgc_filtering] <-
      vec_to_add[lgc_filtering]

    # Close 'If column already exists'
  } else {

    vec_to_add[ !lgc_filtering ] <- num_default
    dtf_data[[ chr_column ]] <- vec_to_add

    # Close else for 'If column already exists'
  }

  return( dtf_data )
}

#### 5.3.5) ddabcd_post_fit_convergence ####
#' Check if Models Converged
#'
#' Function to iterate over model fits across
#' participants and check for model convergence.
#'
#' @param lst_all_fits A list of lists with model fits.
#' @param chr_model A character string, the specific
#'   model from which elements should be extracted.
#'   Options include \code{'Null'}, \code{'M1987'},
#'   or \code{'R2006'}.
#'
#' @returns A logical vector equal to \code{TRUE}
#' if models converged, and \code{FALSE} otherwise.
#'
#' @export

ddabcd_post_fit_convergence <- function(
    lst_all_fits,
    chr_model = 'M1987' ) {

  lgc_converged <- sapply(
    seq_along( lst_all_fits ), function(i) {

      # Check if any estimation results
      lgc_fit <- ddabcd::ddabcd_estimation_succeeded(
        lst_all_fits[[i]][[chr_model]]
      )

      # Any estimation results
      if ( lgc_fit ) {

        # If fit using nls
        if ( class( lst_all_fits[[i]][[chr_model]] ) == 'nls' ) {

          return(
            lst_all_fits[[i]][[chr_model]]$convInfo$stopCode %in% 0
          )

          # Close 'If fit using nls'
        }

        # If fit using mle2
        if ( class( lst_all_fits[[i]][[chr_model]] ) == 'mle2' ) {

          return(
            lst_all_fits[[i]][[chr_model]]@details$convergence %in% 0
          )

          # Close 'If fit using mle2'
        }

        return( NA )

        # Close 'Any estimation results'
      } else {

        return( FALSE )

        # Close else for 'Any estimation results'
      }

    }
  )

  return( lgc_converged )
}
