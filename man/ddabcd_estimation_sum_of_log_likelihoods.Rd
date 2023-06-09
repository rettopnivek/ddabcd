% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ddabcd_model_functions.R
\name{ddabcd_estimation_sum_of_log_likelihoods}
\alias{ddabcd_estimation_sum_of_log_likelihoods}
\title{Compute Sum of Log Likelihoods for Delay Discounting Models}
\usage{
ddabcd_estimation_sum_of_log_likelihoods(
  num_param,
  dtf_data,
  chr_distribution,
  chr_measures = c("ddabcd_indifference_score_1", "ddabcd_delay_log_months"),
  num_fnscale = -1,
  lst_priors = NULL,
  fun_priors = NULL
)
}
\arguments{
\item{num_param}{A numeric named vector of parameter values,
where \code{num_param['a0']} is the intercept only (logit-scale),
\code{num_param['lnk']} is for log discounting,
\code{num_param['s']} is for sensitivity, and
\code{num_param['lnd']} is log dispersion.}

\item{dtf_data}{A data frame with the indifference scores
(ranging from 0 to 1) and the log of the delay duration.}

\item{chr_distribution}{A character string, the type of
probability distribution the data are assumed to follow.
Options include \code{'Normal'}, \code{'Beta'}, or
\code{'Logit-normal'}.}

\item{chr_measures}{A character vector giving the
column names in \code{dtf_data} for 1) the
indifference scores, and 2) the log delays.}

\item{num_fnscale}{An numeric value of either 1 or -1
adjusting the sum of the log-likelihoods (by default
is -1 to ensure compatibility with functions like
\link[bbmle:mle2]{bbmle::mle2}).}

\item{lst_priors}{An optional named list with the
hyperparameters for the priors on each parameter.}

\item{fun_prior}{An optional function that takes the
vector of parameters and a list of hyperparameters
to compute a prior on the parameters. Allows for
penalized maximum likelihood estimation
(see \link{ddabcd_estimation_priors}).}
}
\value{
A numeric value, the sum of the log-likelihoods
adjusted by \code{num_fnscale}.
}
\description{
Function to compute the sum of the log-likelihoods for
one of three delay discounting models: 1) an intercept-only
model, 2) Mazur's (1987) model, or 3) Rachlin's (2006) model.
Can be passed to optimization routines like
\link[stats:optim]{stats::optim} or \link[bbmle:mle2]{bbmle::mle2} to conduct maximum likelihood
estimation.
}
\examples{
# Example data set
data(ddabcd_data_example_long)
# Take data from 1st participant
dtf_delay <- ddabcd_data_example_long[1:7, ]

# Intercept-only model
# Data follow a normal distribution
num_param <- c( a0 = 2.2, lnd = log(.3) )
ddabcd_estimation_sum_of_log_likelihoods(
  num_param, dtf_delay, 'Normal'
)

# Mazur's (1987) model
# Data follow a beta distribution
num_param <- c( lnk = log(0.14), lnd = log(4) )
ddabcd_estimation_sum_of_log_likelihoods(
  num_param, dtf_delay, 'Beta'
)

# Rachlin's (2006) model
# Data follow a logit-normal distribution
num_param <- c( lnk = log(0.14), s = 0.6, lnd = log(4) )
ddabcd_estimation_sum_of_log_likelihoods(
  num_param, dtf_delay, 'Logit-normal'
)

}
