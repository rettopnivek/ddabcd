% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ddabcd_model_functions.R
\name{\%pull_coef\%}
\alias{\%pull_coef\%}
\title{Extract Coefficient Point Estimates}
\usage{
obj_fit \%pull_coef\% obj_param
}
\arguments{
\item{obj_fit}{An R object with a \code{coef} method.}

\item{obj_param}{Either an integer or character
vector specifying the subset of the \code{coef}
output to keep.}
}
\value{
A numeric vector.
}
\description{
Custom operator to extract point estimates for
parameters from an R object compatible with the
\code{coef} method.
}
\examples{
# Example using 'lm'
set.seed( 20230517 ) # For reproducibility
dtf_data <- data.frame( x = rnorm(30) )
dtf_data$y <- dtf_data$x * .5 + rnorm(30)
lst_lm <- lm( y ~ x, data = dtf_data)
lst_lm \%pull_coef\% '(Intercept)'
lst_lm \%pull_coef\% 'x'

}
