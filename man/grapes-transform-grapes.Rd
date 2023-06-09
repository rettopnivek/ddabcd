% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ddabcd_model_functions.R
\name{\%transform\%}
\alias{\%transform\%}
\title{Apply Transformations to Vector of Values}
\usage{
num_values \%transform\% chr_transform
}
\arguments{
\item{num_values}{A numeric vector.}

\item{chr_transform}{A character string, the
transform to apply. Options include
\code{'logit'}, \code{'logistic'},
\code{'probit'}, or \code{'inverse-probit'}.}
}
\value{
A numeric vector of transformed values.
}
\description{
Operator to apply a specified transformation to
a vector of numeric values, such as the logit
or logistic transform.
}
\examples{
c(.25, .5, .75) \%transform\% 'logit' |> round(3)
c(-1.099, 0, 1.099) \%transform\% 'logistic' |> round(3)

}
