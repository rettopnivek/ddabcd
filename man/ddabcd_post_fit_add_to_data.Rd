% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ddabcd_model_functions.R
\name{ddabcd_post_fit_add_to_data}
\alias{ddabcd_post_fit_add_to_data}
\title{Add Extracted Elements From Model Fit to Data}
\usage{
ddabcd_post_fit_add_to_data(
  dtf_data,
  lst_all_fits,
  chr_column,
  lgc_filtering = NULL,
  num_default = NA,
  chr_participant = "src_subject_id",
  ...
)
}
\arguments{
\item{dtf_data}{A data frame. Must contain the column
specified by \code{chr_participant}.}

\item{lst_all_fits}{A list of model fits per participant
(see \link{ddabcd_fit_models_across_participants}).}

\item{chr_column}{A character string, the column to
either add to or update for \code{dtf_data}.}

\item{lgc_filtering}{A logical vector matching in
length to the number of rows of \code{dtf_data}
specifying the subset of rows to update.}

\item{num_default}{The default value to use when
elements can't be extracted from \code{lst_all_fits}.}

\item{chr_participant}{A character string, the column
name with the participant IDs to match across
\code{dtf_data} and \code{lst_all_fits}.}

\item{...}{Additional parameters for the
\link{ddabcd_post_fit_extract} function.}
}
\value{
A data frame, \code{dtf_data} with an updated column.
}
\description{
Function to extract elements from a list of model
fits to different participants and add them
to a data frame.
}
\examples{
# Example data
data(ddabcd_data_example_wide)
# Use data for first time point and first 3 participants
dtf_wide <- ddabcd_data_example_wide \%index\% 1:3
dtf_wide <- dtf_wide \%index\% list( 1, 'eventname' )
# Create long-form data
dtf_long <- ddabcd_data_prep( dtf_wide )

# Fit data using 'nls'
lst_all_fits.nls <- ddabcd_fit_models_across_participants(
  dtf_long, ddabcd_fit_models_using_nls, lgc_progress = FALSE
)

# Add estimate of log discounting to wide-form data set
dtf_wide <- ddabcd_post_fit_add_to_data(
  dtf_wide, lst_all_fits.nls, 'ddabcd_estimates_M1987_nls.lnk'
)
# Add estimate of log discounting to wide-form data set
# for Rachlin's (2006) model
dtf_wide <- ddabcd_post_fit_add_to_data(
  dtf_wide, lst_all_fits.nls, 'ddabcd_estimates_R2006_nls.lnk',
  fun_extract = function(l) coef(l)['lnk'],
  chr_model = 'R2006'
)
# Add estimate of sensitivity to wide-form data set
# for Rachlin's (2006) model
dtf_wide <- ddabcd_post_fit_add_to_data(
  dtf_wide, lst_all_fits.nls, 'ddabcd_estimates_R2006_nls.s',
  fun_extract = function(l) coef(l)['s'],
  chr_model = 'R2006'
)

}
