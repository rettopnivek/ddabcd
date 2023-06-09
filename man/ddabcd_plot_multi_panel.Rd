% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ddabcd_plotting_functions.R
\name{ddabcd_plot_multi_panel}
\alias{ddabcd_plot_multi_panel}
\title{Plot Delay Discounting Data (Multiple Panels)}
\usage{
ddabcd_plot_multi_panel(
  obj_data,
  chr_participant = "src_subject_id",
  int_panels = 9,
  num_dim = c(width = 9, height = 9),
  num_margin = c(3.35, 3.35, 1, 1),
  chr_xlab = "ln(Delay)",
  chr_ylab = "Indifference score",
  num_line = c(-1.1, -1.1),
  lgc_new = FALSE,
  ...
)
}
\arguments{
\item{obj_data}{Either a data frame or a list of
lists with model fits (see
\link{ddabcd_fit_models_across_participants}).}

\item{chr_participant}{A character string, the column name
for each participant's identifier.}

\item{int_panels}{An integer value, the total number of
panels to plot in a single window (preferably a value
whose square root is a whole number).}

\item{num_dim}{A numeric vector specifying the
width and height of the plot panel when a
new plotting window is generated.}

\item{num_margin}{A numeric vector of 4 values giving the
margin sizes for the bottom, left, top, and right edges
of the figure.}

\item{chr_xlab}{A character string, the x-axis label.}

\item{chr_ylab}{A character string, the y-axis label.}

\item{num_line}{A numeric vector with two values
controling the positions of the x and y-axis labels
respectively.}

\item{lgc_new}{Logical; if \code{TRUE} generates a
new plotting window.}

\item{...}{Additional parameters for the
\link{ddabcd_plot_single_panel} function.}
}
\value{
A plot.
}
\description{
Function to plot delay discounting data (typically
the log of the delay durations in months and
indifference scores as proportions) across
multiple participants in a multi-panel figure.
}
\examples{
\dontrun{
# Example data
data("ddabcd_data_example_long")
dtf_long <- ddabcd_data_example_long \%index\% list(1, 'eventname')
# Plot data for 9 participants taken at random
int_participants <- sample( 1:48, size = 9 )
ddabcd_plot_multi_panel(
  dtf_long \%index\% int_participants
)

# Fit data for 9 participants
lst_all_fits.nls <- ddabcd_fit_models_across_participants(
  dtf_long, ddabcd_fit_models_using_nls, lgc_progress = FALSE
)
ddabcd_plot_multi_panel(
  lst_all_fits.nls
)
}

}
