# Plotting functions
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2023-05-22

#### 1) ddabcd_plot_single_panel ####
#' Plot Delay Discounting Data (Single Panel)
#'
#' Function to plot delay discounting data (typically
#' the log of the delay durations in months and
#' indifference scores as proportions) as a single
#' panel figure.
#'
#' @param obj_data Either a data frame or a list of
#'   model fits (see for example
#'   [ddabcd::ddabcd_fit_models_using_nls]).
#' @param obj_x Either a character vector with the
#'   column(s) to index for the x-axis values or
#'   a numeric vector.
#' @param obj_y Either a character vector with the
#'   column(s) to index for the y-axis values or
#'   a numeric vector.
#' @param chr_xlab A character string, the x-axis label.
#' @param chr_ylab A character string, the y-axis label.
#' @param num_xlim An optional numeric vector with the
#'   lower and upper limits for the x-axis.
#' @param num_ylim A numeric vector with the lower and
#'   upper limits for the y-axis. Default is (0, 1).
#' @param num_dim A numeric vector specifying the
#'   width and height of the plot panel when a
#'   new plotting window is generated.
#' @param int_pch The type of point to draw.
#' @param num_cex.pt A numeric value, the size of the point.
#' @param num_lwd A numeric value, the width of the line.
#' @param chr_color A character vector giving the colors
#'   to use for 1) the line for observed data,
#'   2) the points for observed data, 3) the
#'   line for predicted data, and 4) the
#'   error bars for predicted data.
#' @param num_jitter An optional numeric value specifying the
#'   limits for random uniform jitter noise to add to points.
#' @param chr_model A character string, the model to
#'   extract results from if \code{obj_data} is a list
#'   of model fits. Options include \code{'Null'},
#'   \code{'M1987'}, or \code{'R2006'}.
#' @param num_margin A numeric vector of 4 values giving the
#'   margin sizes for the bottom, left, top, and right edges
#'   of the figure.
#' @param lst_predicted A list of either 1 or 3 elements
#'   specifying either the columns or numeric values to
#'   to use for \code{[[1]]} model predictions and
#'   \code{[[2]]} and \code{[[3]]} the lower and upper
#'   boundaries of the prediction interval.
#' @param lgc_add Logical; if \code{TRUE} adds new data
#'   to an existing figure.
#' @param lgc_new Logical; if \code{TRUE} generates a
#'   new plotting window.
#' @param lst_prediction_settings An optional named list
#'   specifying how prediction intervals should be calculated
#'   if a model fit object is provided. The list should contain
#'   \code{lst_prediction_settings$width} for the width of the
#'   prediction interval, \code{lst_prediction_settings$bootstrap}
#'   set to \code{TRUE} for using a parametric bootstrap and
#'   \code{FALSE} otherwise, and \code{lst_prediction_settings$samples}
#'   with the number of samples to draw for the parametric bootstrap.
#' @param ... Additional parameters to the [base::plot]
#'   function.
#'
#' @returns A plot.
#'
#' @examples
#' \dontrun{
#' # Load in example data sets
#' data( ddabcd_data_example_wide )
#' data( ddabcd_data_example_long )
#' dtf_wide <- ddabcd_data_example_wide %index% list( 1, 'eventname' )
#' dtf_long <- ddabcd_data_example_long %index% list( 1, 'eventname' )
#'
#' # By default data assumed to be long-form
#' ddabcd_plot_single_panel( dtf_long %index% 1 ) # 1st participant
#' # Wide-form data can be plotted by providing vector of column names
#' ddabcd_plot_single_panel(
#'   dtf_wide %index% 1, # 1st participant
#'   obj_x = ddabcd_data_delay_durations(),
#'   colnames( dtf_wide %col% 'indif' ),
#'   num_ylim = c( 0, 100 )
#' )
#'
#' # Example using model fits for first participant
#' lst_fits.nls <- ddabcd_fit_models_using_nls( dtf_long %index% 1)
#' ddabcd_plot_single_panel( lst_fits.nls, chr_model = 'M1987' )
#' # Add predictions for Rachlin's (2006) model
#' ddabcd_plot_single_panel(
#'   lst_fits.nls, chr_model = 'R2006',
#'   chr_color = c('black', 'black', 'red' ), lgc_add = TRUE
#' )
#' }
#'
#' @export

ddabcd_plot_single_panel <- function(
    obj_data,
    obj_x = 'ddabcd_delay_log_months',
    obj_y = 'ddabcd_indifference_score_1',
    chr_xlab = 'log(Delay - months)',
    chr_ylab = 'Indifference score (Proportion)',
    num_xlim = NULL,
    num_ylim = c(0, 1),
    num_dim = c( width = 5, height = 5 ),
    int_pch = 19,
    num_lwd = 2,
    num_cex.pt = 1,
    chr_color = c( 'black', 'black', 'blue', 'grey' ),
    num_jitter = 0,
    chr_model = 'M1987',
    num_margin = c( 4, 4, 2, .5 ),
    lst_predicted = NULL,
    lgc_add = FALSE,
    lgc_new = FALSE,
    lst_prediction_settings = NULL,
    ... ) {

  # Default settings for prediction intervals
  if ( is.null( lst_prediction_settings ) ) {

    lst_prediction_settings <- list(
      width = .95,
      bootstrap = FALSE,
      samples = 1000
    )

    # Close 'Default settings for prediction intervals'
  }

  # If data frame provided
  if ( is.data.frame( obj_data ) ) {

    dtf_data <- obj_data

    # Close 'If data frame provided'
  } else {

    # If list of model fits is provided
    if ( is.list( obj_data ) ) {

      dtf_data <- data.frame(
        x = ddabcd_post_fit_attr(
          obj_data[[ chr_model ]], 'delay'
        ),
        y = ddabcd_post_fit_attr(
          obj_data[[ chr_model ]], 'observed'
        ),
        yhat = ddabcd_post_fit_attr(
          obj_data[[ chr_model ]], 'predicted'
        )
      )
      obj_x <- 'x'
      obj_y <- 'y'
      lst_predicted <- list(
        'yhat',
        'yhat.lb',
        'yhat.ub'
      )

      mat_PI <- ddabcd::ddabcd_post_fit_prediction_intervals(
        obj_data[[ chr_model ]],
        num_width = lst_prediction_settings$width,
        lgc_bootstrap = lst_prediction_settings$bootstrap,
        int_samples = lst_prediction_settings$samples
      )

      if ( !is.null( mat_PI ) ) {
        dtf_data$yhat.lb <- mat_PI[1, ]
        dtf_data$yhat.ub <- mat_PI[2, ]
      }

      # Close 'If list of model fits is provided'
    }

    # Close else for 'If data frame provided'
  }

  # Ensure dimensions for plotting window are labeled
  if ( is.null( names(num_dim) ) ) {

    names( num_dim ) <- c( 'width', 'height' )

    # Close 'Ensure dimensions for plotting window are labeled'
  }

  # Create new plotting window
  if (lgc_new) {

    x11( width = num_dim['width'], height = num_dim['height'] )

    # Close 'Create new plotting window'
  }

  # If column name for x-axis provided
  if ( is.character( obj_x ) ) {

    num_x <- dtf_data[, obj_x ]

    # Close 'If column name for x-axis provided'
  } else {

    num_x <- obj_x

    # Close else for 'If column name for x-axis provided'
  }

  # If column name(s) for y-axis provided
  if ( is.character( obj_y ) ) {

    num_y <- dtf_data[, obj_y ]

    # Close 'If column name(s) for y-axis provided'
  } else {

    num_y <- obj_y

    # Close 'If column name(s) for y-axis provided'
  }

  if ( !lgc_add ) {

    if ( is.null( num_xlim ) ) {
      num_xlim <- range( num_x )
    }

    par( mar = num_margin )
    plot(
      num_x,
      num_y,
      type = 'n',
      ylim = num_ylim,
      xlim = num_xlim,
      xlab = chr_xlab,
      ylab = chr_ylab,
      ...
    )

  }

  # Add model predictions
  if ( !is.null( lst_predicted[[1]] ) ) {

    # If prediction intervals provided
    if ( !is.null( lst_predicted[[2]] ) &
         !is.null( lst_predicted[[3]] ) ) {

      # If column name provided
      if ( is.character( lst_predicted[[2]] ) ) {

        # If column name found
        if ( lst_predicted[[2]] %in% colnames(dtf_data) ) {

          num_yhat.lb <- dtf_data[, lst_predicted[[2]]]

          # Close 'If column name found'
        } else {

          num_yhat.lb <- rep( NA, length(num_y) )

          # Close else for 'If column name found'
        }

        # Close 'If column name provided'
      } else {

        num_yhat.lb <- lst_predicted[[2]]

        # Close else for 'If column name provided'
      }

      # If column name provided
      if ( is.character( lst_predicted[[3]] ) ) {

        # If column name found
        if ( lst_predicted[[3]] %in% colnames(dtf_data) ) {

          num_yhat.ub <- dtf_data[, lst_predicted[[3]]]

          # Close 'If column name found'
        } else {

          num_yhat.ub <- rep( NA, length(num_y) )

          # Close else for 'If column name found'
        }

        # Close 'If column name provided'
      } else {

        num_yhat.ub <- lst_predicted[[3]]

        # Close else for 'If column name provided'
      }

      polygon(
        c( num_x, rev( num_x ) ),
        c( num_yhat.lb, rev( num_yhat.ub ) ),
        col = chr_color[4], border = NA
      )

      # Close 'If prediction intervals provided'
    }

    # If column name provided
    if ( is.character( lst_predicted[[1]] ) ) {

      # If column name found
      if ( lst_predicted[[1]] %in% colnames(dtf_data) ) {

        num_yhat <- dtf_data[, lst_predicted[[1]]]

        # Close 'If column name found'
      } else {

        num_yhat <- rep( NA, length(num_y) )

        # Close else for 'If column name found'
      }

      # Close 'If column name provided'
    } else {

      num_yhat <- lst_predicted[[1]]

      # Close else for 'If column name provided'
    }

    lines( num_x, num_yhat, lwd = num_lwd, col = chr_color[3] )

    # Close 'Add model predictions'
  }

  if ( length( num_y ) == length( unique( num_x ) ) ) {
    lines( num_x, num_y, lwd = num_lwd, col = chr_color[1] )
  }

  if ( num_jitter != 0 ) {
    num_x <- num_x + runif( length(num_x), -num_jitter, num_jitter )
  }

  points( num_x, num_y,
          pch = int_pch, col = chr_color[2],
          cex = num_cex.pt )

}

#### 2) ddabcd_plot_multi_panel ####
#' Plot Delay Discounting Data (Multiple Panels)
#'
#' Function to plot delay discounting data (typically
#' the log of the delay durations in months and
#' indifference scores as proportions) across
#' multiple participants in a multi-panel figure.
#'
#' @param obj_data Either a data frame or a list of
#'   lists with model fits (see
#'   [ddabcd::ddabcd_fit_models_across_participants]).
#' @param chr_participant A character string, the column name
#'   for each participant's identifier.
#' @param int_panels An integer value, the total number of
#'   panels to plot in a single window (preferably a value
#'   whose square root is a whole number).
#' @param num_dim A numeric vector specifying the
#'   width and height of the plot panel when a
#'   new plotting window is generated.
#' @param num_margin A numeric vector of 4 values giving the
#'   margin sizes for the bottom, left, top, and right edges
#'   of the figure.
#' @param chr_xlab A character string, the x-axis label.
#' @param chr_ylab A character string, the y-axis label.
#' @param num_line A numeric vector with two values
#'   controling the positions of the x and y-axis labels
#'   respectively.
#' @param lgc_new Logical; if \code{TRUE} generates a
#'   new plotting window.
#' @param ... Additional parameters for the
#'   [ddabcd::ddabcd_plot_single_panel] function.
#'
#' @returns A plot.
#'
#' @examples
#' \dontrun{
#' # Example data
#' data("ddabcd_data_example_long")
#' dtf_long <- ddabcd_data_example_long %index% list(1, 'eventname')
#' # Plot data for 9 participants taken at random
#' int_participants <- sample( 1:48, size = 9 )
#' ddabcd_plot_multi_panel(
#'   dtf_long %index% int_participants
#' )
#'
#' # Fit data for 9 participants
#' lst_all_fits.nls <- ddabcd_fit_models_across_participants(
#'   dtf_long, ddabcd_fit_models_using_nls, lgc_progress = FALSE
#' )
#' ddabcd_plot_multi_panel(
#'   lst_all_fits.nls
#' )
#' }
#'
#' @export


ddabcd_plot_multi_panel <- function(
    obj_data,
    chr_participant = 'src_subject_id',
    int_panels = 9,
    num_dim = c( width = 9, height = 9 ),
    num_margin = c( 3.35, 3.35, 1, 1 ),
    chr_xlab = 'ln(Delay)',
    chr_ylab = 'Indifference score',
    num_line = c( -1.1, -1.1 ),
    lgc_new = FALSE,
    ... ) {

  if ( is.data.frame(obj_data) ) {

    chr_participants <- unique( obj_data[[chr_participant]] )

    lst_index <- lapply(
      chr_participants, function(s) {
        return( obj_data[[chr_participant]] %in% s )
      }
    )

  } else {

    chr_participants <- names( obj_data )
    lst_index <- lapply(
      chr_participants, function(s) {
        return( s )
      }
    )

  }

  int_panels_rows <- ceiling( sqrt( int_panels ) )
  int_panels <- int_panels_rows^2

  # Ensure dimensions for plotting window are labeled
  if ( is.null( names(num_dim) ) ) {

    names( num_dim ) <- c( 'width', 'height' )

    # Close 'Ensure dimensions for plotting window are labeled'
  }

  if ( lgc_new ) {
    x11( width = num_dim['width'], height = num_dim['height'] )
  }

  mat_layout <- matrix(
    1:int_panels, int_panels_rows, int_panels_rows, byrow = TRUE
  )
  layout( mat_layout )

  # Loop over panels
  for (p in 1:int_panels) {

    # If data available to plot
    if ( p <= length( lst_index ) ) {

      # If data frame
      if ( is.data.frame(obj_data) ) {

        ddabcd::ddabcd_plot_single_panel(
          obj_data[ lst_index[[p]], ],
          num_margin = num_margin,
          chr_xlab = '',
          chr_ylab = '',
          lgc_new = FALSE,
          ...
        )

        # Close 'If data frame'
      } else {

        ddabcd::ddabcd_plot_single_panel(
          obj_data[[ lst_index[[p]] ]],
          num_margin = num_margin,
          chr_xlab = '',
          chr_ylab = '',
          lgc_new = FALSE,
          ...
        )

        # Close else for 'If data frame'
      }

      # Close 'If data available to plot'
    } else {

      # Blank plot
      plot( c(0, 1), c(0, 1),
            xaxt = 'n', yaxt = 'n',
            xlab = '', ylab = '',
            type = 'n' )

    }

    # Close 'Loop over panels'
  }

  mtext( chr_xlab, side = 1, line = num_line[1], outer = TRUE )
  mtext( chr_ylab, side = 2, line = num_line[1], outer = TRUE )

}

