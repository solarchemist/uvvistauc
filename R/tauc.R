#' Calculates optical band gap using Tauc plot
#'
#' This function uses Tauc plotting to determine the optical band gap
#' from your supplied UV-Vis absorbance spectrum.
#'
#' @param energy, numeric vector, in eV
#' @param absorbance, numeric vector, unitless,
#'     must be same length as energy (optical absorbance)
#' @param r, exponent that determines transition type,
#'     0.5 for direct allowed (default) transition,
#'     1.5 for direct forbidden,
#'     2 for indirect allowed, and
#'     3 for indirect forbidden.
#' @param lowE.limits, vector of two numbers
#'     that define a straight line approximating the low-energy plateau.
#' @param highE.limits, vector of two numbers
#'     that define a straight line approximating the high-energy plateau.
#' @param bg.limits, vector of two numbers, default c(0.3, 0.8).
#'     that define the relative lower and upper bounds
#'     of points in the band edge to fit.
#' @return tibble with same number of rows as length of input vectors,
#'     with the following columns:
#'     + x, numeric scalar (identical to the input parameter "energy")
#'     + y, absorbance to the power of 1/r
#'       (y is the input parameter "absorbance" raised to 1/r)
#'     + ceiling, line that defines the "ceiling" that this function used
#'     + floor, line that defines the "floor" that this function used
#'     + edge, boolean, TRUE for the rows that are part of the Tauc fit
#'     + fit.Eg, the calculated optical band gap, in eV
#'     + fit.tauc, the fitted Tauc line
#'     + fit.adj_rsq, adjusted R-square of the linear Tauc fit
#'     + fit.points, the number of data points that are part of the Tauc fit (cf. edge)
#'     + r, numeric scalar (input param reproduced)
#'     + lowE.limits, numeric vector, as list column (input param reproduced)
#'     + highE.limits, numeric vector, as list column (input param reproduced)
#'     + bg.limits, numeric vector, as list column (input param reproduced)
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' tauc(energy, absorbance, lowE.limits = c(2.4, 3.4), highE.limits = c(3.9, 4.3))
#' tauc(energy, absorbance, r = 2, lowE.limits = c(2.2, 3.0), highE.limits = c(3.4, 3.8))
#' tauc(energy, absorbance, bg.limits = c(0.20, 0.85), lowE.limits = ..., highE.limits = ...)
#' }
tauc <- function(energy, absorbance, r = 0.5, lowE.limits, highE.limits, bg.limits = c(0.3, 0.8)) {

   ### Sanity checks on input parameters
      # I expect energy to be a numeric vector, stop otherwise
      stopifnot(all(varhandle::check.numeric(energy)))
      # I expect absorbance to be a numeric vector, stop otherwise
      stopifnot(all(varhandle::check.numeric(absorbance)))
      # energy and absorbance vectors must be the same length
      stopifnot(length(energy) == length(absorbance))
      # energy step size does not have to be uniform,
      # but internal logic in this function expects the vector to begin
      # at high E and end at low E
      if ((energy %>% dplyr::first()) < (energy %>% dplyr::last())) {
         message("Reversing the provided energy and absorbance vectors")
         energy <- rev(energy)
         absorbance <- rev(absorbance)
      }
      # lowE.limits, must be vector of length 2
      stopifnot(length(lowE.limits) == 2)
      # no NAs allowed
      stopifnot(!any(is.na(lowE.limits)))
      # must be numeric
      stopifnot(all(varhandle::check.numeric(lowE.limits)))
      # must not be equal
      stopifnot(diff(lowE.limits) != 0)
      # ensure vector is sorted as c(min, max)
      if (diff(lowE.limits) < 0) {lowE.limits <- rev(lowE.limits)}
      # highE.limits (same restrictions as above)
      stopifnot(length(highE.limits) == 2)
      stopifnot(!any(is.na(highE.limits)))
      stopifnot(all(varhandle::check.numeric(highE.limits)))
      stopifnot(diff(highE.limits) != 0)
      if (diff(highE.limits) < 0) {highE.limits <- rev(highE.limits)}
      # bg.limits (same restrictions as above)
      stopifnot(length(bg.limits) == 2)
      stopifnot(!any(is.na(bg.limits)))
      stopifnot(all(varhandle::check.numeric(bg.limits)))
      stopifnot(diff(bg.limits) != 0)
      if (diff(bg.limits) < 0) {bg.limits <- rev(bg.limits)}
      # r exponent, define allowed values
      r.values <- c(0.5, 1.5, 2, 3)
      if (!(r %in% r.values)) {
         message(paste0(
            "Your provided value of r = ", formatC(r), "is invalid!\n",
            "r must be one of ", paste(r.values, collapse = ", ")
         ))
         stop()
      }


   # Let's construct a tibble to populate with this function's results,
   # we will use the row numbers as a unique identifier for each row
   df <- tibble::tibble(uid = seq(energy), x = energy, y = absorbance^(1/r))

   # fit a straight line to the high-energy plateau, see where it intersects the band edge
   # estimate the "ceiling" of the bandgap by fitting a linear function to the high-energy plateau
   lm.ceiling <-
      df %>%
      dplyr::filter(
         (.data$x >= min(highE.limits)) &
         (.data$x <= max(highE.limits))) %>%
      stats::lm(formula = y ~ x)
   ceiling.intercept <- lm.ceiling$coefficients[1]
   ceiling.slope <- lm.ceiling$coefficients[2]
   # we extrapolate the fitted line to the full range of df (not just df.floor)
   df$y.ceiling <- ceiling.slope * df$x + ceiling.intercept

   # identify which spectral datapoints lie above the defined "ceiling"
   df$above.ceiling <- df$y > df$y.ceiling
   # issue a warning if none of the datapoints lie above the ceiling
   # (this warning is highly unlikely to happen)
   if (!any(df$above.ceiling == TRUE)) {
      warning("No datapoints above ceiling")
   }
   # we look for the intersection between the spectrum and the defined ceiling
   # in order to identify the top of the band edge of the spectrum
   # intersect occurs when two subsequent elements in vector above.ceiling
   # flip from TRUE to FALSE or vice versa
   # note the vector may contain more than one intersect (the spectrum
   # can dip and rise over the ceiling multiple times)
   # so consider only the first intersect that occurs above max(lowE.limits)
   this.ceiling <- df %>% dplyr::filter(.data$x > max(lowE.limits))
   # index of the "ceiling" intersect point
   uid.ceiling <-
      which(diff(this.ceiling$above.ceiling) != 0)[
         length(which(diff(this.ceiling$above.ceiling) != 0))]


   # Fit a straight line to the low-energy plateau,
   # then find the y-value at the midpoint between lowE.limits max and min
   # We will use this y-value as the "floor" of the band edge
   this.floor <-
      df %>%
      dplyr::filter(.data$x >= min(lowE.limits) & .data$x <= max(lowE.limits))
   lm.floor <-
      this.floor %>%
      stats::lm(formula = y ~ x)
   floor.intercept <- lm.floor$coefficients[1]
   floor.slope <- lm.floor$coefficients[2]
   # extrapolate the fitted line to the full range of df (in case we want to visualise)
   df$y.floor <- floor.slope * df$x + floor.intercept
   # note on the chosen approach to calculate "floor":
   # easiest way would be to calculate the average y inside the fitted range,
   # but I need the uid of the row that lies at the midpoint
   # number of rows inside the fitted range
   this.rows <- this.floor %>% dplyr::count() %>% dplyr::pull()
   # one row up/down does not matter, so just round the resultant row.no to integer
   # index of the "floor" intersect point
   uid.floor <-
      this.floor %>%
      dplyr::slice(round(this.rows * 0.5)) %>%
      dplyr::pull(.data$uid)


   # mark the identified "ceiling" and "floor" in a new column
   # to make this assignment more robust, we compare uid.ceiling to df$uid
   # this way, the assignment is expected to be correct even if uid does not match df row numbers
   # https://reddit.com/r/RStudio/comments/pgq0lb/how_do_i_insert_a_value_in_a_column/
   df %<>%
      dplyr::mutate(
         intersection =
            ifelse(.data$uid == uid.ceiling, "ceiling",
               ifelse(.data$uid == uid.floor, "floor", "")))


   stopifnot(length(which(df$intersection %in% c("ceiling", "floor"))) == 2)
   # armed with the "ceiling" and "floor" points, we now apply
   # the provided bg.limits to define the band edge region we will fit
   y.ceiling <-
      df %>%
      dplyr::filter(.data$uid == uid.ceiling) %>%
      dplyr::pull(.data$y)
   y.floor <-
      df %>%
      dplyr::filter(.data$uid == uid.floor) %>%
      dplyr::pull(.data$y)
   y.ceiling2floor <- y.ceiling - y.floor
   # note: even if the spectrum extends far into UV (where the abs may dip again)
   # uid.floor/this.floor and uid.ceiling/this.ceiling, as currently implemented,
   # will correctly find *only* the band edge region
   # Next, if we are inside the y-range defined by bg.limits, and inside the
   # x-range bracketed by max(lowE.limits) and min(highE.limits), set edge to TRUE
   df %<>%
      dplyr::mutate(
         edge =
            ifelse(
               (.data$y <= max(bg.limits) * y.ceiling2floor + y.floor) &
               (.data$y >= min(bg.limits) * y.ceiling2floor + y.floor) &
               (.data$x > max(lowE.limits) & .data$x < min(highE.limits)),
               TRUE, FALSE))

   # now we fit a linear model to the rows where edge == TRUE
   df.tauc <-
      df %>%
      dplyr::filter(.data$edge == TRUE) %>%
      # no need to keep other columns in this temporary df
      dplyr::select(.data$uid, .data$x, .data$y)
   # this is our Tauc fit
   lm.tauc <-
      df.tauc %>%
      stats::lm(formula = y ~ x)
   tauc.intercept <- lm.tauc$coefficients[1]
   tauc.slope <- lm.tauc$coefficients[2]
   # x-intercept of fitted line is the optical band gap, Eg = -b / k
   df.tauc$fit.Eg <- -as.numeric(tauc.intercept) / as.numeric(tauc.slope)
   df.tauc$fit.slope <- as.numeric(tauc.slope)
   df.tauc$fit.intercept <- as.numeric(tauc.intercept)
   df.tauc$fit.adj_rsq <- summary(lm.tauc)$adj.r.squared
   # number of datapoints used for Tauc linear fit (quality of fit indicator)
   df.tauc$fit.points <- df.tauc %>% dplyr::count() %>% dplyr::pull()

   # now assign these back to df, use uid to join
   df %<>%
      dplyr::left_join(
         df.tauc %>% dplyr::select(.data$uid, dplyr::starts_with("fit")),
         by = "uid")
   # that looks ok, except the fit.* columns only contain
   # data inside the edge==TRUE range, all rows outside being NA
   # since the data is constant across the spectrum,
   # let's replace all NA's in each column with the value
   # to avoid unnecessary NA's
   df$fit.Eg <- df$fit.Eg[which(!is.na(df$fit.Eg))] %>% unique()
   df$fit.slope <- df$fit.slope[which(!is.na(df$fit.slope))] %>% unique()
   df$fit.intercept <- df$fit.intercept[which(!is.na(df$fit.intercept))] %>% unique()
   df$fit.adj_rsq <- df$fit.adj_rsq[which(!is.na(df$fit.adj_rsq))] %>% unique()
   df$fit.points <- df$fit.points[which(!is.na(df$fit.points))] %>% unique()
   # extrapolate the fitted Tauc line
   df$fit.tauc <- df.tauc$fit.slope[1] * df$x + df.tauc$fit.intercept[1]

   # Return the calculated values as well as the input parameters
   return(
      df %>%
      dplyr::select(
         .data$x,
         .data$y,
         ceiling = .data$y.ceiling,
         floor   = .data$y.floor,
         .data$edge,
         .data$fit.Eg,
         .data$fit.tauc,
         .data$fit.adj_rsq,
         .data$fit.points) %>%
      # input parameters
      dplyr::mutate(
         r = r,
         lowE.limits = list(lowE.limits),
         highE.limits = list(highE.limits),
         bg.limits = list(bg.limits))
   )
}
