#' Movement Metric Calculator Individual
#'
#' calculates distance traveled between all consecutive points (rows) in the data frame
#'
#' @param individual_movement_data data_frame with at least two columns containing x and y coordinate data. If these columns are not named "X"and "Y" the names of the columns must be given to the x and y parameters
#' @param x character
#' @param y character
#' @param step_number character, gives column name of step numbers in individual movement data
#'
#' @return data frame
#' @export
#'
get_step_metrics_individual <- function(individual_movement_data, x = "X", y = "Y", step_number = NA) {
  if (! is.na(step_number)) individual_movement_data <- individual_movement_data[order(individual_movement_data[[step_number]]),] #sort by step number
  diff_x <- diff(individual_movement_data[[x]])
  diff_y <- diff(individual_movement_data[[y]])

  individual_movement_data$Step_length <- c(NA, mapply(FUN = function(a,b) sqrt(a^2 + b^2), a = diff_x, b = diff_y)) #first row is NA because no step has been taken, subsequent rows are hypotenuse of delta x and delta y

  step_angles <- mapply(atan2, y = diff_y, x = diff_x)
  step_turns <- (diff(step_angles) + pi) %% (2*pi) - pi #scales all turns from -pi to pi, rather than -2pi to 2pi which results from simple distancing, e.g. turns of -pi/2 and 3pi/2 will both be converted to -pi/2
  individual_movement_data$Step_turn <- c(NA, NA, step_turns)
  return(individual_movement_data)
}
