get_step_metrics_individual <- function(individual_movement_data, x = "X", y = "Y", step_number = NA) {
  #input: individual movement data; a data_frame with at least two columns containing x and y coordinate data.
  #       x; character giving name of column with x coordinates
  #       y; character giving name of column with y coordinates
  #       step_number: name of a column with the ordered number of steps. Used to ensure proper sorting
  #output: a data frame of same structure as individual_movement_data with an additional columns named "Step_length" and "Step_Turn", the latter in radians
  #description: calculates distance traveled between all consecutive points (rows) in the data frame
  if (! is.na(step_number)) individual_movement_data <- individual_movement_data[order(individual_movement_data[,step_number]),] #sort by step number
  diff_x <- diff(individual_movement_data[[x]])
  diff_y <- diff(individual_movement_data[[y]])

  individual_movement_data$Step_length <- c(NA, mapply(FUN = function(a,b) sqrt(a^2 + b^2), a = diff_x, b = diff_y)) #first row is NA because no step has been taken, subsequent rows are hypotenuse of delta x and delta y

  step_angles <- mapply(atan2, y = diff_y, x = diff_x)
  step_turns <- (diff(step_angles) + pi) %% (2*pi) - pi #scales all turns from -pi to pi, rather than -2pi to 2pi which results from simple distancing, e.g. turns of -pi/2 and 3pi/2 will both be converted to -pi/2
  individual_movement_data$Step_turn <- c(NA, NA, step_turns)
  return(individual_movement_data)
}
