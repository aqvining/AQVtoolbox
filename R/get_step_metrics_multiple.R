#' @title Movement Metric Calculator
#'
#' @description calculates distance traveled between all consecutive points (rows) in the data frame, with breaks by individual identifiers
#'
#' @param movement_data a data_frame with at least 3 columns containing individual identifiers, x, and y coordinate data.
#' @param id character giving name of column with unique identifiers
#' @param x character giving name of column with x coordinates
#' @param y character giving name of column with y coordinates
#' @param step_number name of a column with the ordered number of steps. Used to ensure proper sorting
#'
#' @return data frame
#' @export
#' @importFrom dplyr group_by_at mutate
#' @importFrom purrr map
#' @importFrom tidyr nest unnest '%>%'
#'
#' @examples
#' move_data1 <- data.frame(x = rnorm(10), y = rnorm(10), Individual = "Subject1", step = 1:10)
#' move_data2 <- data.frame(x = rnorm(10), y = rnorm(10), Individual = "Subject2", step = 1:10)
#' get_step_metrics_multiple(rbind(move_data1, move_data2), id = "Individual", x = "x", y = "y", step_number = "step")
get_step_metrics_multiple <- function(movement_data, id = "ID", x = "X", y = "Y", step_number = NA) {
  movement_data <- movement_data %>%
    group_by_at(id) %>%
    nest() %>% #break dataframe into groups by ID and store into nested data frame for function mapping
    mutate(data = map(data, get_step_metrics_individual, x, y , step_number)) %>% #apply get_step_metrics_individual to each each data frame stored in data column of nested data frame
    unnest(cols = c(data))
  return(movement_data)
}
