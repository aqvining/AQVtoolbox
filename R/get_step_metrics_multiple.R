get_step_metrics_multiple <- function(movement_data, id = "ID", x = "X", y = "Y", step_number = NA) {
  #input: movement_data; a data_frame with at least 3 columns containing individual identifiers, x, and y coordinate data.
  #       id; character givine name of column with unique identifyers
  #       x; character giving name of column with x coordinates
  #       y; character giving name of column with y coordinates
  #       step_number: name of a column with the ordered number of steps. Used to ensure proper sorting
  #output: a data frame of same structure as individual_movement_data with an additional columns named "Step_length" and "Step_Turn"
  #description: calculates distance traveled between all consecutive points (rows) in the data frame, with breaks by individual identifiers
  movement_data <- movement_data %>% group_by_at(id) %>% nest() %>% #break dataframe into groups by ID and store into nested data frame for function mapping
    mutate(data = map(data, get_step_metrics_individual, x, y , step_number)) %>% #apply get_step_metrics_individual to each each data frame stored in data column of nested data frame
    unnest(cols = c(data))
  return(movement_data)
}
