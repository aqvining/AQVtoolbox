#' @title Modify by Group
#' @author Alexander Vining & Katherine Stewart
#' 
#' @description modifies a dataframe by applying a transformative function accross groups
#'
#' @importFrom dplyr group_by_at mutate
#' @importFrom purrr map
#' @importFrom tidyr nest unnest '%>%'
#' @param raw_data a dataframe
#' @param groups a vector of character strings giving names of columns to group by. Use NA if raw_data is already grouped
#' @param FUNC a function that takes a dataframe is input or a formula that can be converted to such a function.
#'
#' @return a dataframe, form determined by the function in FUNC
#' @export
#' @examples
#' \dontrun{ 
#' sean_tracks2 <- read.csv("//10.126.19.90/EAS_ind/avining/data/Misc/sean_tracks2.csv")
#' sean_tracks3 <- modify_by_group(sean_tracks2, c("track_name_old"), ~mutate(., waypoint_number = seq_along(timestamp2)))
#' FFT_subset <- read.csv("//10.126.19.90/EAS_ind/avining/R Workshops/Session 3/FFT_subset.csv")
#' FFT_subset <- modify_by_group(FFT_subset, c("individual.local.identifier", "day"), ~get_step_metrics_individual(., x = "location.long", y = "location.lat"))
#' }

modify_by_group <- function(raw_data, groups, FUNC) {
  if (! is.data.frame(raw_data)) stop("raw_data must be a dataframe")
  if (! is.na(groups)) raw_data <- raw_data %>% group_by_at(groups)
  raw_data %>% nest() %>% 
  #creates a dataframe for each group containing relevant rows from raw_data. Stores these dataframes in a list, as a column (named data) in a dataframe 
  ##this full result has a nested structure of dataframes inside a dataframe.
  mutate(data = map(data, FUNC)) %>% 
  ##The syntax of map passes each element of data (the list of data in raw_data filtered by groups) to FUNC.
  unnest(cols=c(data))
}

sean_tracks2 <- read.csv("//10.126.19.90/EAS_ind/avining/data/Misc/sean_tracks2.csv")
sean_tracks3 <- modify_by_group(sean_tracks2, c("track_name_old"), ~mutate(., waypoint_number = seq_along(timestamp2)))





