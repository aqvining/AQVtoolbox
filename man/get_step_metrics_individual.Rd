% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_step_metrics_individual.R
\name{get_step_metrics_individual}
\alias{get_step_metrics_individual}
\title{Movement Metric Calculator Individual}
\usage{
get_step_metrics_individual(
  individual_movement_data,
  x = "X",
  y = "Y",
  step_number = NA
)
}
\arguments{
\item{individual_movement_data}{data_frame with at least two columns containing x and y coordinate data. If these columns are not named "X"and "Y" the names of the columns must be given to the x and y parameters}

\item{x}{character}

\item{y}{character}

\item{step_number}{character, gives column name of step numbers in individual movement data}
}
\value{
data frame
}
\description{
calculates distance traveled between all consecutive points (rows) in the data frame
}
