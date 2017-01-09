#' @title Run a simulation loop
#'
#' @description
#' A generic function to run a simulation loop for a fixed period of time.

#' @param step_function - function to run a timestep (step_function()) which returns
#'     a list containing updated.pop with the updated population and
#'     end.experiment which is TRUE if the experiment has ended, OR which just
#'     returns a data frame with the updated population
#' @param latest.df - data frame with columns corresponding to function requirements
#' @param end.time - end time of simulation
#' @param ... - any other arguments for step_function(), e.g. parameters or timestep
#'
#' @return
#' output of final step of simulation, including all of the time steps up to now
#'
#' @export
#'
#' @examples
#'
#' growth <- function(latest.df, growth.rate) {
#' current.count <- latest.df$count
#' growth.num <- current.count * growth.rate
#' next.count <- current.count + growth.num
#' next.time <- latest.df$time + 1
#' new.df <- data.frame(time=next.time, count=next.count)
#' finished <- next.count == 0
#' list(updated.pop=new.df, end.experiment=finished)
#' }
#' df <- data.frame(time=0, count=1)
#' results <- run_simulation(growth, df, 100, growth.rate=0.1)
#' plot_populations(results)
#'
run_simulation <- function(step_function, latest.df, end.time, ...)
{
  if (length(findGlobals(step_function, merge=FALSE)$variables) > 0)
    warning(paste("Function provided uses global variable(s):",
                  paste(findGlobals(step_function, merge=FALSE)$variables,
                        collapse=", ")))
  population.df <- latest.df
  keep.going <- (latest.df$time < end.time)
  while (keep.going)
  {
    data <- step_function(latest.df, ...)
    latest.df <- data$updated.pop
    population.df <- rbind(population.df, latest.df)
    keep.going <- (latest.df$time < end.time) && (!data$end.experiment)
  }
  row.names(population.df) <- NULL
  population.df
}
