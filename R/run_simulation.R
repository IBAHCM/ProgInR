#' @title Run a simulation loop
#'
#' @description
#' A generic function to run a simulation loop for a fixed period of time.

#' @param step_function - function to run a timestep (\code{step_function()})
#'   which returns a list containing updated.pop with the updated population and
#'   end.experiment which is TRUE if the experiment has ended, OR which just
#'   returns a data frame with the updated population
#' @param latest.df - data frame with columns corresponding to function
#'   requirements
#' @param end.time - end time of simulation
#' @param debug - Do you want to print out a limited amount of debugging
#'   information about your code? - default FALSE
#' @param ... - any other arguments for \code{step_function()}, e.g. parameters
#'   or timestep
#'
#' @return data frame containing population history of simulation over time
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
#' results <- run_simulation(growth, df, 100, growth.rate=0.1, debug=TRUE)
#' plot_populations(results)
#'
run_simulation <- function(step_function, latest.df, end.time, debug=FALSE, ...)
{
  # Check whether step_function uses global variables
  if (length(findGlobals(step_function, merge=FALSE)$variables) > 0)
    warning(paste("Function provided uses global variable(s):",
                  paste(findGlobals(step_function, merge=FALSE)$variables,
                        collapse=", ")))

  # Collect and report debugging information to identify sources of errors
  pop.names <- colnames(latest.df)
  if (debug)
  {
    cat(c("Population names being used: ",
          paste(pop.names, collapse=", "), "\n"))
    if (nrow(latest.df) != 1)
      warning("Input dataframe latest.df has ", nrow(latest.df), " rows")
    cat(c("Parameter names being used: ",
          paste(names(c(...)), collapse=", "), "\n"))
  }

  population.df <- latest.df
  keep.going <- (latest.df$time < end.time)

  if (debug)
  {
    data <- step_function(population.df, ...)
    if (is.data.frame(data))
    { # We have an experiment that doesn't end, or can't determine when it does
      latest.df <- data
    }
    else # a list
    { # We have an experiment that can determine when it ends
      latest.df <- data$updated.pop
      ended <- data$end.experiment
    }

  }
  first.run <- TRUE
  while (keep.going)
  {
    data <- step_function(latest.df, ...)
    if (is.data.frame(data))
    { # We have an experiment that doesn't end, or can't determine when it does
      latest.df <- data
      ended <- FALSE
    }
    else # a list
    { # We have an experiment that can determine when it ends
      latest.df <- data$updated.pop
      ended <- data$end.experiment
      if (debug && first.run)
      {
        cat("end.experiment returned from first run: ", ended, "\n")
      }
    }

    if (debug)
    {
      if (first.run)
      {
        first.run <- FALSE
        cat("Population returned from first run: ",
            paste(latest.df, collapse=", "),
            "\n")
        ret.names <- colnames(latest.df)
        if (any(ret.names != pop.names))
        {
          cat("Population names being used in output: ",
              paste(ret.names, collapse=", "), "\n")
          if (any(sort(ret.names) != sort(pop.names)))
            stop("Mismatch in input and output population dataframe column names")
          else
            print("Input and output dataframe column names in different order")
        }
      }
      if (nrow(latest.df) != 1)
        cat("Output dataframe has ", nrow(latest.df), " rows\n")
      if (any(is.na(latest.df)))
        cat("Output dataframe has NAs: ", paste(latest.df, collapse=", "), "\n")
    }
    population.df <- rbind(population.df, latest.df)
    keep.going <- (latest.df$time < end.time) && (!ended)
  }
  row.names(population.df) <- NULL
  population.df
}
