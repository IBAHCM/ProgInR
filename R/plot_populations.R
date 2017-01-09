#' @title Plot population(s) against time
#'
#' @description
#' Plot all of the populations in a data frame against time. One column must
#' be called "time" and will be used as the x-axis of the plot. The rest will
#' be used as different lines on the y-axis, with a legend denoting their
#' column names.
#'
#' @param populations - data frame with columns corresponding to different population
#'                   segments and a 'time' column
#' @param new.graph - (optionally) whether to start a new graph, default TRUE
#' @param ylim - (optionally, for new graphs) the limits of the y axis, default min to max pop size
#' @param lty - (optionally) the line type for the graph, default 1
#' @param col - (optionally) the colour for all lines - default 1:num.populations
#'    you can name these c(susceptibles="green", ...)
#' @param ... - (optionally) any other arguments that plot and lines will both accept
#'
#' @export
#'
#' @examples
#'
#' df <- data.frame(time=0:100, grow=exp((0:100) / 10), die=exp(seq(10, 0, by = -0.1)))
#' plot_populations(df, lty = 2, main = "A title")
#'
plot_populations <- function(populations, new.graph=TRUE,
                             ylim=NA, lty=1, col=NA, ...)
{
  if (any(colnames(populations)=="time"))
  {
    time <- populations$time
    populations$time = NULL
  }
  else
    stop("No time available")

  labels <- colnames(populations)
  if (is.na(col[1]))
    line.cols <- 1:length(labels)
  else
  {
    if (all(labels(col)==1:length(labels)))
      line.cols <- col
    else
    {
      line.cols <- c()
      for (name in labels)
        line.cols <- c(line.cols, col[name])
    }
  }

  if (is.na(ylim[1]))
    ylim <- c(0, max(rowSums(populations)))

  index <- 1
  for (label in labels)
  {
    this.pop <- populations[[label]]
    if (new.graph)
    {
      plot(time, this.pop,
           ylim=ylim, xlab='time', ylab='population size',
           type='l', col=line.cols[index], lty=lty, ...)
      legend("topright", legend=labels, lty=lty, col=line.cols)
      new.graph <- FALSE
    }
    else
      lines(time, this.pop, col=line.cols[index], lty=lty, ...)
    index <- index + 1
  }
}
