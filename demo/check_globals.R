### Does the function works without any external (global) information?
library(codetools)
library(ProgInR)

if (length(findGlobals(plot_populations, merge=FALSE)$variables) != 0)
{
  stop("Function plot_populations() may not use global variable(s): ",
       findGlobals(plot_populations, merge=FALSE)$variables)
}
