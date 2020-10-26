library(dplyr)
options(scipen = 999)



round5 <- function(thing_to_round) {return(round(thing_to_round, 5))}



sigstars <- function(pval) {
  stars <- ifelse(
    pval < .001, "***", ifelse(
      pval < .01, "**", ifelse(
        pval < .05, "*", "")))
  return(stars)
}
