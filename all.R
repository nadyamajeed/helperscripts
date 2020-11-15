cat("\n####################")
cat("\nLoading Nadya's functions and other QOL upgrades from Github.")
cat("\Last updated: 15 Nov 2020")
cat("\nPackage(s) : dplyr")
cat("\nOption(s)  : Prevent scientific notation.")
cat("\n")

starttime = Sys.time()

##########



library(dplyr)
options(scipen = 9999)



roundx <- function(thing_to_round, dp, force = FALSE) {
  if(!is.logical(force)) stop("'force' must be set to TRUE or FALSE (default FALSE).")
  number = round(as.numeric(thing_to_round), as.numeric(dp))
  if(force) {number = format(number, nsmall = dp)}
  return(number)
}
round2 <- function(thing_to_round, force = FALSE) {return(roundx(thing_to_round, 2, force = force))}
round3 <- function(thing_to_round, force = FALSE) {return(roundx(thing_to_round, 3, force = force))}
round4 <- function(thing_to_round, force = FALSE) {return(roundx(thing_to_round, 4, force = force))}
round5 <- function(thing_to_round, force = FALSE) {return(roundx(thing_to_round, 5, force = force))}



sigstars <- function(pval) {
  pval = as.numeric(pval)
  stars = ifelse(
    pval < .001, "***", ifelse(
      pval < .01, "**", ifelse(
        pval < .05, "*", "")))
  return(stars)
}



dS <- function(varname, label = FALSE, dummy = FALSE) {
  
  n = sum(!is.na(varname))
  
  if(dummy) {
    yes = sum(varname == 1, na.rm = T)
    no = sum(varname == 0, na.rm = T)
    if(yes==0 & no==0) {stop("\nCategorical variables should be dummy-coded in 0/1. Neither found.\n")}
    else {
      percentage = round4(yes / (yes + no))
      out = data.frame('n' = n, 'percentage' = percentage)
    }
  }
  
  else {
    if(!is.numeric(varname)) {stop("\nVariable is not numeric.\nPlease convert the variable, check the variable name, or set categorical = TRUE to compute descriptives for (dummy-coded) categorical variables.\n")}
    else {
      m = mean(varname, na.rm = T) %>% round2()
      sd = sd(varname, na.rm = T) %>% round2()
      min = min(varname, na.rm = T) %>% round2()
      max = max(varname, na.rm = T) %>% round2()
      out = data.frame('n' = n, 'm' = m, 'sd' = sd, 'min' = min, 'max' = max)
    }
  }
  
  if(label != FALSE) {rownames(out) = label}
  return(out)
}



winsorSD <- function(values, SD = 3) {
  m = mean(values, na.rm = TRUE)
  oneSD = sd(values, na.rm = TRUE)
  
  margin = oneSD * SD
  lowerbound = m - margin
  upperbound = m + margin
  
  out = values
  out[out < lowerbound] = lowerbound
  out[out > upperbound] = upperbound
  return(out)
}



summary.t <- function(t.test.output) {
  if(class(t.test.output) != "htest") stop("Needs output from t.test() function.")
  degrees_of_freedom = t.test.output$parameter
  t_statistic = t.test.output$statistic
  p_value = t.test.output$p.value
  res.t = paste0("t(", degrees_of_freedom, ") = ", round2(t_statistic, force = TRUE), ", p = ", round3(p_value, force = TRUE))
  return(res.t)
}



##########

endtime = Sys.time()
cat("\nFinished loading Nadya's QOL upgrades.")
cat("\nTime taken:", (endtime - starttime))
cat("\n####################")
cat("\n")

rm(starttime); rm(endtime)
