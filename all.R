cat("\n####################")
cat("\nLoading Nadya's functions and other QOL upgrades from Github.")
cat("\nLast updated: 15 Nov 2020, 11:32pm")
cat("\nPackage(s)  : dplyr")
cat("\nOption(s)   : Prevent scientific notation.")
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



dS <- function(varname, label = FALSE, dummy = FALSE, compatible = FALSE) {
  
  n = sum(!is.na(varname))
  
  if(dummy) {
    yes = sum(varname == 1, na.rm = T)
    no = sum(varname == 0, na.rm = T)
    if(yes==0 & no==0) {stop("\nCategorical variables should be dummy-coded in 0/1. Neither found.\n")}
    else {
      percentage = round4(yes / (yes + no)) * 100
      if(!compatible) {out = data.frame('n' = n, 'percentage' = percentage)}
      if(compatible) {out = data.frame('n' = n, 'value' = percentage, 'sd' = NA, 'min' = NA, 'max' = NA)}
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
      if(compatible) {out = out %>% dplyr::rename(value = m)}
    }
  }
  
  if(label != FALSE) {rownames(out) = label}
  return(out)
}



dS.full <- function(data, exclude = NULL, print = TRUE, csv = TRUE, debug = FALSE) {
  
  if(!is.data.frame(m)) stop("Please pass in a data.frame.")
  
  # exclude variables if requested
  if(!is.null(exclude)) {
    exclusions = data %>% dplyr::select(contains(exclude)) %>% colnames()
    cat("Excluding the following columns:", exclusions, "\n")
    data = data %>% dplyr::select(-contains(exclude))
    }
  
  # retrieve colnames
  vars = colnames(data)
  if(debug) {print(vars)}
  
  # extract descriptives
  out = data.frame()
  for(current_var in vars) {
    
    # extract values in column
    current_values = data[, current_var]
    
    # check if column is numeric first, proceed if yes, otherwise skip
    if(is.numeric(current_values)) {
      
      # check if column is dummy coded
      dummycheck = sum(current_values != 0 & current_values != 1, na.rm = T)
      dummy = ifelse(dummycheck == 0, TRUE, FALSE)
      
      # prepare label
      label = current_var
      if(dummy) {label = paste(current_var, "(%)")}
      
      # run dS for current variable
      current_dS = dS(current_values, dummy = dummy, compatible = TRUE, label = label)
      
      # bind back to table of descriptives
      out = rbind(out, current_dS)
    }
    
    else {cat("\nSkipping", current_var, "as it is not numeric.\n")}
  }
  
  if(print){cat("\n"); print(out)}
  
  if(csv){
    cat("\nWriting descriptives.csv into the working directory.\n")
    write.csv(out, "descriptives.csv")
    cat("Done!\n")
  }
  
  invisible(out)
}



winsorSD <- function(values, numSD = 3, debug = FALSE) {
  m = mean(values, na.rm = TRUE)
  oneSD = sd(values, na.rm = TRUE)
  if(debug){cat("\nMean = ", m, ", SD = ", oneSD, "\n", sep = "")}
  
  margin = oneSD * numSD
  lowerbound = m - margin
  upperbound = m + margin
  if(debug){cat("\nMargin = ", margin, ", Bounds = [", lowerbound, ", ", upperbound, "]\n", sep = "")}
  
  out = values
  out[out < lowerbound] = lowerbound
  out[out > upperbound] = upperbound
  invisible(out)
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
