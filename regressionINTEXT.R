##########

cat("\n####################")
cat("\nLoading Nadya's in-text support for regressions from Github.")
cat("\n    Version : 0.0.1.9003 (for R version 3.6.3)")
cat("\nLast update : 30 May 2021, 6:14am")
cat("\n")

library(dplyr)

##########



intext_regression = function(
  regression.output, varname = NULL, round = 2, round_ci = round,
  add_beta = TRUE, add_ci = TRUE, add_intercept = FALSE) {
  
  ##### sub-functions #####
  
  intext_regCoeffs = function(b, se) {return(paste0("b = ", b, ", SE = ", se, sep = ""))}
  
  intext_CI = function(cilower, ciupper) {return(ci = paste0("95% CI = [", cilower, ", ", ciupper, "]", sep = ""))}
  
  intext_regression_single = function(
    regression.output, varname = NULL, round = 2, round_ci = round,
    add_beta = TRUE, add_ci = TRUE, add_intercept = FALSE) {
    
    res = regression.output
    if(!("stdcoeff" %in% colnames(res))) {
      # there is no beta column
      add_beta = FALSE
      res = res %>% dplyr::mutate(betaholder = NA) %>% dplyr::select(variable, betaholder, everything())
    }
    
    # if varname is not set, assume first non-intercept term (usually second row) is desired
    if(is.null(varname)) {rownum = 2}
    # if varname is set, find it
    else {
      # if varname is not given as a character, stop function and tell user
      if(!is.character(varname)) {stop("varname not recognised. Pass in the name of a term in character class.")}
      else {
        # find row number of the variable
        rownum = match(varname, res$variable)
        # if row number cannot be found, stop function and tell user
        if(is.na(rownum)) {stop("varname not recognised. Pass in the name of a term in the equation.")}
      }
    }
    
    # retrieve varname
    varname = res[rownum, 1]
    
    # function to force rounding later
    forceround = function(thing_to_round, dp) {
      number = round(as.numeric(thing_to_round), as.numeric(dp))
      number = format(number, nsmall = dp)
      return(number)
    }
    
    # check for std coeff
    if(add_beta) {
      beta = res[rownum, "stdcoeff"] %>% forceround(dp = round) %>% trimws()
      beta_info = paste0("β = ", beta, ", ", sep = "")
    }
    else {beta_info = ""}
    
    # get b and se
    b = res[rownum, "coeff"] %>% forceround(dp = round) %>% trimws()
    se = res[rownum, "se"] %>% forceround(dp = round) %>% trimws()
    bs = intext_regCoeffs(b, se)
    
    # get ci
    if(add_ci) {
      if(ncol(res %>% dplyr::select(contains("CI95"))) != 2) {
        cat("\nNo 95% CIs found.\n")
        ci95 = ""
      }
      else {
        ci95 = res[rownum, c("CI95lower", "CI95upper")] %>% forceround(dp = round_ci) %>% trimws()
        while(ci95[1] == 0 | ci95[2] == 0) {
          round_ci = round_ci + 1
          ci95 = res[rownum, c("CI95lower", "CI95upper")] %>% forceround(dp = round_ci) %>% trimws()
          }
        ci95 = paste0(intext_CI(ci95[1], ci95[2]), ", ", sep = "")
      }
    }
    else {ci95 = ""}
    
    # get pval
    pval = res[rownum, "p"]; if(length(pval) == 0) pval = res[rownum, "pval"]
    psegment = intext_p(pval)
    
    # get intercept (used for simple slopes context)
    if(add_intercept) {
      intercept_row = match("(Intercept)", res$variable)
      intercept_value = res[intercept_row, "coeff"] %>% forceround(dp = round) %>% trimws()
      intercept_info = paste0(", intercept = ", intercept_value, sep = "")
    }
    else {intercept_info = ""}
    
    # paste everything together and return
    return(paste0(varname, " ", beta_info, bs, ", ", ci95, psegment, intercept_info, sep = ""))
  }
  
  ##### start of main function #####
  
  if(is.null(varname) | length(varname) == 1) {return(intext_regression_single(regression.output, varname = varname, round = round, add_beta = add_beta, add_ci = add_ci, add_intercept = add_intercept))}
  else {
    collated = list()
    for(v in varname) {collated[[v]] = intext_regression_single(regression.output, varname = v, round = round, add_beta = add_beta, add_ci = add_ci, add_intercept = add_intercept)}
    return(collated)
  }
}



##########

cat("Success.")
cat("\n####################")

##########
