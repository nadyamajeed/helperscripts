devtools::source_url("https://raw.githubusercontent.com/nadyamajeed/helperscripts/main/all.R")

##########

cat("\n####################")
cat("\nLoading Nadya's t-test upgrades (with Bayesian t-test!) from Github.")
cat("\n            Version : 0.0.2.9000")
cat("\n        Last update : 23 May 2021, 9:52am")
cat("\nRequired Package(s) : BayesFactor, effectsize")
cat("\n")

starttime = Sys.time()

##########



intext_t = function(t.test.output) {
  if(class(t.test.output) != "htest") stop("Needs output from t.test() function.")
  degrees_of_freedom = t.test.output$parameter
  t_statistic = t.test.output$statistic
  p_value = t.test.output$p.value
  return(paste0("t(", degrees_of_freedom, ") = ", round2(t_statistic, force = TRUE), ", ", intext_p(p_value)))
}



tBF10 = function(ttestBF.output) {
  BF10 = ttestBF.output@bayesFactor[["bf"]]
  if(BF10 < 0.01) {evcat = "Decisive evidence for H0"}
  else if(BF10 < 0.03) {evcat = "Very strong evidence for H0"}
  else if(BF10 < 0.10)  {evcat = "Strong evidence for H0"}
  else if(BF10 < 0.33)  {evcat = "Substantial evidence for H0"}
  else if(BF10 < 1.00)  {evcat = "Anecdotal evidence for H0"}
  else if(BF10 == 1)  {evcat = "No evidence"}
  else if(BF10 < 3)  {evcat = "Anecdotal evidence for H1"}
  else if(BF10 < 10)  {evcat = "Substantial evidence for H1"}
  else if(BF10 < 30)  {evcat = "Strong evidence for H1"}
  else if(BF10 < 100)  {evcat = "Very strong evidence for H1"}
  else if(BF10 >= 100)  {evcat = "Decisive evidence for H1"}
  return(list(BF10 = BF10, evcat = evcat))
}



intext_t_bayes = function(ttestBF.output) {
  out = tBF10(ttestBF.output)
  BF10 = out$BF10 %>% round2(force = TRUE)
  return(paste0("BF10 = ", BF10, ", ", out$evcat, sep = ""))
}



ttestFNB = function(
  col1 = NULL, col2 = NULL,
  formula = NULL, data = NULL,
  paired = NULL, var.equal = TRUE,
  d = TRUE,
  print = TRUE, viewtable = FALSE) {
  
  if(is.null(col1) & is.null(col2) & is.null(formula) & is.null(data)) (stop("Incorrect input format. Try again."))
  
  if(!is.null(formula) & !is.null(data)) {
    # convert formula version to two-col version
    dv_name = all.vars(formula)[1]
    iv_name = all.vars(formula)[2]
    iv_values = data[, iv_name]
    iv_levels = unique(iv_values) %>% unlist()
    col1 = data[iv_values == iv_levels[1], dv_name] %>% unlist()
    col2 = data[iv_values == iv_levels[2], dv_name] %>% unlist()
  }
  
  res.freq = t.test(x = col1, y = col2, paired = paired, var.equal = var.equal)
  res.freq %>% intext_t() %>% cat(); cat("\n")
  
  res.bayes = BayesFactor::ttestBF(x = col1, y = col2, paired = paired)
  res.bayes %>% intext_t_bayes() %>% cat(); cat("\n")
  
  cohensd = effectsize::cohens_d(x = col1, y = col2, paired = paired)
  
  table_out = data.frame(
    't' = res.freq$statistic %>% round(2),
    'df' = res.freq$parameter,
    'p' = res.freq$p.value %>% round(3),
    'BF10' = res.bayes@bayesFactor[["bf"]] %>% round(2),
    m1 = mean(col1, na.rm = TRUE) %>% round(2),
    m2 = mean(col2, na.rm = TRUE) %>% round(2),
    'd' = cohensd$Cohens_d %>% round(2),
    'd_CILB' = cohensd$CI_low %>% round(2),
    'd_CIUB' = cohensd$CI_high %>% round(2)
  )
  
  if(print) {
    cat("\n")
    cat("Groups:", iv_levels, "\n")
    cat("Mean in first group :", table_out$m1, "\n")
    cat("Mean in second group:", table_out$m2, "\n")
    print(cohensd)
  }
  if(viewtable) {View(table_out)}

  return(invisible(table_out))
}



##########

endtime = Sys.time()
cat("\nFinished loading Nadya's t-test upgrades.")
cat("\nTime taken :", (endtime - starttime))
cat("\nUse ttestFNB() to run both frequentist and Bayesian t-tests at the same time :D")
cat("\nPlease cite the BayesFactor package if you use ttestFNB()!")
cat("\n####################")
cat("\n")

rm(starttime); rm(endtime)
