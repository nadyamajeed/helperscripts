devtools::source_url("https://raw.githubusercontent.com/nadyamajeed/helperscripts/main/all.R")

##########

cat("\n####################")
cat("\nLoading Nadya's t-test upgrades (with Bayesian t-test!) from Github.")
cat("\n            Version : 0.0.3.9001")
cat("\n        Last update : 1 Jul 2021, 4:55am")
cat("\nRequired Package(s) : BayesFactor (0.9.12-4.2), effectsize (0.4.5)")
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
  # note that ttestBF.output@bayesFactor[["bf"]] gives logBF10 as of BayesFactor version 0.9.12-4.2
  # need to exp() to get BF10
  BF10 = exp(ttestBF.output@bayesFactor[["bf"]])
  # use effectsize::interpret_bf() to get evidence category
  evcat = paste0(effectsize::interpret_bf(BF10, include_value = FALSE, exact = TRUE), " H1 (rule: jeffreys1961)")
  # return BF10 and evidence category
  return(list(BF10 = BF10, evcat = evcat))
}



intext_t_bayes = function(ttestBF.output) {
  out = tBF10(ttestBF.output)
  BF10 = out$BF10 %>% round2(force = TRUE)
  return(paste0("BF10 = ", BF10, ", ", out$evcat))
}



ttestFNB = function(
  col1 = NULL, col2 = NULL,
  formula = NULL, data = NULL,
  paired = NULL, var.equal = TRUE,
  d = TRUE,
  dp = 2,
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
    't' = res.freq$statistic %>% round(dp),
    'df' = res.freq$parameter,
    'p' = res.freq$p.value %>% round(3),
    'BF10' = tBF10(res.bayes)$BF10 %>% round(dp),
    'BF_cat' = tBF10(res.bayes)$evcat,
    m1 = mean(col1, na.rm = TRUE) %>% round(dp),
    sd1 = sd(col1, na.rm = TRUE) %>% round(dp),
    m2 = mean(col2, na.rm = TRUE) %>% round(dp),
    sd2 = sd(col2, na.rm = TRUE) %>% round(dp),
    'd' = cohensd$Cohens_d %>% round(dp),
    'd_CILB' = cohensd$CI_low %>% round(dp),
    'd_CIUB' = cohensd$CI_high %>% round(dp)
  )
  
  if(print) {
    cat("\n")
    cat("First group is ", levels(iv_values)[1], " (M = ", table_out$m1, ", SD = ", table_out$sd1, ")\n", sep = "")
    cat("Second group is ", levels(iv_values)[2], " (M = ", table_out$m2, ", SD = ", table_out$sd2, ")\n", sep = "")
    cat("\n")
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
