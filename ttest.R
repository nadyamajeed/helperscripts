devtools::source_url("https://raw.githubusercontent.com/nadyamajeed/helperscripts/main/all.R")

##########

cat("\n####################")
cat("\nLoading Nadya's t-test upgrades (with Bayesian t-test!) from Github.")
cat("\n            Version : 0.0.0.9000")
cat("\n        Last update : 16 Dec 2020, 7:45am")
cat("\nRequired Package(s) : BayesFactor")
cat("\n")

starttime <- Sys.time()

##########



intext_t <- function(t.test.output) {
  if(class(t.test.output) != "htest") stop("Needs output from t.test() function.")
  degrees_of_freedom = t.test.output$parameter
  t_statistic = t.test.output$statistic
  p_value = t.test.output$p.value
  return(paste0("t(", degrees_of_freedom, ") = ", round2(t_statistic, force = TRUE), ", ", intext_p(p_value)))
}



tBF10 <- function(ttestBF.output) {
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



intext_t_bayes <- function(ttestBF.output) {
  out = tBF10(ttestBF.output)
  BF10 = out$BF10 %>% round2(force = TRUE)
  return(paste0("BF10 = ", BF10, ", ", out$evcat, sep = ""))
}



ttestFNB <- function(
  col1 = NULL, col2 = NULL,
  formula = NULL, data = NULL,
  paired = NULL, var.equal = TRUE) {
  
  if(!is.null(formula) & !is.null(data)) {
    # using formula version
    t.test(formula = formula, data = data, paired = paired, var.equal = var.equal) %>% intext_t() %>% print()
    BayesFactor::ttestBF(formula = formula, data = data, paired = paired) %>% intext_t_bayes() %>% print()
  }
  
  else if(!is.null(col1) & !is.null(col2)) {
    # using two-column
    t.test(x = col1, y = col2, paired = paired, var.equal = var.equal) %>% intext_t() %>% print()
    BayesFactor::ttestBF(x = col1, y = col2, paired = paired) %>% intext_t_bayes() %>% print()
  }
  
  else(stop("Incorrect input format. Try again."))
  
  invisible(NULL)
}



##########

endtime <- Sys.time()
cat("\nFinished loading Nadya's t-test upgrades.")
cat("\nTime taken :", (endtime - starttime))
cat("\nUse ttestFNB() to run both frequentist and Bayesian t-tests at the same time :D")
cat("\nPlease cite the BayesFactor package if you use ttestFNB()!")
cat("\n####################")
cat("\n")

rm(starttime); rm(endtime)
