library(devtools)
source_url("https://raw.githubusercontent.com/nadyaeiou/nadyasscripts/main/all.R")

##########

cat("\n####################")
cat("\nLoading Nadya's linear regression upgrades from Github.")
cat("\nLast update: 17 Nov 2020, 7:58pm")
cat("\nPackage(s) : QuantPsyc")
cat("\n")

starttime <- Sys.time()

##########



library(QuantPsyc)



regression <- function(formula, data, confint = TRUE, round = TRUE) {
  
  # run lm
  lm.output = lm(formula, data = data)
  
  # prepare extraction of coeff, se, t, p
  out = lm.output %>% summary()
  out = data.frame(out[["coefficients"]])
  colnames(out) = c("coeff", "se", "t", "p")

  # reformat
  out = out %>% dplyr::mutate(
    variable = rownames(out),
    stdcoeff = NA,
    coeff = coeff,
    se = se,
    p = p,
    sig = sigstars(p),
    .keep = "none"
  )
  
  # add std coeffs
  out$stdcoeff = c('(Intercept)' = NA, lm.beta(lm.output))
  
  # round if needed
  if(round) {
    out = out %>% dplyr::mutate(
      stdcoeff = round2(stdcoeff),
      coeff = round2(coeff),
      se = round2(se),
      p = round3(p)
    )
  }
  
  # extract 95% CI and add to output if requested, and round if needed
  if(confint) {
    ci95 = lm.output %>% confint()
    if(round) {
      ci95[ , 1] = ci95[ , 1] %>% round2()
      ci95[ , 2] = ci95[ , 2] %>% round2()
      }
    out$CI95lower = ci95[ , 1]
    out$CI95upper = ci95[ , 2]
  }
  
  # return clean output
  return(out)
}



regression.hierarchical <- function(formulae, data, intext = TRUE, viewtable = TRUE, csv = NULL, print = TRUE, round = TRUE) {
  if(!is.data.frame(data)) stop("Data should be a data.frame.")
  if(!is.null(csv)) {
    if(!grepl(".csv", csv)) stop("You have indicated that you want a .csv output. Please ensure your filename (passed to csv argument) ends in '.csv'. If you do not want a .csv output, omit the csv argument.")
  }
  
  # get number of models
  num_of_models = length(formulae)
  
  # initialise list of results
  results = list()
  
  # run regression for each model
  for(n in 1:num_of_models) {
    
    # prepare model label
    label = paste0("m", n)
    
    # retrieve current formula
    current_formula = formulae[[n]]
    
    # run regression for current model
    current_result = regression(current_formula, data, round = round)
    
    # relabel columns
    colnames(current_result)[2:8] = paste0(label, "_", colnames(current_result)[2:8])
    
    # add results to list
    results[[label]] = current_result
  }
  
  # if user wants to view table of outputs side by side
  # or if user wants to write csv
  # prepare table and execute accordingly
  if(viewtable | !is.null(csv)) {
    table_of_outputs = results[[1]]
    for(n in 2:num_of_models) {table_of_outputs = merge(table_of_outputs, results[[n]], all = T, sort = F)}
    if(viewtable) {View(table_of_outputs)}
    if(!is.null(csv)) {write.csv(table_of_outputs, csv, row.names = F)}
  }
  
  # if user wants to see printed list, print it
  if(print) {print(results)}
  
  # if user wants to see intext, print it
  if(intext) {
    for(n in 1:num_of_models) {
      res = results[[n]]
      varname = res[2, 1]
      beta = res[2, 2] %>% round2(force = TRUE) %>% trimws()
      b = res[2, 3] %>% round2(force = TRUE) %>% trimws()
      se = res[2, 4] %>% round2(force = TRUE) %>% trimws()
      ci95 = res[2, c(7, 8)] %>% round2(force = TRUE) %>% trimws()
      p = res[2, 5] %>% round3(force = TRUE) %>% trimws()
      p.operator = ifelse(res[2, 6] == "***", "<", "=")
      
      bbs = paste0("β = ", beta, ", b = ", b, ", SE = ", se, sep = "")
      ci95 = paste0("95% CI = [", ci95[1], ", ", ci95[2], "]", sep = "")
      psegment = ifelse(p.operator == "<", "p < .001", paste0("p = ", p, sep = ""))
      cat("Model ", n, ": ", varname, " ", bbs, ", ", ci95, ", ", psegment, "\n", sep = "")
    }
  }
  
  # silently return list
  invisible(results)
}



##########

endtime <- Sys.time()
cat("\nFinished loading Nadya's linear regression upgrades.")
cat("\nTime taken:", (endtime - starttime))
cat("\n####################")
cat("\n")

rm(starttime); rm(endtime)
