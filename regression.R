library(devtools)
source_url("https://raw.githubusercontent.com/nadyaeiou/nadyasscripts/main/all.R")

##########

cat("\n####################")
cat("\nLoading Nadya's linear regression upgrades (with Amelia support) from Github.")
cat("\n        Last update : 13 Dec 2020, 1:43am")
cat("\n Loading Package(s) : tidyverse")
cat("\nRequired Package(s) : broom, car, effectsize, lm.beta, purrr")
cat("\n")

starttime <- Sys.time()

##########



library(tidyverse)



intext_regCoeffs <- function(beta, b, se) {
  return(paste0("β = ", beta, ", b = ", b, ", SE = ", se, sep = ""))
}



intext_CI <- function(cilower, ciupper) {
  return(ci = paste0("95% CI = [", cilower, ", ", ciupper, "]", sep = ""))
}



intext_regression <- function(regression.output, varname = NULL) {
  
  res = regression.output
  
  # if varname is not set, assume first non-intercept term is desired
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

  varname = res[rownum, 1]

  beta = res[rownum, 2] %>% round2(force = TRUE) %>% trimws()
  b = res[rownum, 3] %>% round2(force = TRUE) %>% trimws()
  se = res[rownum, 4] %>% round2(force = TRUE) %>% trimws()
  bbs = intext_regCoeffs(beta, b, se)
  
  ci95 = res[rownum, c(7, 8)] %>% round2(force = TRUE) %>% trimws()
  ci95 = intext_CI(ci95[1], ci95[2])
  
  pval = res[rownum, 5]
  psegment = intext_p(pval)
  
  return(paste0(varname, " ", bbs, ", ", ci95, ", ", psegment, sep = ""))
}



regression <- function(
  formula.lm, data, std_method = "effectsize",
  round = TRUE, intext = FALSE, intext_specific = NULL,
  rsq = FALSE, vif = FALSE, full = FALSE) {
  
  if(full) {intext = TRUE; rsq = TRUE; vif = TRUE}
  
  if(!(std_method %in% c("effectsize", "lm.beta"))) {
    stop("std_method specified not recognised.\nEither use 'effectsize' (default, scales all variables before running regression)\nor 'lm.beta' (follows SPSS method, less computationally-expensive but inappropriate for interactions).")
  }
    
  # run lm
  lm.output = lm(formula.lm, data = data)
  
  # prepare extraction of coeff, se, t, p
  out = lm.output %>% summary()
  out = data.frame(out[["coefficients"]])
  colnames(out) = c("coeff", "se", "t", "p")
  
  # reformat
  out = out %>% dplyr::mutate(
    variable = rownames(out),
    coeff = coeff,
    se = se,
    p = p,
    sig = sigstars(p),
    .keep = "none"
  )
  
  # add std coeffs
  if(std_method == "effectsize") {
    std.summary = lm(formula.lm, data = effectsize::standardize(data)) %>% summary()
    std.summary = data.frame(std.summary[["coefficients"]])
    colnames(std.summary) = c("stdcoeff", "se", "t", "p")
    stdcoeffs_df = std.summary %>% dplyr::mutate(
      variable = rownames(std.summary),
      stdcoeff = stdcoeff,
      .keep = "none"
    )
  }
  else if(std_method == "lm.beta") {
    stdcoeffs = lm.beta::lm.beta(lm.output)$standardized.coefficients
    stdcoeffs_df = data.frame(
      variable = names(stdcoeffs),
      stdcoeff = stdcoeffs
    )
  }
  out = merge(stdcoeffs_df, out, sort = FALSE)
  out[1, "stdcoeff"] = NA
  
  # add confint
  ci95 = lm.output %>% confint()
  out$CI95lower = ci95[ , 1]
  out$CI95upper = ci95[ , 2]
  
  # if user wants to see intext, print it
  if(intext) {
    cat(intext_regression(regression.output = out, varname = intext_specific), "\n\n")
  }
  
  # round if needed
  if(round) {
    out = out %>% dplyr::mutate(
      stdcoeff = round2(stdcoeff),
      coeff = round2(coeff),
      se = round2(se),
      p = round3(p),
      CI95lower = round2(CI95lower),
      CI95upper = round2(CI95upper)
    )
  }
  
  # add vif if requested (and round if needed)
  if(vif) {
    vif.values = car::vif(lm.output)
    if(round) {vif.values = round2(vif.values)}
    vif.values = c(NA, vif.values)
    out$vif = vif.values
  }
  
  # retrieve and print R squared values if requested
  if(rsq) {
    summarised = summary(lm.output)
    cat(
      "Multiple R square = ", summarised[["r.squared"]] %>% round4(),
      ", Adjusted R square = ", summarised[["adj.r.squared"]] %>% round4(),
      "\n\n", sep = "")
  }
  
  # return clean output
  return(out)
}



regressionAmelia.sub <- function(formula.lm, data.amelia) {
  # https://www.andrewheiss.com/blog/2018/03/07/amelia-tidy-melding/
  
  model.out <- data.amelia %>%
    mutate(
      model = data %>% purrr::map(~ lm(formula.lm, data = .)),
      tidied = model %>% purrr::map(~ broom::tidy(., conf.int = TRUE)),
      glance = model %>% purrr::map(~ broom::glance(.))
    )
  
  params <- model.out %>%
    unnest(tidied) %>%
    dplyr::select(m, term, estimate, std.error) %>%
    gather(key, value, estimate, std.error) %>%
    spread(term, value) %>%
    ungroup()
  
  just_coefs <- params %>%
    dplyr::filter(key == "estimate") %>%
    dplyr::select(-m, -key)
  
  just_ses <- params %>%
    dplyr::filter(key == "std.error") %>%
    dplyr::select(-m, -key)
  
  coefs_melded <- mi.meld(just_coefs, just_ses)
  
  model_degree_freedom <- model.out %>%
    unnest(glance) %>%
    dplyr::filter(m == "imp1") %>%
    pull(df.residual)
  
  melded_summary <- as.data.frame(cbind(t(coefs_melded$q.mi),
                                        t(coefs_melded$se.mi))) %>%
    magrittr::set_colnames(c("estimate", "std.error")) %>%
    dplyr::mutate(term = rownames(.)) %>%
    dplyr::select(term, everything()) %>%
    dplyr::mutate(statistic = estimate / std.error,
                  conf.low = estimate + std.error * qt(0.025, model_degree_freedom),
                  conf.high = estimate + std.error * qt(0.975, model_degree_freedom),
                  p.value = 2 * pt(abs(statistic), model_degree_freedom, lower.tail = FALSE))
  
  ##### ADDED PART BY NADYA TO REORDER PREDICTORS ACCORDING TO FORMULA ORDER INSTEAD OF ALPHABETICAL #####
  predictors = c("(Intercept)", labels(terms(formula.lm)))
  melded_summary = melded_summary %>% dplyr::slice(match(predictors, term))
  ##### END OF ADDITION #####
  
  return(melded_summary)
}



regressionAmelia <- function(formula.lm, amelia.output, intext = FALSE, intext_specific = NULL) {
  
  # handles regression and pooling for EM datasets by Amelia
  
  if(class(amelia.output) != "amelia") {
    stop("Wrong input format. Pass an amelia output.")
  }
  
  data.amelia.unstd <- bind_rows(unclass(amelia.output$imputations), .id = "m") %>%
    group_by(m) %>%
    nest()
  
  out = regressionAmelia.sub(formula.lm, data.amelia.unstd)
  
  data.amelia.std = data.amelia.unstd
  for(d in 1:nrow(data.amelia.std)) {
    data.amelia.std$data[[d]] = data.amelia.std$data[[d]] %>% dplyr::mutate_all(scale)
  }
  
  out = out %>% dplyr::mutate(
    variable = term,
    stdcoeff = round(regressionAmelia.sub(formula.lm, data.amelia.std)$estimate, 10),
    coeff = estimate,
    se = std.error,
    p = round(p.value, 10),
    sig = sigstars(p),
    CI95lower = conf.low,
    CI95upper = conf.high,
    .keep = "none"
  )
  
  out[1, "stdcoeff"] = NA
  
  # if user wants to see intext, print it
  if(intext) {cat(intext_regression(regression.output = out, varname = intext_specific)), "\n\n")}
  
  return(out)
}



regression.hierarchical <- function(formulae, data, intext = TRUE, intext_specific = NULL, viewtable = TRUE, csv = NULL, print = TRUE, round = TRUE) {
  if(!is.data.frame(data) & (class(data) != "amelia")) stop("Data should be of class data.frame or amelia.")
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
    if(is.data.frame(data)) {current_result = regression(current_formula, data, round = round)}
    else if(class(data) == "amelia") {current_result = regressionAmelia(current_formula, data)}
    else {stop("Hmm... can't run regression. Check class of data.")}
    
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
      cat(intext_regression(regression.output = res, varname = intext_specific), "\n")
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
