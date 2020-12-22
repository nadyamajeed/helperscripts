devtools::source_url("https://raw.githubusercontent.com/nadyaeiou/nadyasscripts/main/all.R")
devtools::source_url("https://raw.githubusercontent.com/nadyaeiou/nadyasscripts/main/regressionINTEXT.R")

##########

cat("\n####################")
cat("\nLoading Nadya's linear regression upgrades (with Amelia and mice+mitml support) from Github.")
cat("\n            Version : 0.0.1.9003 (for R version 3.6.3)")
cat("\n        Last update : 23 Dec 2020, 7:02am")
cat("\n Loading Package(s) : tidyverse")
cat("\nRequired Package(s) : broom, car, effectsize, lm.beta, purrr")
cat("\n")

starttime <- Sys.time()

if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}

##########



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
    if(!require(effectsize)){install.packages("effectsize")}
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
    if(!require(lm.beta)){install.packages("lm.beta")}
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
    if(!require(car)){install.packages("car")}
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



regressionAmelia <- function(
  formula.lm, amelia.output = NULL, amelia.data = NULL,
  intext = FALSE, intext_specific = NULL, only_intext = FALSE, intext_add_intercept = FALSE,
  ss_dummy_predictor = FALSE) {
  # handles regression and pooling for EM datasets by Amelia
  
  ##### sub-functions #####
  
  # sub-function to handle melding
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
    
    coefs_melded <- Amelia::mi.meld(just_coefs, just_ses)
    
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
    
    # ADDED PART BY NADYA TO REORDER PREDICTORS ACCORDING TO FORMULA ORDER INSTEAD OF ALPHABETICAL #
    predictors = c("(Intercept)", labels(terms(formula.lm)))
    melded_summary = melded_summary %>% dplyr::slice(match(predictors, term))
    # END OF ADDITION #
    
    return(melded_summary)
  }
  
  ##### start of main function #####
  
  # check if Amelia, broom, and purrr are installed
  if(!require(Amelia)){install.packages("Amelia")}
  if(!require(broom)){install.packages("broom")}
  if(!require(purrr)){install.packages("purrr")}
  
  # check data input
  if(is.null(amelia.output) & is.null(amelia.data)) {stop("No data passed in.\nEither pass a full amelia output to amelia.output\nor pass amelia data to amelia.data")}
  if(!is.null(amelia.output) & class(amelia.output) != "amelia") {stop("Wrong input format. Pass an amelia output.")}
  
  # extract data if needed
  if(is.null(amelia.data)) {
    data.amelia.unstd <- bind_rows(unclass(amelia.output$imputations), .id = "m") %>%
      group_by(m) %>%
      nest()
  }
  else {data.amelia.unstd = amelia.data}
  
  # convert formula to formula format if needed
  if(is.character(formula.lm)) {formula.lm = as.formula(formula.lm)}
  
  # run regression
  out = regressionAmelia.sub(formula.lm, data.amelia.unstd)
  
  # run regression but with std
  data.amelia.std = data.amelia.unstd
  for(d in 1:nrow(data.amelia.std)) {
    # if function is not running ss, carry out std as per normal
    if(!ss_dummy_predictor) {data.amelia.std$data[[d]] = data.amelia.std$data[[d]] %>% dplyr::mutate_all(scale)}
    # if function is running ss with dummy predictor, std all EXCEPT dummy predictor cols
    else {
      mod_holder = data.amelia.std$data[[d]][, c("modlo", "modhi")]
      data.amelia.std$data[[d]] = data.amelia.std$data[[d]] %>% dplyr::mutate_all(scale)
      data.amelia.std$data[[d]][, c("modlo", "modhi")] = mod_holder
    }
  }
  
  # prepare output table
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
  if(intext | only_intext) {
    cat(intext_regression(regression.output = out, varname = intext_specific, add_intercept = intext_add_intercept), "\n\n")
  }
  
  # depending on whether user has chosen to keep only intext or full results, return accordingly
  if(only_intext) {invisible(intext_regression(regression.output = out, varname = intext_specific, add_intercept = intext_add_intercept))}
  else {return(out)}
}



regressionMice <- function(
  formula.lm, mice.output,
  intext = FALSE, intext_specific = NULL, only_intext = FALSE, intext_add_intercept = FALSE) {
  # handles regression and pooling for MCMC datasets by mice
  
  ##### sub-functions #####
  
  # sub-function to run regression on each imputed dataset
  # modified from mice::with.mids() and then %>% mice::as.mitml.result() %>% mitml::testEstimates()
  regressionMice.sub <- function(f, d) {
    
    # check data input
    if(class(d) != "mids") {stop("Wrong input format. Pass a mice output of class mids.")}
    # initialise storage
    collated_results = list()
    # loop over each imputed dataset
    for(n in 1:d$m) {
      # fill in missing data
      data_completed = mice::complete(d, n) %>% as.data.frame()
      # run regression
      collated_results[[n]] = lm(f, data = data_completed)
    }
    # convert to same output format as mice::with.mids() so that other mice functions can work with the output
    object = list(call = NA, call1 = d$call, nmis = d$nmis, analyses = collated_results)
    oldClass(object) = c("mira", "matrix")
    
    return(object %>% mice::as.mitml.result() %>% mitml::testEstimates())
  }
  
  # sub-function to extract/convert required outputs
  miceCleaner <- function(thing_to_convert) {return(thing_to_convert %>% as.data.frame() %>% dplyr::mutate(variable = rownames(.)))}
  
  # sub-function to standardise within each imputed dataset
  miceStd <- function(d) {
    d.long = complete(d, action = "long", include = TRUE)
    for(d in 0:d$m) {
      d.long[d.long$.imp == d, c(-1, -2)] = d.long[d.long$.imp == d, c(-1, -2)] %>% dplyr::mutate_all(scale)
    }
    return(d.long %>% as.mids())
  }
  
  ##### start of main function #####
  
  # check if mice and mitml are installed
  if(!require(mice)){install.packages("mice")}
  if(!require(mitml)){install.packages("mitml")}
  
  # check data input
  if(class(mice.output) != "mids") {stop("Wrong input format. Pass a mice output of class mids.")}
  
  # convert formula to formula format if needed
  if(is.character(formula.lm)) {formula.lm = as.formula(formula.lm)}
  
  # run regression, extract main results and confints
  reg.output = regressionMice.sub(formula.lm, mice.output)
  mainresults = reg.output$estimates %>% miceCleaner()
  confints = reg.output %>% confint() %>% miceCleaner()
  
  # get std coeffs
  reg.output.std = regressionMice.sub(formula.lm, miceStd(mice.output))
  mainresults.std = reg.output.std$estimates %>% miceCleaner()
  
  # prepare output table
  out = merge(mainresults, confints, sort = FALSE) %>%
    dplyr::mutate(
      variable = variable,
      stdcoeff = mainresults.std$Estimate,
      coeff = Estimate,
      se = Std.Error,
      p = round(`P(>|t|)`, 10),
      sig = sigstars(p),
      CI95lower = `2.5 %`,
      CI95upper = `97.5 %`,
      .keep = "none"
    )
  out[1, "stdcoeff"] = NA
  
  # if user wants to see intext, print it
  if(intext) {
    cat(intext_regression(regression.output = out, varname = intext_specific, add_intercept = intext_add_intercept), "\n\n")
  }
  
  # depending on whether user has chosen to keep only intext or full results, return accordingly
  if(only_intext) {return(intext_regression(regression.output = out, varname = intext_specific, add_intercept = intext_add_intercept))}
  else {return(out)}
}



regression.hierarchical <- function(
  formulae, data,
  intext = TRUE, intext_specific = NULL,
  viewtable = TRUE, csv = NULL, print = TRUE,
  round = TRUE) {
  
  # check data format first
  if(!(class(data) %in% c("data.frame", "amelia", "mids")))
    stop("Data should be of class data.frame or amelia (from Amelia()) or mids (from mice()).")
  
  # check csv mame
  if(!is.null(csv)) {
    if(!grepl(".csv", csv))
      stop("You have indicated that you want a .csv output. Please ensure your filename (passed to csv argument) ends in '.csv'. If you do not want a .csv output, omit the csv argument.")
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
    else if(class(data) == "mids") {current_result = regressionMice(current_formula, data)}
    else {stop("Hmm... can't run regression. Check class of data.")}
    
    # relabel columns
    colnames(current_result)[-1] = paste0(label, "_", colnames(current_result)[-1])
    
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
      cat("\nModel", n, "\n")
      intexts = intext_regression(regression.output = results[[n]], varname = intext_specific)
      for(i in intexts) {cat(i, "\n")}
    }
  }
  
  # silently return list
  invisible(results)
}



simpleslopesAmelia <- function(dv, iv, mod, mod_continuous = FALSE, covars = NULL, amelia.output, debug = FALSE) {
  # computes simple slopes for EM imputed data
  # via Holmbeck (2002) method
  
  # check data input format
  if(class(amelia.output) != "amelia") {stop("Data should be passed in as output from amelia().")}
  
  # check that covars format is correct, if any
  if(!is.null(covars) & !is.character(covars)) {stop("covars should be passed in as character form of formula, e.g., 'covar1 + covar2 + covar3'.")}
  
  # prepare data
  data.amelia <- bind_rows(unclass(amelia.output$imputations), .id = "m") %>%
    group_by(m) %>%
    nest()
  
  # reprint interaction result just in case
  f = paste0(dv, "~", iv, "*", mod)
  if(!is.null(covars)) {if(covars != "") {f = paste0(f, "+", covars)}}
  regressionAmelia(f, amelia.output = amelia.output, intext = TRUE, intext_specific = paste0(iv, ":", mod, sep = ""), only_intext = TRUE)
  
  ##### CARRY OUT HOLMBECK PROCEDURE #####
  
  # if categorical moderator (computational example 1)
  if(!mod_continuous) {
    if(debug) {cat("Now carrying out Holmbeck procedure to create grouping variables.")}
    
    for(d in 1:nrow(data.amelia)) {
      if(debug) {print(d)}
      
      # retrieve column with iv variable
      iv_column = data.amelia$data[[d]][[iv]]
      
      # retrieve column with moderator variable
      moderator_column = data.amelia$data[[d]][[mod]]
      
      # check that moderator has been dummy-coded
      yes = sum(moderator_column == 1, na.rm = T)
      no = sum(moderator_column == 0, na.rm = T)
      if(yes==0 & no==0) {stop("\nCategorical variables should be dummy-coded in 0/1. Neither found.\n")}
      
      # for each imputed dataset, add grouping columns as per Holmbeck procedure
      data.amelia$data[[d]] = data.amelia$data[[d]] %>%
        dplyr::mutate(
          modlo = moderator_column,
          modhi = moderator_column - 1
        )
    }
    
    # prepare equation for each level of moderator
    f.0 = paste0(dv, "~", iv, "* modlo")
    f.1 = paste0(dv, "~", iv, "* modhi")
    
    # add covars if any
    if(!is.null(covars)) {
      if(covars != "") {
        f.0 = paste0(f.0, "+", covars)
        f.1 = paste0(f.1, "+", covars)
      }
    }
    
    # run separate regressions
    cat("When mod = 0:\n")
    r0 = regressionAmelia(f.0, amelia.data = data.amelia, intext = TRUE, intext_specific = iv, only_intext = TRUE, intext_add_intercept = TRUE, ss_dummy_predictor = TRUE)
    cat("When mod = 1:\n")
    r1 = regressionAmelia(f.1, amelia.data = data.amelia, intext = TRUE, intext_specific = iv, only_intext = TRUE, intext_add_intercept = TRUE, ss_dummy_predictor = TRUE)
    
  }
  
  # if continuous moderator (computational example 2)
  if(mod_continuous) {stop("Support for continuous moderators not yet written. Sorry!")}
  
  ##### END OF HOLMBECK PROCEDURE #####
  
  invisible(list(mod_at_0 = r0, mod_at_1 = r1))
}



##########

endtime <- Sys.time()
cat("\nFinished loading Nadya's linear regression upgrades.")
cat("\nTime taken :", (endtime - starttime))
cat("\n####################")
cat("\n")

rm(starttime); rm(endtime)
