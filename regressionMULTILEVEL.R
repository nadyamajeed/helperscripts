devtools::source_url("https://raw.githubusercontent.com/nadyamajeed/helperscripts/main/all.R")
devtools::source_url("https://raw.githubusercontent.com/nadyamajeed/helperscripts/main/regressionINTEXT.R")

##########

cat("\n####################")
cat("\nLoading Nadya's multilevel modelling upgrades from Github.")
cat("\n            Version : 0.0.2.9002 (for R version 3.6.3)")
cat("\n        Last update : 27 Mar 2021, 1:22am")
cat("\n Loading Package(s) : lme4 (written for 1.1-23), lmerTest (written for 3.1-2)")
cat("\nRequired Package(s) : effectsize (std coeffs), psych and purrr (repeated alphas)")
cat("\n")

starttime = Sys.time()

library(lme4); library(lmerTest)

##########



alpha_repeated = function(search_start, time_colname, data, check_keys = FALSE, round = 2) {
  
  # check format
  if(!is.character(search_start)) stop("search_start should be of class character.")
  if(!is.character(time_colname)) stop("time_colname should be of class character.")
  if(length(time_colname) > 1) stop("time_colname should be of length 1.")
  
  # report whether check_keys is TRUE or FALSE
  if(check_keys) {cat("!! check_keys is set to TRUE. Please be careful. Items may be wrongly reversed. !!\n")}
  else {cat("!! check_keys is set to FALSE. Please ensure items have already been reversed if needed. !!\n")}
  
  # only keep relevant columns in the data and sort by timepoint
  data = data %>% dplyr::select(all_of(time_colname), starts_with(search_start))
  data = data[order(data[time_colname, ])]
  
  # split data into subsets based on timepoint
  data_split = split(data %>% dplyr::select(-time_colname), data[, time_colname])
  
  # compute alphas for each timepoint
  alpha_cleaner = function(df) {
    a = psych::alpha(x = df, check.keys = check_keys)
    return(a$total$raw_alpha %>% round(round))
  }
  all_alphas = purrr::map(data_split, purrr::quietly(alpha_cleaner))
  
  # keep the actual value of alpha
  all_values = vector()
  for(i in all_alphas) {all_values = c(all_values, i[["result"]])}
  
  # keep warning messages
  squeeze_warnings = function(vector_of_warnings) {
    if(length(vector_of_warnings) == 0) {return(NA_character_)}
    else {squeezed = vector_of_warnings[1]}
    if(length(vector_of_warnings > 1)) {
      for(i in 2:length(vector_of_warnings)) {squeezed = paste0(squeezed, vector_of_warnings[i])}
    }
    return(squeezed)
  }
  all_warnings = vector()
  for(i in all_alphas) {all_warnings = c(all_warnings, squeeze_warnings(i[["warnings"]]))}
  
  # prepare output
  collated_info = data.frame(
    timepoint = names(all_alphas),
    alpha = all_values,
    warning = all_warnings
  )
  
  # print summarised results and invisibly return full output
  cat("Range of alphas is [", min(collated_info$alpha), ",", max(collated_info$alpha), "]\n")
  if(sum(!is.na(collated_info$warning)) > 0) {
    warning("There are warnings! Please view full output to check.")
    View(collated_info)
  }
  invisible(collated_info)
  
}



mlm = function(
  formula.lmer, data, REML = FALSE, switch_optimiser = FALSE, switch_optimiser2 = FALSE,
  csv = NULL, raw = TRUE, confint = NULL, std = FALSE, round = 5, test_random = FALSE, bonferroni = NULL, intext_specific = NULL,
  print = TRUE, timer = FALSE, debug = FALSE) {
  
  # check for csv
  if(!is.null(csv)) {
    if(grepl(".csv", csv)) stop("You have indicated that you want a .csv output. Please ensure your filename (passed to csv argument) DOES NOT have any format ending (including .csv). If you do not want a .csv output, omit the csv argument.")
  }
  
  starttime = Sys.time()
  
  if(is.character(formula.lmer)) {formula.lmer = as.formula(formula.lmer)}
  
  # fix for convergence errors
  if(switch_optimiser & switch_optimiser2) {stop("Both optimiser switch options set to TRUE. Please set one to FALSE (or both to FALSE to use default).")}
  if(switch_optimiser) {
    optSwitch = lmerControl(optimizer = "Nelder_Mead", optCtrl = list(maxfun = 10000000))
    cat("Using Nelder-Mead optimisation with maximum 10,000,000 evaluations\ninstead of default nloptwrap.\n\n")
  }
  if(switch_optimiser2) {
    optSwitch = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))
    cat("Using bobyqa optimisation with maximum 10,000,000 evaluations\ninstead of default nloptwrap.\n\n")
  }
  
  # run multilevel and summarise
  if(switch_optimiser | switch_optimiser2) {lmer.output = lmer(formula.lmer, data = data, REML = REML, control = optSwitch)}
  else {lmer.output = lmer(formula.lmer, data = data, REML = REML)}
  lmer.summary = summary(lmer.output)
  
  # internal function to convert to data.frame and suppress class change warnings
  convert.to.data.frame = function(thing_to_convert) {
    suppressWarnings(as.data.frame(thing_to_convert))
  }
  
  # extract random effects and conduct significance testing using lmerTest::rand
  randomeffects = VarCorr(lmer.output) %>% convert.to.data.frame() %>%
    dplyr::filter(is.na(var2)) %>%
    dplyr::mutate(
      variable = paste0(var1, " | ", grp),
      var = (sdcor * sdcor) %>% round(round),
      .keep = "none")
  if(test_random) {
    randomeffects = dplyr::mutate(
      p = c(NA, lmerTest::rand(lmer.output)[2, "Pr(>Chisq)"] %>% round(3), NA),
      sig = sigstars(p)
    )
  }
  randomeffects[nrow(randomeffects), "variable"] = "Residual"
  
  # extract fixed effects
  # with bonferroni correction if needed
  fixedeffects = lmer.summary[["coefficients"]] %>% convert.to.data.frame() %>%
    dplyr::mutate(
      variable = rownames(.),
      coeff = Estimate %>% round(round),
      se = `Std. Error` %>% round(round),
      p = `Pr(>|t|)` %>% round(3),
      sig = sigstars(p),
      p.temp = `Pr(>|t|)`,
      .keep = "none"
    )
  
  # add bonferroni correction if needed
  if(!is.null(bonferroni)) {
    fixedeffects = fixedeffects %>%
      dplyr::mutate(
        p.adj = (p.temp * bonferroni) %>% round(3),
        sig.adj = sigstars(p.adj),
        .keep = "unused"
      )
  } else {
    fixedeffects = fixedeffects %>%
      dplyr::mutate(p.temp = NULL)
  }
  
  # add std coeffs if requested
  if(std) {
    cat("Standardised coeffs calculated at level 1.\n\n")
    if(switch_optimiser | switch_optimiser2) {std.output = lmer(formula.lmer, data = effectsize::standardize(data), REML = REML, control = optSwitch)}
    else {std.output = lmer(formula.lmer, data = effectsize::standardize(data), REML = REML)}
    
    # random effects
    std.random = VarCorr(std.output) %>% convert.to.data.frame() %>%
      dplyr::filter(is.na(var2)) %>%
      dplyr::mutate(
        variable = paste0(var1, " | ", grp),
        stdvar = (sdcor * sdcor) %>% round(round),
        .keep = "none"
      )
    std.random[nrow(std.random), "variable"] = "Residual"
    randomeffects = merge(std.random, randomeffects, sort = FALSE)
    
    # fixed effects
    std.summary = std.output %>% summary()
    std.fixed = std.summary[["coefficients"]] %>% convert.to.data.frame() %>%
      dplyr::mutate(
        variable = rownames(.),
        stdcoeff = Estimate %>% round(round),
        .keep = "none"
      )
    fixedeffects = merge(std.fixed, fixedeffects, sort = FALSE)
    fixedeffects[1, "stdcoeff"] = NA
  }
  
  # extract confints (unstd)
  if(!is.null(confint)) {
    if(confint == "fixed") {ci.raw = confint(lmer.output, parm = "beta_")}
    else {ci.raw = confint(lmer.output, parm = confint, oldNames = FALSE)}
    ci = ci.raw %>%
      convert.to.data.frame() %>%
      dplyr::mutate(
        variable = rownames(.),
        CI95lower = `2.5 %` %>% round(round),
        CI95upper = `97.5 %` %>% round(round),
        .keep = 'none'
      )
    # add to fixed effects table and undo reordering
    originalorder = fixedeffects$variable
    fixedeffects = merge(fixedeffects, ci, all.x = TRUE, sort = FALSE) %>%
      dplyr::slice(match(originalorder, variable))
  }
  
  if(debug | print) {
    print(randomeffects)
    cat("\n")
    print(fixedeffects)
  }
  
  # prepare output
  out = list(random = randomeffects, fixed = fixedeffects)
  if(raw) {out$raw = lmer.output}
  
  # if user wants to write csv, prepare table and execute accordingly
  if(!is.null(csv)) {
    for(effect in c("random", "fixed")) {
      write.csv(out[[effect]], paste0(csv, "_", effect, ".csv", sep = ""), row.names = F)
    }
  }
  
  # if user wants to see intext, print it
  if(!is.null(intext_specific)) {
    for(variable in intext_specific) {
      cat(intext_regression(regression.output = out[["fixed"]], varname = variable, round = round), "\n")
    }
  }
  
  endtime = Sys.time()
  if(timer) {cat("\nTime taken: ", endtime - starttime, "seconds\n")}
  
  return(invisible(out))
}



mlm.hierarchical = function(
  formulae, data, REML = FALSE, switch_optimiser = FALSE, switch_optimiser2 = FALSE,
  intext_specific = NULL, viewtable = TRUE, csv = NULL, print = TRUE, raw = TRUE,
  confint = NULL, std = FALSE, round = 5, bonferroni = NULL, debug = FALSE) {
  
  if(!is.data.frame(data)) stop("Data should be of class data.frame.")
  if(!is.null(csv)) {
    if(grepl(".csv", csv)) stop("You have indicated that you want a .csv output. Please ensure your filename (passed to csv argument) DOES NOT have any format ending (including .csv). If you do not want a .csv output, omit the csv argument.")
  }
  
  if(!is.null(intext_specific)) {
    for(variable in intext_specific) {
      if(!(variable %in% confint)) {
        confint = c(variable, confint)
      }
    }
  }
  
  # get number of models
  num_of_models = length(formulae)
  
  # initialise list of results
  results = list()
  
  # run regression for each model
  for(n in 1:num_of_models) {
    
    # prepare model label
    label = paste0("m", n)
    if(print) {cat("\n-----\n\n", label, "\n")}
    
    # retrieve current formula
    current_formula = formulae[[n]]
    
    # run regression for current model
    current_result = mlm(
      formula.lmer = current_formula, data = data,
      REML = REML, switch_optimiser = switch_optimiser, switch_optimiser2 = switch_optimiser2, csv = NULL,
      confint = confint, std = std, round = round, bonferroni = bonferroni,
      raw = raw, print = print)
    
    # relabel columns
    colnames(current_result$random)[-1] = paste0(label, "_", colnames(current_result$random)[-1])
    colnames(current_result$fixed)[-1] = paste0(label, "_", colnames(current_result$fixed)[-1])
    if(debug) {print(current_result)}
    
    # add results to list
    results[[label]] = current_result
  }
  
  # if user wants to view table of outputs side by side
  # or if user wants to write csv
  # prepare table and execute accordingly
  if(viewtable | !is.null(csv)) {
    for(effect in c("random", "fixed")) {
      table_of_outputs = results[[1]][[effect]]
      for(n in 2:num_of_models) {table_of_outputs = merge(table_of_outputs, results[[n]][[effect]], all = T, sort = F)}
      if(viewtable) {View(table_of_outputs)}
      if(!is.null(csv)) {write.csv(table_of_outputs, paste0(csv, "_", effect, ".csv", sep = ""), row.names = F)}
    }
  }
  
  # if user wants to see intext, print it
  if(!is.null(intext_specific)) {
    for(variable in intext_specific) {
      for(n in 1:num_of_models) {
        res = results[[n]][["fixed"]]
        cat(intext_regression(regression.output = res, varname = variable, round = round), "\n")
      }
    }
  }
  
  # silently return list
  return(invisible(results))
}



##########

endtime = Sys.time()
cat("\nFinished loading Nadya's multilevel modelling upgrades.")
cat("\nTime taken :", (endtime - starttime))
cat("\n! NOTE ! Please cite lme4 (conducting multilevel) and lmerTest (significance testing) in your manuscript.")
cat("\n####################")
cat("\n")

rm(starttime); rm(endtime)
