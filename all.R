cat("\n####################")
cat("\nLoading Nadya's functions and other QOL upgrades from Github.")
cat("\n            Version : 0.0.6.9000")
cat("\n       Last updated : 23 May 2021, 7:32am")
cat("\n Loading Package(s) : dplyr")
cat("\nRequired Package(s) : e1071 (skewness in descriptives functions)")
cat("\n                      haven (write_double and unhaven functions)")
(cat"\n                      merTools (ICC calculation for multilevel datasets)")
cat("\n          Option(s) : Prevent scientific notation")
cat("\n")

starttime = Sys.time()

library(dplyr)
options(scipen = 99999)

# --------------------------------------------------



# ----- descriptives, summarisers, renamers, etc. ----- 



descStats = function(var, data = NULL, label = FALSE, dummy = FALSE, compatible = FALSE, skewness = TRUE, mlm_grouping = NULL, mlm_grouping_report = TRUE) {
  
  # check how varvalues was passed -- was it a vector or a colname?
  if(length(var) != 1) {varvalues = var; mlm_usable = FALSE} # vector
  else if(length(var) == 1 & is.character(var) & !is.null(data)) {varvalues = data[, var]; mlm_usable = TRUE} # colname
  else stop("Did you pass in var and data (optional) correctly?")
  
  # count number of valid observations
  n = sum(!is.na(varvalues))
  
  # for 0/1 variables
  if(dummy) {
    yes = sum(varvalues == 1, na.rm = T)
    no = sum(varvalues == 0, na.rm = T)
    if(yes==0 & no==0) {stop("\nCategorical variables should be dummy-coded in 0/1. Neither found.\n")}
    else {
      percentage = round4(yes / (yes + no)) * 100
      if(!compatible) {out = data.frame('n' = n, 'percentage' = percentage)}
      if(compatible) {out = data.frame('n' = n, 'value' = percentage, 'sd' = NA, 'min' = NA, 'max' = NA)}
    }
  }
  
  # for continuous variables
  else {
    if(!is.numeric(varvalues)) {stop("\nVariable is not numeric.\nPlease convert the variable, check the variable name, or set categorical = TRUE to compute descriptives for (dummy-coded) categorical variables.\n")}
    else {
      m = mean(varvalues, na.rm = T) %>% round2()
      sd = sd(varvalues, na.rm = T) %>% round2()
      min = min(varvalues, na.rm = T) %>% round2()
      max = max(varvalues, na.rm = T) %>% round2()
      skew = e1071::skewness(varvalues, na.rm = T) %>% round2()
      out = data.frame('n' = n, 'm' = m, 'sd' = sd, 'min' = min, 'max' = max, 'skew' = skew)
      if(compatible) {out = out %>% dplyr::rename(value = m)}
    }
  }
  
  # if mlm level 1 data
  if(!is.null(mlm_grouping)) {
    if(mlm_grouping_report) catcat("\nYou have indicated that this is level 1 data from a multilevel dataset,\nwith grouping identifier", mlm_grouping, "\nPlease cite merTools for calculation of ICC.\n")
    if(!mlm_usable) stop("\nArguments not usable in this format. Please pass in var as character and pass in data.\n")
    out$merToolsICC = merTools::ICC(outcome = var, group = mlm_grouping, data = data) %>% round(2)
  }
  
  # clean up and return
  if(label != FALSE) {rownames(out) = label}
  return(out)
}



descStats.full = function(data, exclude = NULL, split = FALSE, mlm_grouping = NULL, print = TRUE, csv = TRUE, csv_name = "descriptives.csv", debug = FALSE) {
  
  ##### sub-function to extract descriptives #####
  
  descStats.full.sub = function(data, vars) {
    out = data.frame()
    for(current_var in vars) {
      
      # extract values in column
      current_values = data[, current_var]
      
      # check if column is non-empty first, proceed if non-empty, else (empty) skip
      if(sum(is.na(current_values)) != length(current_values)) {
        
        # check if column is numeric first, proceed if yes, otherwise skip
        if(is.numeric(current_values)) {
          
          # check if column is dummy coded
          dummycheck = sum(current_values != 0 & current_values != 1, na.rm = T)
          dummy = ifelse(dummycheck == 0, TRUE, FALSE)
          
          # prepare label
          label = current_var
          if(dummy) {label = paste(current_var, "(%)")}
          
          # run descStats for current variable
          current_descStats = descStats(current_var, data = data, dummy = dummy, compatible = TRUE, label = label, mlm_grouping = mlm_grouping, mlm_grouping_report = FALSE)
          
          # bind back to table of descriptives
          out = rbind(out, current_descStats)
        }
        
        else {cat("\nSkipping", current_var, "as it is not numeric.\n")}
      }
      else {cat("\nSkipping", current_var, "as it is empty.\n")}
    }
    return(out)
  }
  
  ##### start of main function #####
  
  if(!is.data.frame(data)) stop("Please pass in a data.frame.")
  
  # force format back (sometimes data cleaning messes with the format)
  data = as.data.frame(data)
  
  # exclude variables if requested
  if(!is.null(exclude)) {
    exclusions = data %>% dplyr::select((starts_with(exclude) & ends_with(exclude))) %>% colnames()
    cat("Excluding the following columns:", exclusions, "\n")
    data = data %>% dplyr::select(-(starts_with(exclude) & ends_with(exclude)))
  }
  
  # retrieve colnames
  vars = colnames(data)
  if(debug) {print(vars)}
  
  # if mlm, report and remove the grouping col from cols to calculate
  if(!is.null(mlm_grouping)) {
    cat("\nYou have indicated that this is level 1 data from a multilevel dataset,\nwith grouping identifier", mlm_grouping, "\nPlease cite merTools for calculation of ICC.\n")
    vars = vars[!grepl(mlm_grouping, vars)]
  }
  
  # extract desc stats
  if(split == FALSE) {out = descStats.full.sub(data = data, vars = vars)}
  else {
    out = data.frame()
    number_of_rows_with_missing_levels = sum(is.na(data[[split]]))
    if(number_of_rows_with_missing_levels > 0) {
      cat("\nWarning! Some rows removed due to NA level in grouping variable.\n")
      data = data[!is.na(data[[split]]), ]
    }
    levels_for_split = unique(data[[split]])
    for(level in levels_for_split) {
      out.current = descStats.full.sub(
        data = data[data[[split]] == level, ],
        vars = vars
      )
      colnames(out.current) = paste0(level, "_", colnames(out.current))
      if(nrow(out) == 0) {out = out.current}
      else {out = cbind(out, out.current)}
    }
  }
  
  # print if requested
  if(print){cat("\n"); print(out)}
  
  # write csv if requested
  if(csv){
    cat("\nWriting csv into the working directory.\n")
    write.csv(out, csv_name)
    cat("Done!\n")
  }
  
  # return silently
  invisible(out)
}



descStats.split = function(varname, group, levels = NULL, labels = NULL) {
  
  # if levels not specified, automatically retrieve levels
  if(is.null(levels)) {
    # stop if labels specified but levels not, in case it's wrong order
    if(!is.null(labels)) {stop("Labels given but levels not specified.\nSpecify levels using levels = c(...) argument, or drop the labels argument.")}
    # otherwise, continue
    if(!is.factor(group)) {group = as.factor(group)}
    levels = levels(group)
  }
  
  # extract descStats for each level
  out = data.frame()
  for(level in levels) {out = rbind(out, descStats(varname[group == level], label = level))}
  
  # replace labels if requested
  if(!is.null(labels)) {rownames(out) = labels}
  
  # return split descStats
  return(out)
}



frequencies = function(varname) {
  table(varname) %>% as.data.frame() %>%
    dplyr::mutate(
      value = varname,
      freq_raw = Freq,
      freq_percent = round2(100 * freq_raw / sum(freq_raw)),
      .keep = "none")
}



rename_pattern = function(data.frame, find, replace = "") {
  # takes in a data.frame, finds patterns in col names and replaces them
  # useful for adding into a dplyr chain (instead of having a separate colnames() line at the end)
  if(class(find) != "character" | class(replace) != "character") stop("Patterns should be given as character.")
  colnames(data.frame) = sub(find, replace, colnames(data.frame))
  return(data.frame)
}



# the following functions are to allow back-compatibility
try(detach(deprecatedHelperScripts))
attach(list(
  dS = function(...) {warning("Function is deprecated. Use descStats() instead."); descStats(...)},
  dS.full = function(...) {warning("Function is deprecated. Use descStats.full() instead."); descStats.full(...)},
  dS.split = function(...) {warning("Function is deprecated. Use descStats.split() instead."); descStats.split(...)}
), name = "deprecatedHelperScripts")



# ----- data manipulation ----- 



centre = function(column) {scale(column, center = TRUE, scale = FALSE) %>% as.numeric()}



dichotomise = function(column, missing = NA_real_) {
  # converts continuous variable into dichotomous/binary variable
  # useful for converting durations or counts into occurences (yes/no)
  # e.g., from number of stressors (count) to stressor exposure (exposed/not exposed)
  # e.g., from music listening duration (0h - 24h) to music listening status (listened/did not listen)
  dplyr::case_when(
    column > 0 ~ 1,
    column == 0 ~ 0,
    is.na(column) ~ missing
  )
}



roundx = function(thing_to_round, dp, force = FALSE) {
  if(!is.logical(force)) stop("'force' must be set to TRUE or FALSE (default FALSE).")
  number = round(as.numeric(thing_to_round), as.numeric(dp))
  if(force) {number = format(number, nsmall = dp)}
  return(number)
}
round2 = function(thing_to_round, force = FALSE) {return(roundx(thing_to_round, 2, force = force))}
round3 = function(thing_to_round, force = FALSE) {return(roundx(thing_to_round, 3, force = force))}
round4 = function(thing_to_round, force = FALSE) {return(roundx(thing_to_round, 4, force = force))}
round5 = function(thing_to_round, force = FALSE) {return(roundx(thing_to_round, 5, force = force))}



winsorSD = function(values, numSD = 3, debug = FALSE) {
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
  return(out)
}



# ----- significance testing ----- 



intext_p = function(pval) {
  if(pval > 1) {stop("pval > 1. Are you sure you passed in a p-value?")}
  if(pval < 0) {stop("pval < 0. Are you sure you passed in a p-value?")}
  return(ifelse(pval < .001, "p < .001", paste0("p = ", round3(pval, force = TRUE), sep = "")))
}



sigstars = function(pval) {
  pval = as.numeric(pval)
  stars = dplyr::case_when(
    pval < .001 ~ "***",
    pval < .01  ~ "**",
    pval < .05  ~ "*",
    TRUE        ~ ""
  )
  return(stars)
}



# ----- zero-order correlation matrix ----- 



better_cormatrix = function(data, first = NULL, include = NULL, exclude = NULL, pval_output = c("star", "matrix", "none"), csv_name = NULL) {
  
  ##### sub-function to fix colnames and rownames #####
  
  fix_matrix_names = function(d) {
    rownames(d) = paste0(c(1:nrow(d)), ". ", rownames(d))
    colnames(d) = c(1:ncol(d))
    d[ncol(d)] = NULL
    return(d)
  }
  
  ##### start of main function #####
  
  # check arguments
  if(!is.null(first) & !is.null(include)) if(!(first %in% include)) stop("first is not in include. Please fix.")
  if(!is.null(include)) if(length(include) < 2) stop("Too few columns in include.")
  if(!is.null(include) & !is.null(exclude)) stop("Can't have both include and exclude arguments. Omit one.")
  if(!is.null(csv_name)) if(!grepl(".csv", csv_name)) stop("Ensure csv_name ends in .csv. If you do not want a csv, leave this argument blank.")
  if((length(pval_output) > 1) | !(pval_output[1] %in% c("star", "matrix", "none"))) {pval_output = pval_output[1]; warning("pval_output not specified or invalid. Using star output.")}
  
  # reorder variables if needed
  if(is.null(first)) {d.zoc = data} else {d.zoc = data %>% dplyr::select(first, everything())}
  
  # include/exclude variables if needed
  if(!is.null(include)) {d.zoc = d.zoc %>% dplyr::select(all_of(include))}
  if(!is.null(exclude)) {d.zoc = d.zoc %>% dplyr::select(-all_of(exclude))}
  
  # generate correlation matrix
  cormatrix = cor(d.zoc, use = "pairwise.complete.obs") %>% as.data.frame()
  
  # erase cells that aren't needed and check significance
  vars_done = NULL
  for(var in colnames(cormatrix)) {
    # track which vars are done
    vars_done = c(vars_done, var)
    # erase the wrong side of the diagonal
    cormatrix[vars_done, var] = NA
    # format nicely to 2dp
    cormatrix[, var] = round(cormatrix[, var], 2)
    # if pval_output is set to "star", add stars
    if(pval_output == "star") {
      for(var2 in colnames(cormatrix)) if(!(var2 %in% vars_done)) {
        pval = cor.test(d.zoc[,var], d.zoc[,var2])$p.value
        star = sigstars(pval)
        cormatrix[var2, var] = paste0(cormatrix[var2, var], star)
      }
    }
  }; rm(var); rm(vars_done)
  
  # fix colnames and rownames
  cormatrix = fix_matrix_names(cormatrix)
  
  # if pval_output is set to "matrix", generate and clean matrix
  if(pval_output == "matrix") {
    pmatrix = cor(d.zoc, use = "pairwise.complete.obs") %>% as.data.frame()
    vars_done = NULL
    for(var in colnames(pmatrix)) {
      # track which vars are done
      vars_done = c(vars_done, var)
      # erase the wrong side of the diagonal
      pmatrix[vars_done, var] = NA
      # replace with pvals
      for(var2 in colnames(pmatrix)) if(!(var2 %in% vars_done)) {
        pval = cor.test(d.zoc[,var], d.zoc[,var2])$p.value %>% round(3)
        pmatrix[var2, var] = pval
      }
    }; rm(var); rm(vars_done)
    # fix colnames and rownames
    pmatrix = fix_matrix_names(pmatrix)
  }
  
  # write matrix to csv if asked for
  if(!is.null(csv_name)) {
    write.csv(cormatrix, csv_name, row.names = T)
    if(pval_output == "matrix") write.csv(pmatrix, paste0("pvals_", csv_name), row.names = T)
  }
  
  # return cormatrix
  if(pval_output == "matrix") return(list("corr" = cormatrix, "pval" = pmatrix))
  else return(cormatrix)
  
}



# ----- import & export ----- 



strip_qualtrics = function(data) {
  data %>% dplyr::select(
    -Status, -IPAddress, -Finished, -RecordedDate,
    -RecipientLastName, -RecipientFirstName, -RecipientEmail,
    -LocationLatitude, -LocationLongitude, -UserLanguage)
}



unhaven = function(data) {data %>% haven::zap_labels() %>% haven::zap_label() %>% as.data.frame()}



write_double = function(data, filename) {
  # writes both .csv and .sav files at once
  write.csv(data, paste0(filename, ".csv"), row.names = F)
  haven::write_sav(data, paste0(filename, ".sav"))
  cat("csv and sav files have been written to the working directory.\n")
  return(invisible(data))
}



# --------------------------------------------------

endtime = Sys.time()
cat("\nFinished loading Nadya's QOL upgrades.")
cat("\nTime taken :", (endtime - starttime))
cat("\n####################")
cat("\n")

rm(starttime); rm(endtime)
