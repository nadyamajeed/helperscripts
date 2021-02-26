cat("\n####################")
cat("\nLoading Nadya's functions and other QOL upgrades from Github.")
cat("\n            Version : 0.0.2.9001")
cat("\n       Last updated : 26 Feb 2021, 8:42pm")
cat("\n Loading Package(s) : dplyr")
cat("\nRequired Package(s) : haven (for write_double and unhaven functions)")
cat("\n          Option(s) : Prevent scientific notation.")
cat("\n")

starttime = Sys.time()

library(dplyr)
options(scipen = 9999)

# --------------------------------------------------



# ----- descriptives, summarisers, renamers, etc. ----- 



descStats = function(varname, label = FALSE, dummy = FALSE, compatible = FALSE) {
  
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



descStats.full = function(data, exclude = NULL, split = FALSE, print = TRUE, csv = TRUE, csv_name = "descriptives.csv", debug = FALSE) {
  
  ##### sub-function to extract descriptives #####
  
  descStats.full.sub = function(data, vars) {
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
        
        # run descStats for current variable
        current_descStats = descStats(current_values, dummy = dummy, compatible = TRUE, label = label)
        
        # bind back to table of descriptives
        out = rbind(out, current_descStats)
      }
      
      else {cat("\nSkipping", current_var, "as it is not numeric.\n")}
    }
    return(out)
  }
  
  ##### start of main function #####
  
  if(!is.data.frame(data)) stop("Please pass in a data.frame.")
  
  # exclude variables if requested
  if(!is.null(exclude)) {
    exclusions = data %>% dplyr::select(contains(exclude)) %>% colnames()
    cat("Excluding the following columns:", exclusions, "\n")
    data = data %>% dplyr::select(-contains(exclude))
  }
  
  # retrieve colnames
  vars = colnames(data)
  if(debug) {print(vars)}
  
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
detach(deprecatedHelperScripts)
attach(list(
  dS = function(...) {warning("Function is deprecated. Use descStats() instead."); descStats(...)},
  dS.full = function(...) {warning("Function is deprecated. Use descStats.full() instead."); descStats.full(...)},
  dS.split = function(...) {warning("Function is deprecated. Use descStats.split() instead."); descStats.split(...)}
), name = "deprecatedHelperScripts")



# ----- data manipulation ----- 



centre = function(column) {scale(column, center = TRUE, scale = FALSE)}



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
  invisible(out)
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



# ----- import & export ----- 



unhaven = function(data) {
  return(data %>% haven::zap_labels() %>% haven::zap_label() %>% as.data.frame())
}



write_double = function(data, filename) {
  # writes both .csv and .sav files at once
  write.csv(data, paste0(filename, ".csv"), row.names = F)
  haven::write_sav(data, paste0(filename, ".sav"))
  cat("csv and sav files have been written to the working directory.\n")
  invisible(data)
}



# --------------------------------------------------

endtime = Sys.time()
cat("\nFinished loading Nadya's QOL upgrades.")
cat("\nTime taken :", (endtime - starttime))
cat("\n####################")
cat("\n")

rm(starttime); rm(endtime)
