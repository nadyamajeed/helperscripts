rma.mv_table <- function(rma.mv.output, special_rowname = FALSE, debug = FALSE) {

  # count how many estimates (intercept and slopes) have been computed
  estimate_counts <- length(rma.mv.output$b)
  if(debug) {print(estimate_counts)}
  
  # initialise empty dataframe to collect each row
  res_compiled <- data.frame()
  
  # loop through each estimate
  for(i in 1:estimate_counts) {
    
    # store extracted values in single-row dataframe
    res_row <- data.frame(
      n = rma.mv.output[["s.nlevels"]][1], k = rma.mv.output[["s.nlevels"]][2],
      estimate = round5(rma.mv.output$b[i]), se = round5(rma.mv.output$se[i]),
      zval = round5(rma.mv.output$zval[i]), pval = round5(rma.mv.output$pval[i]),
      sig = sigstars(rma.mv.output$pval[i]),
      ci.lb = round5(rma.mv.output$ci.lb[i]), ci.ub = round5(rma.mv.output$ci.ub[i]))
    
    # if debugger enabled, print this row
    if(debug) {print(res_row)}
    
    # bind with master dataframe
    res_compiled <- rbind(res_compiled, res_row)
    
  }
  
  # set rownames
  if(special_rowname == FALSE) {rownames(res_compiled) <- NULL}
  else{
    if(estimate_counts == 1) {rownames(res_compiled) <- special_rowname}
    else{
      list_of_estimates <- c("intercept", rma.mv.output[["call"]][["mods"]])
      special_rowname_stems <- c(rep(special_rowname, estimate_counts))
      special_rowname_compiled <- paste0(special_rowname_stems, list_of_estimates)
      rownames(res_compiled) <- special_rowname_compiled
    }
  }

  return(res_compiled)
}
