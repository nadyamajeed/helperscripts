devtools::source_url("https://raw.githubusercontent.com/nadyamajeed/helperscripts/main/all.R")

##########

cat("\n####################")
cat("\nLoading Nadya's meta-analysis upgrades from Github.")
cat("\nPackage(s): metafor")
cat("\n")

starttime <- Sys.time()

##########



library(metafor)



funnel_and_ranktest <- function(metafor.output, legend = TRUE, xlab = "Effect size") {

  # decrease margins so the full space is used
  par(mar=c(5,4,1,2))

  # draw funnel plot
  funnel(metafor.output, legend = legend, xlab = xlab)

  # rank correlation test, kendall's tau
  ranktest(metafor.output)
}



rma.mv_table <- function(rma.mv.output, special_rowname = "", continuous = FALSE) {
  
  # count how many estimates (intercept and slopes) have been computed
  # if more than 1 (i.e., there are slopes) and continuous == FALSE, STOP the function
  if(length(rma.mv.output$b) > 1 & continuous == FALSE) {
    stop("This function is meant for intercept-only models.\nFor models with slopes (moderators), please ensure you have set continuous = TRUE,\nor use summary() to ensure you get full information.")
  }
  
  # for intercept-only models (no mods)
  if(!continuous) {
    # conduct data extraction
    res <- data.frame(
      'n' = rma.mv.output[["s.nlevels"]][1], 'k' = rma.mv.output[["s.nlevels"]][2],
      'effectsize' = round5(rma.mv.output$b), 'se' = round5(rma.mv.output$se),
      'zval' = round5(rma.mv.output$zval), 'pval' = round3(rma.mv.output$pval, force = TRUE),
      'sig' = sigstars(rma.mv.output$pval),
      'ci.lb' = round5(rma.mv.output$ci.lb), 'ci.ub' = round5(rma.mv.output$ci.ub)
      )
  }
  
  # for slope models (with mods) -- ONLY ONE SLOPE
  if(continuous) {
    # stop function if more than one slope
    if(length(rma.mv.output$b) > 2) {
      stop("This function is meant for models with only one slope (moderator). For models with multiple moderators, please use summary() to ensure you get full information.")
    }
    # conduct data extraction
    res <- data.frame(
      'n' = rma.mv.output[["s.nlevels"]][1], 'k' = rma.mv.output[["s.nlevels"]][2],
      'Q' = paste0(round2(rma.mv.output[["QM"]], force = TRUE), sigstars(rma.mv.output[["QMp"]])),
      'b' = round4(rma.mv.output[["b"]][2]), 'SEb' = round4(rma.mv.output[["se"]][2]),
      'CI' = paste0("[", round4(rma.mv.output[["ci.lb"]][2]), ", ", round4(rma.mv.output[["ci.ub"]][2]), "]")
      )
  }
  
  # set rowname
  if(special_rowname == ""){rownames(res) <- NULL}
  else{rownames(res) <- special_rowname}
  
  # return formatted row
  return(res)
}



summary_and_forest <- function(metafor.output, xlab = "Effect size", specific_label = "", compressed_summary = FALSE) {

  mlab = "Overall effect size"
  if(specific_label != "") {mlab = paste0("Overall effect size for ", specific_label)}

  forest(metafor.output, top = 1, xlab = xlab,
         mlab = mlab)

  if(compressed_summary) {out <- rma.mv_table(metafor.output, special_rowname = specific_label)}
  else {out <- summary(metafor.output)}

  return(out)
}



##########

endtime <- Sys.time()
cat("\nFinished loading Nadya's meta-analysis upgrades.")
cat("\nTime taken:", (endtime - starttime))
cat("\n####################")
cat("\n")

rm(starttime); rm(endtime)
