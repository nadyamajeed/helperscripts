library(devtools)
source_url("https://raw.githubusercontent.com/nadyaeiou/nadyasscripts/main/all.R")



library(metafor)



funnel_and_ranktest <- function(metafor.output, legend = TRUE, xlab = "Effect size") {

  # decrease margins so the full space is used
  par(mar=c(5,4,1,2))

  # draw funnel plot
  funnel(metafor.output, legend = legend, xlab = xlab)

  # rank correlation test, kendall's tau
  ranktest(metafor.output)
}



rma.mv_table <- function(rma.mv.output, special_rowname = FALSE) {

  # count how many estimates (intercept and slopes) have been computed
  # if more than 1 (i.e., there are slopes) STOP the function
  if(length(rma.mv.output$b) > 1) {
    stop("This function is meant for intercept-only models. For models with slopes (moderators), please use summary() to ensure you get full information.")
  }
  
  # conduct data extraction
  res <- data.frame(
    n = rma.mv.output[["s.nlevels"]][1], k = rma.mv.output[["s.nlevels"]][2],
    effectsize = round5(rma.mv.output$b), se = round5(rma.mv.output$se),
    zval = round5(rma.mv.output$zval), pval = round5(rma.mv.output$pval),
    sig = sigstars(rma.mv.output$pval),
    ci.lb = round5(rma.mv.output$ci.lb), ci.ub = round5(rma.mv.output$ci.ub))
  
  # set rowname
  if(special_rowname == FALSE){rownames(res) <- NULL}
  else{rownames(res) <- special_rowname}

  # return formatted row
  return(res)
}



summary_and_forest <- function(metafor.output, xlab = "Effect size", specific_label = "", compressed_summary = FALSE) {

  mlab = "Overall effect size"
  if(specific_label != "") {mlab = paste0("Overall effect size for ", specific_label)}

  forest(metafor.output, top = 1, xlab = xlab,
         mlab = mlab)

  if(compressed_summary) {rma.mv_table(metafor.output)}
  else {summary(metafor.output)}
}
