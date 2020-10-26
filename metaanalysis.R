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



summary_and_forest <- function(rma.mv.output, xlab = "Effect size", specific_label = "") {

  mlab = "Overall effect size"
  if(specific_label != "") {mlab = paste0("Overall effect size for ", specific_label)}

  forest(rma.mv.output, top = 1, xlab = xlab,
         mlab = mlab)

  summary(rma.mv.output)
}



rma.mv_table <- function(rma.mv.output, special_rowname = FALSE) {

  res <- data.frame(
    n = rma.mv.output[["s.nlevels"]][1], k = rma.mv.output[["s.nlevels"]][2],
    effectsize = round5(rma.mv.output$b), se = round5(rma.mv.output$se),
    zval = round5(rma.mv.output$zval), pval = round5(rma.mv.output$pval),
    sig = sigstars(rma.mv.output$pval),
    ci.lb = round5(rma.mv.output$ci.lb), ci.ub = round5(rma.mv.output$ci.ub))
  
  if(special_rowname == FALSE){rownames(res) <- NULL}
  else{rownames(res) <- special_rowname}

  return(res)
}
