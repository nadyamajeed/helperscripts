devtools::source_url("https://raw.githubusercontent.com/nadyamajeed/helperscripts/main/all.R")

##########

cat("\n####################")
cat("\nLoading Nadya's latent variable analysis upgrades from Github.")
cat("\n            Version : 0.0.0.9000 (for R version 3.6.3)")
cat("\n        Last update : 20 Mar 2021, 10:19pm")
cat("\n Loading Package(s) : lavaan, lavaanPlot, blavaan, tibble")
cat("\n")

starttime = Sys.time()

library(lavaan); library(lavaanPlot); library(blavaan); library(tibble)

##########



n.lavaan = function(model_in, cfa_or_sem, data, show_output = TRUE) {
  if(!is.character(model_in)) {stop("\nPass in a model that lavaan can read.\n")}
  starttime = Sys.time()
  
  if(cfa_or_sem=="cfa") {out = model_in %>% cfa(data = data, missing = "fiml", mimic = "Mplus")}
  if(cfa_or_sem=="sem") {out = model_in %>% sem(data = data, missing = "fiml", mimic = "Mplus")}
  
  lavaanPlot(
    model = out,
    node_options = list(shape = "box", fontname = "Helvetica"),
    edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE, stars = TRUE) %>%
    print()
  
  if(show_output) {summary(out, standardized = TRUE, fit.measures = TRUE)}
  endtime = Sys.time()
  cat("\n----------\nn.lavaan duration:", endtime - starttime, "\n")
  return(out)
}



n.bcompare = function(bresult1, bresult2, showtime = TRUE) {
  starttime = Sys.time()
  res = invisible(blavCompare(bresult1, bresult2))
  logbf = res[["bf"]][["bf"]]
  names(logbf) = "log BF (positive favours object1)"
  endtime = Sys.time()
  if(showtime) {cat("\n--------------------\nTime taken for model comparison:", (endtime - starttime), "min \n--------------------\n")}
  return(logbf)
}



##########

endtime = Sys.time()
cat("\nFinished loading Nadya's latent variable analysis upgrades.")
cat("\nTime taken:", (endtime - starttime))
cat("\n####################")
cat("\n")

rm(starttime); rm(endtime)
