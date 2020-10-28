library(devtools)
source_url("https://raw.githubusercontent.com/nadyaeiou/nadyasscripts/main/all.R")

##########

cat("\n####################")
cat("\nLoading Nadya's latent variable analysis upgrades from Github.")
cat("\nPackage(s): lavaan, lavaanPlot, blavaan, tibble")
cat("\n")

starttime <- Sys.time()

##########



library(lavaan); library(lavaanPlot); library(blavaan); library(tibble)



n.lavaan <- function(model_in, cfa_or_sem, data, show_output = TRUE) {
  if(!is.character(model_in)) {stop("\nPass in a model that lavaan can read.\n")}
  starttime <- Sys.time()
  
  if(cfa_or_sem=="cfa") {out <- model_in %>% cfa(data = data, missing = "fiml", mimic = "Mplus")}
  if(cfa_or_sem=="sem") {out <- model_in %>% sem(data = data, missing = "fiml", mimic = "Mplus")}
  
  lavaanPlot(
    model = out,
    node_options = list(shape = "box", fontname = "Helvetica"),
    edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stand = TRUE, stars = TRUE) %>%
    print()
  
  if(show_output) {summary(out, standardized = TRUE, fit.measures = TRUE)}
  starttime <- Sys.time()
  cat("\n----------\nn.lavaan duration:", starttime - endtime, "\n")
  return(out)
}



n.bsem <- function(
  model_in, data, show_output = TRUE,
  bayesian_cores = 1, bayesian_n.chains = 5, bayesian_burnin = 1000, bayesian_sample = 5000
) {
  if(!is.character(model_in)) {stop("\nPass in a model that lavaan can read.\n")}
  starttime <- Sys.time()
  
  out <-
    model_in %>% bsem(
      data = data, mimic = "Mplus",
      n.chains = bayesian_n.chains, burnin = bayesian_burnin, sample = bayesian_sample,
      bcontrol = list(cores = bayesian_cores)     # 'cores' allows multicore use, faster
    )
  
  if(show_output) {summary(out, standardized = TRUE, fit.measures = TRUE)}
  starttime <- Sys.time()
  cat("\n----------\nn.bsem duration:", starttime - endtime, "\n")
  return(out)
}



##########

endtime <- Sys.time()
cat("\nFinished loading Nadya's latent variable analysis upgrades.")
cat("\nTime taken:", (endtime - starttime))
cat("\n####################")
cat("\n")

rm(starttime); rm(endtime)
