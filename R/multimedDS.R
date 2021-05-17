multimedDS <- function(outcome=outcome.name, med.main=med.main.name, med.alt = NULL, treat=treat.name, covariates.transmit = NULL, data=data.name, sims=1000){
  
  data <- eval(parse(text=data), envir = parent.frame())
  
  covariates <- unlist(strsplit(covariates.transmit, split=","))

  m.med.out <- mediation::multimed(outcome=outcome, med.main=med.main, med.alt = med.alt, treat=treat, covariates = covariates,
                                 experiment = NULL, data=data, design = c("single", "parallel"),
                                 sims = sims, R2.by = 0.01, conf.level = 0.95, weight = NULL)
  
  out <- summary(m.med.out)
  return(out)
  
}