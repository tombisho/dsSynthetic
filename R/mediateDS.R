mediateDS <- function(model.m, model.y, treat=treat.name, mediator=med.name, boot=FALSE, robustSE=FALSE, sims=1000, seed=NULL){
  
  model.m <- eval(parse(text=model.m), envir = parent.frame())
  model.y <- eval(parse(text=model.y), envir = parent.frame())
  
  #mget(ls(), envir = parent.frame())
  
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  # med.out <- mediation::mediate(model.m, model.y, sims = sims, boot = boot, boot.ci.type = "perc", 
  #   treat = treat, mediator = mediator, covariates = NULL,
  #   outcome = NULL, control = NULL, conf.level = 0.95, control.value = 0,
  #   treat.value = 1, long = TRUE, dropobs = FALSE, robustSE = robustSE,
  #   cluster = NULL, group.out = NULL, use_speed = FALSE)
  
  med.out <- mediation::mediate(model.m, model.y, sims = sims, boot = boot, 
                                treat = treat, mediator = mediator, robustSE = robustSE)
  
  out <- summary(med.out)
  return(out)
  
}