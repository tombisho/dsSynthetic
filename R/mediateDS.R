#'
#' @title Causal Mediation Analysis
#' @description This function is similar to R function \code{mediate} from the 
#' \code{mediation} package.
#' @details The function 'mediate' is used to estimate various quantities for 
#' causal mediation analysis, including average causal mediation effects 
#' (indirect effect), average direct effects, proportions mediated, and total effect.
#' @param model.m a string character, the name of a fitted model object for mediator.
#' @param model.y a string character, the name of a fitted model object for outcome.
#' @param treat a character string indicating the name of the treatment variable used
#' in the models. The treatment can be either binary (integer or a two-valued factor)
#' or continuous (numeric).
#' @param mediator a character string indicating the name of the mediator variable
#' used in the models.
#' @param boot a logical value. if 'FALSE' a quasi-Bayesian approximation is used for
#' confidence intervals; if 'TRUE' nonparametric bootstrap will be used. Default is 'FALSE'.
#' @param conf.level the level of the returned two-sided confidence intervals. 
#' @param robustSE a logical value. If 'TRUE', heteroskedasticity-consistent standard
#' errors will be used in quasi-Bayesian simulations. Ignored if 'boot' is 'TRUE' or
#' neither 'model.m' nor 'model.y' has a method for vcovHC in the sandwich package. 
#' Default is 'FALSE'.
#' @param sims a number of Monte Carlo draws for nonparametric bootstrap or 
#' quasi-Bayesian approximation.
#' @param seed a number of a seed random number generator. Default value is NULL.
#' @param newobj a character string that provides the name for the output object
#' that is stored on the data servers. Default \code{med.out}.
#' @return a summary table of the object of class 'mediate'.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
mediateDS <- function(model.m, model.y, treat, mediator, boot, conf.level, robustSE, sims, seed, newobj){
  
  model.m <- eval(parse(text=model.m), envir = parent.frame())
  model.y <- eval(parse(text=model.y), envir = parent.frame())
  
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  # med.out <- mediation::mediate(model.m, model.y, sims = sims, boot = boot, boot.ci.type = "perc", 
  #   treat = treat, mediator = mediator, covariates = NULL,
  #   outcome = NULL, control = NULL, conf.level = 0.95, control.value = 0,
  #   treat.value = 1, long = TRUE, dropobs = FALSE, robustSE = robustSE,
  #   cluster = NULL, group.out = NULL, use_speed = FALSE)
  
  med.out <- mediation::mediate(model.m, model.y, sims=sims, boot=boot,
                                treat=treat, mediator=mediator, conf.level=conf.level,
                                robustSE=robustSE)
  
  out <- summary(med.out)
  
  # save the outcome on the server-side
  base::assign(newobj, med.out, envir = parent.frame())
  
  return(out)
  
}
# AGGREGATE FUNCTION
# mediateDS
