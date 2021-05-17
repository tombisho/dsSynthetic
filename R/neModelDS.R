neModelDS <- function(formula, family, expData, se, nBoot){
  
  library(medflex)
  
  # get the value of the 'expData' provided as character on the client side
  expData <- eval(parse(text=expData), envir = parent.frame())
  
  formula <- stats::as.formula(formula)

  neModel.out <- medflex::neModel(formula=formula, family=family, expData=expData, se=se, 
                                  nBoot=nBoot, parallel="no", ncpus=1, progress=FALSE)
  out <- summary(neModel.out)
  return(out)
  
}







