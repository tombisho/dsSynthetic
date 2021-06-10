#'
#' @title Fit a natural effect model
#' @description This function is similar to R function \code{neModel} from the 
#'
#' @title Fit a natural effect model
#' @description This function is similar to R function \code{neModel} from the 
#' \code{medflex} package.
#' @details The function 'neModel' is used to fit a natural effect model on the
#' expanded dataset.
#' @param formula a formula object providing a symbolic description of the 
#' natural effect model.
#' @param family aa description of the error distribution and link function to be
#' used in the model. For glm this can be a character string naming a family 
#' function, a family function or the result of a call to a family function. 
#' For glm.fit only the third option is supported.
#' @param expData the expanded dataset (of class "expData").
#' @param se character string indicating the type of standard errors to be calculated.
#' The default type is based on the bootstrap.
#' @param nBoot number of bootstrap replicates.
#' @param newobj a character string that provides the name for the output object
#' that is stored on the data servers. Default \code{neModel.out}. 
#' @return a summary table of the object of class 'neModel'.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
neModelDS <- function(formula, family, expData, se, nBoot, newobj){
  
  # get the value of the 'expData' provided as character on the client side
  expData <- eval(parse(text=expData), envir = parent.frame())
  
  formula <- stats::as.formula(formula)

  neModel.out <- medflex::neModel(formula=formula, family=family, expData=expData, se=se, 
                                  nBoot=nBoot, parallel="no", ncpus=1, progress=FALSE)
  
  out <- summary(neModel.out)
  base::assign(newobj, neModel.out, envir = parent.frame())
  
  return(out)
  
}
