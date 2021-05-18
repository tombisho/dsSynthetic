#'
#' @title Expand the dataset and impute nested counterfactual outcomes
#' @description This function is similar to R function \code{neImpute} from the 
#' \code{mmedflex} package.
#' @details The function 'neImpute' both expands the data along hypothetical exposure
#' values and imputes nested counterfactual outcomes.
#' @param object a string character, the name of an object used to select a method.
#' @return a data frame of class c("data.frame", "expData", "impData") is assigned at
#' the server-side of each study.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
neImputeDS <- function(object){
  
  object <- eval(parse(text=object), envir = parent.frame())

  out <- medflex::neImpute(object)
  
  return(out)
  
}