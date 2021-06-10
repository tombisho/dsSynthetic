#'
#' @title Expand the dataset and calculate ratio-of-mediator probability weights
#' @description This function is similar to R function \code{neWeight} from the 
#' \code{mmedflex} package.
#' @details The function 'neWeight' both expands the data along hypothetical exposure
#' values and calculates ratio-of-mediator probability weights.
#' @param object a string character, the name of an object used to select a method.
#' @return a data frame of class c("data.frame", "expData", "weightData") is assigned at
#' the server-side of each study.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
neWeightDS <- function(object){
  
  object <- eval(parse(text=object), envir = parent.frame())

  out <- medflex::neWeight(object)
  
  return(out)
  
}
# ASSIGN FUNCTION
