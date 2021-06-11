#'
#' @title Linear hypotheses for natural effect models
#' @description This function is similar to R function \code{neLht} from the 
#' \code{medflex} package.
#' @details The function \code{neLhtDS} allows to calculate linear combinations of 
#' natural effect model parameter estimates.
#' @param model.name a fitted natural effect model object. This is the object saved
#' on the server-side by the \code{ds.neModel} function.
#' @return a summary table of the object of class c("neLht", "glht") (see glht).
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
neLhtDS <- function(model.name){
  
  # evaluate the object 'model.name' provided as character on the client side
  model <- eval(parse(text=model.name), envir = parent.frame())
  
  neLht.out <- medflex::neLht(model=model)
  out <- summary(neLht.out)
  return(out)
  
}
