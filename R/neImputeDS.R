neImputeDS <- function(object){
  
  object <- eval(parse(text=object), envir = parent.frame())

  out <- medflex::neImpute(object)
  
  return(out)
  
}