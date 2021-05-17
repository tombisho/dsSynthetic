neWeightDS <- function(object){
  
  object <- eval(parse(text=object), envir = parent.frame())

  out <- medflex::neWeight(object)
  
  return(out)
  
}
# ASSIIGN FUNCTION
