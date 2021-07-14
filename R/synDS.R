#'
#' @title Generating synthetic datasets
#' @description This function is similar to R function \code{syn} from the 
#' \code{synthpop} package.
#' @details The function 'syn' is used to generate synthetic versions of a dataset.
#' @param data a dataframe containing the variables to be included in the synthetic dataset
#' @param method a single string or a vector of strings of length ncol(data)
#' specifying the synthesising method to be used for each variable in the data.
#' Order of variables is exactly the same as in data.
#' @param m number of synthetic copies of the original (observed) data to be generated. The default is m = 1.
#' @param seed an integer to be used as an argument for the set.seed(). If no integer is provided, the defaul
#' @return synthetic datasets
#' @author Tom Bishop
#' @export
#'
synDS <- function(data, method = "cart", m=1, seed){
  
  data.x <- eval(parse(text=data), envir = parent.frame())
  
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  
  
  syn.out <- synthpop::syn(data = data.x, method = method, m = m, seed = seed )
  
  out <- syn.out
  
  # save the outcome on the server-side
  #base::assign(newobj, syn.out, envir = parent.frame())
  
  return(out)
  
}
# AGGREGATE FUNCTION
# synDS
