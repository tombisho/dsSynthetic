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
#' @param k number of rows to generate
#' @param proper a logical value with default set to FALSE. If TRUE proper synthesis is conducted.
#' @param seed an integer to be used as an argument for the set.seed(). If no integer is provided, the defaul
#' @return synthetic datasets
#' @author Tom Bishop
#' @export
#'
synDS <- function(data, method, m, k, proper, seed){
  
  data.x <- eval(parse(text=data), envir = parent.frame())

  # if(!is.null(seed)){
  #   set.seed(seed)
  # }
  
  if(is.null(k)){
    k = nrow(data.x)
  }
  
  syn.out <- synthpop::syn(data = data.x, method = method, m = m, k = k,
                           proper = proper, drop.not.used = TRUE, drop.pred.only = TRUE,
                           print.flag = FALSE, seed = seed)
  
  out <- syn.out
  
  # save the outcome on the server-side
  #base::assign(newobj, syn.out, envir = parent.frame())
  
  return(out)
  
}
# AGGREGATE FUNCTION
# synDS
