#'
#' @title Generating synthetic datasets
#' @description This function is similar to R function \code{syn} from the 
#' \code{synthpop} package.
#' @details The function 'syn' is used to generate synthetic versions of a dataset.
#' @param data a dataframe containing the variables to be included in the synthetic dataset
#' @param arguments the arguments passed to \code{syn}
#' @return synthetic datasets
#' @author Tom Bishop
#' @export
#'
synDS2 <- function(data, ...){
  arguments = list(...)
  data.x <- eval(parse(text=data), envir = parent.frame())
  
  arguments[['data']] <- data.x
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  
  for(i in names(arguments)){
    if(grepl('minbucket', i, fixed = TRUE)){
      if(arguments[[i]] < nfilter.tab){
        arguments[[i]] <- nfilter.tab
      }
    }
  }
  # all the numeric columns should be smoothed:
  sm <- list()
  sm[colnames(data.x[,sapply(data.x, is.numeric)])] <- 'density' # all the numeric columns
  if(length(sm) > 0){
    arguments[['smoothing']] <- sm
  }
  arguments[['print.flag']] = FALSE
  # more sanitizing, the drop. args are essential, otherwise it returns the actual data:
  arguments[['drop.not.used']] <- TRUE
  arguments [['drop.pred.only']] <- TRUE
  # models and strata should be removed
  arguments[['models']] <- FALSE
  arguments[['strata']] <- NULL
  out <- do.call(synthpop::syn, arguments)

  # save the outcome on the server-side
  #base::assign(newobj, syn.out, envir = parent.frame())
  
  return(out)
  
}
# AGGREGATE FUNCTION
# synDS2
