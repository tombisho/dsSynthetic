#'
#' @title Generating synthetic datasets
#' @description This function is similar to R function \code{syn} from the 
#' \code{synthpop} package.
#' @details The function 'syn' is used to generate synthetic versions of a dataset.
#' The following disclosure protections are enforced
#' @param data a dataframe containing the variables to be included in the synthetic dataset
#' @param ... the arguments passed to \code{syn}
#' @return synthetic datasets
#' @author Tom Bishop
#' @export
#'
synDS2 <- function(data, ...){
  responseMessage.combined <- NULL
  
  arguments = list(...)
  data.x <- eval(parse(text=data), envir = parent.frame())
  
  arguments[['data']] <- data.x
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  
  for(i in names(arguments)){
    if(grepl('minbucket', i, fixed = TRUE)){
      if(arguments[[i]] < nfilter.tab){
        arguments[[i]] <- nfilter.tab
        responseMessage.combined <- c(responseMessage.combined,"'minbucket' argument overridden to value set by data custodian.")
      }
    }
  }
  # all the numeric columns should be smoothed:
  sm <- list()
  sm[colnames(data.x[,sapply(data.x, is.numeric)])] <- 'density' # all the numeric columns
  if(length(sm) > 0){
    arguments[['smoothing']] <- sm
    responseMessage.combined <- c(responseMessage.combined,"Smoothing has been applied to numeric columns.")
  }
  if('print.flag' %in% names(arguments)){
    if(arguments[['print.flag']] != FALSE){
      arguments[['print.flag']] = FALSE
      responseMessage.combined <- c(responseMessage.combined,"Printing of synthesising history and information messages has been supressed.")
    }
  }
  # more sanitizing, the drop. args are essential, otherwise it returns the actual data:
  if('drop.not.used' %in% names(arguments)){
    if(arguments[['drop.not.used']] == FALSE){
      arguments[['drop.not.used']] <- TRUE
      responseMessage.combined <- c(responseMessage.combined,"'drop.not.used' parameter overridden to prevent disclosure")
    }
  }
  if('drop.pred.only' %in% names(arguments)){
    if(arguments[['drop.pred.only']] == FALSE){
      arguments [['drop.pred.only']] <- TRUE
      responseMessage.combined <- c(responseMessage.combined,"'drop.pred.only' parameter overridden to prevent disclosure")
    }
  }

  # models and strata should be removed
  if('models' %in% names(arguments)){
    if(arguments[['models']] == TRUE){
      arguments[['models']] <- FALSE
      responseMessage.combined <- c(responseMessage.combined,"'models' parameter overridden to prevent disclosure")
    }
  }
  if('strata' %in% names(arguments)){
    if(arguments[['strata']]){
      arguments[['strata']] <- NULL
      responseMessage.combined <- c(responseMessage.combined,"'strata' parameter overridden to prevent disclosure")
    }
  }

  out <- do.call(synthpop::syn, arguments)

  # if(!is.numeric(Boolean.operator.n) | Boolean.operator.n==0){
  #   studysideMessage<-"FAILED: Boolean.operator specified incorrectly. Must be: '==', '!=', '<', '<=', '>' or '>='"
  #   stop(studysideMessage, call. = FALSE)
  # }
  
  # save the outcome on the server-side
  #base::assign(newobj, syn.out, envir = parent.frame())
  
  return(out)
  
}
# AGGREGATE FUNCTION
# synDS2
