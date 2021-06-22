#'
#' @title Significance Test for Treatment-Mediator Interaction in Causal Mediation Analysis
#' @description This function is similar to R function \code{test.TMint} from the 
#' \code{mediation} package.
#' @details The function 'test.TMint' is used to test whether the average causal mediation
#' effects and direct effects are significantly different between the treatment and control
#' conditions.
#' @param x the name of the output of the \code{ds.mediate} function saved at each server.
#' @param conf.level the level of the returned two-sided confidence intervals for the effect
#' differences. By default it is set to the value used in the original mediate call.
#' @return an object of class "htest" when applied to a mediate object. See t.test for more
#' explanations of the contents. The function returns an object of class "htest.order" which
#' has its own print method included in this package.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
test.TMintDS <- function(x, conf.level){
  
  x <- eval(parse(text=x), envir = parent.frame())

  if(!is.null(conf.level)){
    conf.level <- x$conf.level
  }
  
  test.TMint.out <- mediation::test.TMint(x, conf.level)
  
  return(test.TMint.out)
  
}
# AGGREGATE FUNCTION
# test.TMintDS
