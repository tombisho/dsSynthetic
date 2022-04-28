#'
#' @title Generating synthetic datasets
#' @description This function is similar to R function \code{syn} from the 
#' \code{synthpop} package.
#' @details The function 'syn' is used to generate synthetic versions of a dataset.
#' @param data a dataframe containing the variables to be included in the synthetic dataset
#' @param method currently overwritten to CART for disclosure protection
#' @param m number of synthetic copies of the original (observed) data to be generated. The default is m = 1.
#' @param k number of rows to generate, defaults to the length of the data set
#' @param proper a logical value with default set to FALSE. If TRUE proper synthesis is conducted.
#' @param seed an integer to be used as an argument for the set.seed(). If no integer is provided, one will be generated
#' @return synthetic datasets
#' @author Tom Bishop
#' @export
#'
synDS <- function(data, method, m = 1, k=NA, proper = FALSE, seed=NA){
  
  # Override method to CART (which can handle any data type)
  # This is to limit disclosure possibilities in the first version
  # In later versions other methods could be enabled
  method = "cart"
  
  #############################################################
  # CAPTURE THE nfilter SETTINGS - to be used to prevent CART leaves being too small
  # and to prevent leaking information for levels with small counts
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #############################################################
  
  # to work with DSLite
  data.x <- eval(parse(text=data), envir = parent.frame())

  # synthpop vignette recommends using "density" smoothing for numeric variables
  # to reduce the risk of releasing real unusual values so we set the smoothing
  # option for numeric variables
  
  num_list = list()
  nums = unlist(lapply(data.x, is.numeric))
  num_list[colnames(data.x[,nums])] = "density"
  
  if(!is.null(seed)){
     set.seed(seed)
  }
  
  #by default, return the same number of rows as the real data
  if(is.null(k)){
    k = nrow(data.x)
  }
  
  # Check for factor or character columns with small counts
  # and block the synthesis
  
  chars_facts_df = data.x[,unlist(lapply(data.x, function(x) is.factor(x)|is.character(x)))]

  facts_df = as.data.frame(lapply(chars_facts_df, as.factor))
  ansmat.count = unlist(lapply(facts_df, summary))
  any.invalid.cell = any(ansmat.count<nfilter.tab&ansmat.count>0)
  
  if(any.invalid.cell)
  {
    cell.count.warning = paste0("At least one group has between 1 and ", nfilter.tab-1, " observations. Please change groups")
    syn.out = NA
  }
  else{
    cell.count.warning = NA
    syn.out <- synthpop::syn(data = data.x, method = method, m = m, k = k,
                             proper = proper, drop.not.used = TRUE, drop.pred.only = TRUE,
                             print.flag = FALSE, seed = seed, cart.minbucket = nfilter.tab, smoothing = num_list)
  }
    result = list(cell.count.warning, syn.out)
    names(result)<-list("Warning","Data")
    return(result)

  #tidy up
  #eval(quote(rm(offset.to.use)), envir = parent.frame())
  

  
}
# AGGREGATE FUNCTION
# synDS
