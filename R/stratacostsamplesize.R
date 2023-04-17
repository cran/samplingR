

#' @title Strata sample size by costs function
#' @description This function returns the total sample size given a costs function
#' consisting on the fixed total study cost, overhead study cost and a vector of costs by strata.
#' can be given so the allocation is calculated to not exceed the total study cost.
#' @param Nh Vector of population strata sizes.
#' @param C Total study cost.
#' @param cini Overhead study cost.
#' @param ch Vector of costs to take an individual from a strata for the sample.
#' @param alloc The allocation method to be used. Default is "unif".
#'
#' @return Sample size.
#'
#' @details Sample size of "min" and "optim" allocations for a given costs function
#' is the same and equal to the sum of \code{\link{stratacostallocation}} return
#' values with the same costs function.
#'
#' @export
#'
#' @examples
#' stratacostsamplesize(Nh=c(100,500,200), C=1000, cini=70, ch=c(9,5,12), alloc="prop")

stratacostsamplesize<-function(Nh, C, cini, ch, alloc=c("unif", "prop")){
  alloc=match.arg(alloc)

  #Acceptance conditions
  if(alloc != "unif" && alloc != "prop") stop('Alloc must be one of c("unif", "prop")')
  if(missing(C)) stop('Total study cost must be provided.')
  if(missing(ch)) stop('Strata costs vector must be provided.')
  if(!missing(ch) && any(ch<=0)) stop("All strata costs must be greater than 0")
  if(length(Nh) != length(ch)) stop("Strata costs lenght must be equal to strata variance lenght.")
  if(!missing(cini) && cini>=C) stop("Overhead study cost muist be lower than total study cost.")
  if(!missing(C) && missing(cini)) cini<-0

  Nh<-as.vector(Nh)
  N<-sum(Nh)
  Wh<-Nh/N
  if(alloc=="unif"){
    return(((C-cini)*length(ch))/(sum(ch)))
  }
  else if(alloc=="prop"){
    return((C-cini)/sum(ch*Wh))
  }

}


