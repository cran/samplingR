#' @title Strata allocation by costs function
#' @description This function returns the optimum allocation by strata given a
#' vector of costs by strata. Optionally the fixed total study cost and overhead study cost
#' can be given so the allocation is calculated to not exceed the total study cost.
#'
#' @param Nh Vector of population strata sizes.
#' @param n Sample size.
#' @param var Vector of strata variances.
#' @param C Total study cost.
#' @param cini Overhead study cost.
#' @param ch Vector of costs to take an individual from a strata for the sample.
#'
#' @return Vector of strata sample sizes.
#' @export
#'
#' @examples
#' stratacostallocation(rep(125,4),50,c(458,313,407,364),C=25000,cini=2000,ch=c(100,150,20,200))

stratacostallocation<-function(Nh, n, var, C, cini, ch){

  if(missing(ch)) stop('Strata costs vector must be provided.')
  if(!missing(ch) && any(ch<=0)) stop("All strata costs must be greater than 0")
  if(length(Nh) != length(ch)) stop("Strata costs lenght must be equal to strata variance lenght.")
  if(!missing(cini) && cini>=C) stop("Overhead study cost muist be lower than total study cost.")
  if(!missing(C) && missing(cini)) cini<-0

  var<-as.array(var)
  #Population strata sizes
  Nh<-as.array(Nh)
  #Population size
  N<-sum(Nh)
  #Relative strata size coeficients
  Wh<-Nh/N

  if(missing(C)){
    a<-Nh*sqrt(var)/sqrt(ch)
    return(n*a/sum(a))
  }
  else{
    return( ((C-cini)*Nh*sqrt(var)/sqrt(ch))/sum(Nh*sqrt(var)*sqrt(ch)))
  }
}


#cini<-2000
#C<-25000
#ch<-c(100,150,20,200)
#sizes<-stratacostallocation(rep(125,4), var=c(458, 313,407,364), n<-50, ch=ch, C=C, cini=cini);sizes

#sum(floor(sizes)*ch)+cini
