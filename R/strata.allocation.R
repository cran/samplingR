#' @title Strata allocation given a sample size
#' @description Function to allocate the number of samples to be taken for each
#' strata given the total sample size and the strata.allocation method. The number of allocations
#'  returned will be equal to the length of the parameters.
#' @param Nh Vector of population strata sizes.
#' @param n Sample size
#' @param var Vector of strata variances.
#' @param ch Vector of costs to take an individual from a strata for the sample.
#' @param alloc The allocation method to be used. Default is "unif".
#'
#'
#' @return Vector of strata sample sizes.
#' @export
#'
#' @examples
#' strata.allocation(Nh=rep(125,4), n=100, alloc="unif") #25, 25, 25, 25
#' strata.allocation(Nh=c(100, 50, 25), n=100, alloc="prop")


strata.allocation<-function(Nh, n, var, ch, alloc=c("unif", "prop", "min", "optim")){
  alloc=match.arg(alloc)
  if(!missing(var)) var<-as.array(var)
  if(!missing(ch)) var<-as.array(ch)

  #Acceptance conditions
  if(alloc != "unif" && alloc != "prop" && alloc != "min" && alloc != "optim")
    stop('Alloc must be one of c("unif", "prop", "min", "optim")')
  if((alloc=="min" || alloc=="optim") && missing(var))stop('Strata variance values must be given for alloc=c("min","optim")')

  if(alloc!="unif" && alloc!="prop" && length(Nh) != length(var))
    stop("Strata size lenght must be equal to strata variance lenght.")
  if( (alloc=="min" || alloc=="optim") && length(Nh) != length(var)) stop("Strata variance length must be equal to strata sizes length")
  if(alloc=="optim" && missing(ch))stop('Strata cost values must be given for alloc="optim"')
  if((alloc=="min" || alloc=="optim") && length(Nh) != length(ch)) stop("Strata costs length must be equal to strata sizes length")

  #Population strata sizes
  Nh<-as.array(Nh)
  #Population size
  N<-sum(Nh)

  if(alloc=="unif"){
    return(rep(ceiling(n/length(Nh)), length(Nh)))
    #nh<-floor(n/length(Nh))
    #i<-1
    #while (sum(nh)!=n) {
    #  nh[i]<-nh[i]+1
    #  i<-i+1
    #}
    #return(nh)
  }
  else if(alloc=="prop"){
    k<-n/N
    return(Nh*k)
  }
  else if(alloc=="min"){
    return( n*(Nh*sqrt(var))/(sum(Nh*sqrt(var))) )
  }
  else{
    a<-Nh*sqrt(var)/sqrt(ch)
    return(n*a/sum(a))
  }
}

