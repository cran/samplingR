#' @title Strata allocation given a sample size
#' @description Function to allocate the number of samples to be taken for each
#' strata given the total sample size and the allocation method. The number of allocations
#'  returned will be equal to the length of the parameters.
#' @param Nh Vector of population strata sizes.
#' @param n Sample size
#' @param var Vector of strata variances.
#' @param alloc The allocation method to be used. Default is "unif".
#'
#'
#' @return Vector of strata sample sizes.
#' @export
#'
#' @examples
#' allocation(rep(125,4), 100, alloc="unif") #25, 25, 25, 25
#' allocation(c(100, 50, 25), 100, alloc="prop")


allocation<-function(Nh, n, var, alloc=c("unif", "prop", "min", "optim")){
  alloc=match.arg(alloc)

  #Acceptance conditions
  if(alloc != "unif" && alloc != "prop" && alloc != "min" && alloc != "optim")
    stop('Alloc must be one of c("unif", "prop", "min", "optim")')
  if(alloc!="unif" && alloc!="prop" && length(Nh) != length(var)){
    stop("Strata size lenght must be equal to strata variance lenght."); var<-as.array(var)}

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
    return( n*(Nh*sqrt(var))/sum(Nh*sqrt(var)) )
  }
}

