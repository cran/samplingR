#' @title Sample size estimation on stratified sampling
#' @description Calculates the required sample size in order to achieve an
#' absolute sampling error  less or equal to the specified for an specific
#' estimator and an optional confidence interval in stratified sampling.
#' @param Nh Vector of population strata sizes.
#' @param var Vector of estimated strata variances.
#' @param error Sampling error.
#' @param alpha Significance level to obtain confidence intervals.
#' @param estimator The estimator to be estimated. Default is "total".
#' @param alloc The allocation to be used when taking samples. Default is "prop".
#' @param replace Whether the samples to be taken can have repeated instances or not.
#'
#' @return Number of instances of the sample to be taken.
#'
#' @details With "proportion" and "class total" estimators variance vector must
#' contain \code{\link{var}} return values equal to p*(1-p)/(Nh-1) values.
#' @export
#'
#' @examples
#' stratasamplesize(Nh=c(120,100,110,50), var=c(458, 313,407,364), error=5, alpha=0.05, "mean", "prop")
#'
stratasamplesize<-function(Nh, var, error, alpha, estimator=c("total", "mean", "proportion", "class total"), alloc=c("prop", "min"), replace=FALSE){
  estimator=match.arg(estimator)
  alloc=match.arg(alloc)

  #Aceptance conditions
  if(!missing(alpha) && (alpha<0 || alpha>1)) stop("Alpha value must range between 0 and 1.")
  if(estimator != "total" && estimator != "proportion" && estimator!="mean" &&
     estimator!="class total") stop('Estimator must be one of c("none", "total", "proportion", "mean", "class total").')
  if(alloc != "prop" && alloc != "min") stop('Alloc must be one of c("prop", "min")')
  if(length(var) != length(Nh)) stop("Strata size lenght must be equal to strata variance lenght")
  if( (estimator=="proportion" || estimator == "class total" )
      && ( (any(var<0) || any(var>0.3)) )) stop("Strata variance values must range between 0 and 0.3")
  if((estimator=="proportion" || estimator=="class total") && missing(var)) var<-rep(0.3, length(Nh))


  var<-as.array(var)
  #Population strata sizes
  Nh<-as.array(Nh)
  #Population size
  N<-sum(Nh)
  #Relative strata size coeficients
  Wh<-Nh/N


  #stratified without replacement
  if(!replace){
    if(missing(alpha)){

      if(alloc == "prop"){
        if(estimator == "mean"){
          a<-sum(Wh*var)
          return( ceiling(a/(error*error+a/N)) )
        }
        else if(estimator == "proportion"){
          a<-sum(Wh*var)
          return( ceiling(a/(error*error+a/N)) )
        }
        else if(estimator == "total"){
          a<-sum(Nh*var)
          return( ceiling( (N*a) / (error*error+a)) )
        }
        else{
          a<-sum(Nh*var)
          return( ceiling( (N*a) / (error*error+a)) )
        }
      }

      else if(alloc == "min"){
        if(estimator == "mean"){
          b<-sum(Wh*sqrt(var))^2
          a<-sum(Wh*var)
          return( ceiling(b/(error^2+a/N)))
        }
        else if(estimator=="proportion"){
          b<-sum(Wh*sqrt(var))^2
          a<-sum(Wh*var)
          return( ceiling(b/(error^2+a/N)))
        }
        else if(estimator=="total"){
          b<-sum(Nh*sqrt(var))^2
          a<-sum(Nh*var)
          return( ceiling(b/(error^2+a)))
        }
        else{
          b<-sum(Nh*sqrt(var))^2
          a<-sum(Nh*var)
          return( ceiling(b/(error^2+a)))
        }
      }
    }
    #Confidence interval
    else{
      k<-qnorm(1-alpha/2)
      if(alloc == "prop"){
        if(estimator == "mean"){
          a<-sum(Wh*var)
          return( ceiling(a/((error^2/k^2)+a/N)) )
        }
        else if(estimator == "proportion"){
          a<-sum(Wh*var)
          return( ceiling(a/((error^2/k^2)+a/N)) )
        }
        else if(estimator == "total"){
          a<-sum(Nh*var)
          return( ceiling( (N*a) / ((error^2/k^2) +a) ) )
        }
        else{
          a<-sum(Nh*var)
          return( ceiling( (N*a) / ((error^2/k^2)+a) ) )
        }
      }
      #minimum variance allocation
      else {
        if(estimator == "mean"){
          b<-sum(Wh*sqrt(var))^2
          a<-sum(Wh*var)
          return( ceiling(b/( (error^2/k^2)+a/N)) )
        }
        else if(estimator=="proportion"){
          b<-sum(Wh*sqrt(var))^2
          a<-sum(Wh*var)
          return( ceiling(b/( (error^2/k^2)+a/N)) )
        }
        else if(estimator=="total"){
          b<-sum(Nh*sqrt(var))^2
          a<-sum(Nh*var)
          return( ceiling(b/ ((error^2/k^2)+a)) )
        }
        else{
          b<-sum(Nh*sqrt(var))^2
          a<-sum(Nh*var)
          return( ceiling(b/ ((error^2/k^2)+a)) )
        }
      }
    }
  }
  #stratified with replacement
  else{
    var<-(Nh-1)*var #var<-p*q

    if(missing(alpha)){
      if(alloc == "prop"){
        if(estimator == "mean"){
          a<-sum(Wh*var)
          return( ceiling(a/(error^2)) )
        }
        else if(estimator == "proportion"){
          a<-sum(Wh*var)
          return( ceiling(a/(error^2)) )
        }
        else if(estimator == "total"){
          a<-sum(Nh*var)
          return( ceiling( (N*a) / (error^2)) )
        }
        else{
          a<-sum(Nh*var)
          return( ceiling( (N*a)/(error^2)) )
        }
      }
      #minimum variance allocation
      else{
        if(estimator == "mean"){
          b<-sum(Wh*sqrt(var))^2
          return( ceiling(b/(error^2)) )
        }
        else if(estimator=="proportion"){
          b<-sum(Wh*sqrt(var))^2
          return( ceiling(b/(error^2)) )
        }
        else if(estimator=="total"){
          b<-sum(Nh*sqrt(var))^2
          return( ceiling(b/(error^2) ) )
        }
        else{
          b<-sum(Nh*sqrt(var))^2
          return( ceiling(b/(error^2) ) )
        }
      }
    }
    else{
      k<-qnorm(1-alpha/2)
      if(alloc=="prop"){
        if(estimator == "mean"){
          a<-sum(Wh*var)
          return( ceiling(a/(error^2/k^2)) )
        }
        else if(estimator == "proportion"){
          a<-sum(Wh*var)
          return( ceiling(a/(error^2/k^2)) )
        }
        else if(estimator == "total"){
          a<-sum(Nh*var)
          return( ceiling( (N*a) / (error^2/k^2)) )
        }
        else{
          a<-sum(Nh*var)
          return( ceiling( (N*a) / (error^2/k^2)) )
        }
      }
      else{

        if(estimator == "mean"){
          b<-sum(Wh*sqrt(var))^2
          return( ceiling(b/(error^2/k^2)) )
        }
        else if(estimator=="proportion"){
          b<-sum(Wh*sqrt(var))^2
          return( ceiling(b/(error^2/k^2)) )
        }
        else if(estimator=="total"){
          b<-sum(Nh*sqrt(var))^2
          return( ceiling(b/(error^2/k^2) ) )
        }
        else{
          b<-sum(Nh*sqrt(var))^2
          return( ceiling(b/(error^2/k^2) ) )
        }
      }

    }

  }


}

# Nh=rep(125,4)
# var=c(458.3932,313.914,407.8838,364.1714)
# stratasamplesize(Nh, var, 5, estimator="mean", alloc="prop", alpha=0.05)




