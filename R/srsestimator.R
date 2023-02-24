#'@title Simple Ramdom Sampling estimator
#'@description Function to make estimations of diferent parameters based on
#'a Simple Random Sample.
#'
#'
#'@param N Size of the total data set.
#'@param data Sample of the data. It must only contain a single column of the
#'data to estimate.
#'@param estimator One of "total", "mean".
#'@param alpha Optional value to determine the estimation error and build 1-alpha
#' confidence interval of the estimator.
#'
#'
#'@return A list containing different interest values:
#' \itemize{
#' \item estimator
#' \item variance
#' \item sampling.error
#' \item estimation.error
#' \item confint
#'}

#'@importFrom stats qnorm
#'
#'@examples
#'data<-rnorm(200, 100, 20)
#'sample<-data[samplingR::srs(200, 50)]
#'tau<-sum(data);tau
#'srsestimator(200, sample, "total", 0.05)
#'
#'@export


srsestimator<-function(N, data, estimator, alpha=0.05){
  if(estimator != "total" && estimator != "mean") stop('Estimator should be one of "total", "mean".')
  if(!missing(alpha) && (alpha<0 || alpha>1)) stop("Alpha value should be between 0 and 1.")
  #if(dim(data)[2]>1) stop("Data must have a single column of data.")
  #Size of the sample
  n<-length(data)
  f<-n/N
  if (estimator == "total"){
    estimator<-N*sum(data/n)
    var<-N*N*(1-f)*var(data)/n
    #sampling error
    serror<-sqrt(var)
  }
  else if(estimator == "mean"){
    estimator<-sum(data/n)
    var<-(1-f)*estimator*(1-estimator)/(n-1)
    serror<-sqrt(var)
  }
  #Estimation error
  if(!missing(alpha)){
    esterror<-qnorm(1-alpha/2)*serror
    confint<-c(estimator-esterror, estimator+esterror)
  }

  return(list("estimator" = estimator, "variance" = var, "sampling.error" = serror,
              "estimation.error" = esterror, "confint" = confint))

}



# data<-rnorm(200, 100, 20)
# dataf<-as.data.frame(data)
# sample<-data[RSampling::srs(200, 50)]
# tau<-sum(data);tau
# srsestimator(200, sample, "total", 0.05)

