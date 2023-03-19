#'@title Stratified sample
#'@description With this function you receive a sample of each strata within your
#'data with specified size for each strata.
#'
#'@param data Population data consisting of a number of columns of data and a last
#'column specifying the strata each instance belongs to.
#'@param n Numeric array of sample sizes for each strata to be taken.
#'@param replace Whether the sample to be taken can have repeated instances or not.
#'@return A list containing one strata sample per index.
#'
#'@details n length must be equal to number of strata in data.\cr
#'On return list each strata sample can be accessed calling object$strataname where
#'strataname are values of the last column of the original data.
#'
#'@examples
#'data<-cbind(rnorm(500, 50, 20), rep(c("clase 1", "clase 2","clase 3","clase4"),125))
#'stratifiedsample(data=data, n=c(10,20,30,40))
#'@export

stratifiedsample <- function(data, n, replace=FALSE){

  data<-as.data.frame(data)
  data[,1]<-as.numeric(data[,1])

  clase<-levels(as.factor(data[,-1])) #strata names

  if(length(clase) != length(n)) stop("Strata sample sizes length must be equal to number of strata.")
  domaindata<-list()  #separated strata
  for(i in clase){
    domaindata[[i]]<-data[which(data[,-1]==i),]
  }

  N<-sapply(domaindata, nrow) #Strata sizes

  if(any(n>N)) stop("Strata sample sizes must be smaller or equal than its respective strata size.")

  sample<-list() #sample of each strata
  for(i in 1:length(clase)){
    sample[[i]]<-srs(N=N[i], n=n[i], data=domaindata[[i]], replace=replace)
  }
  names(sample)<-clase
  return(sample)
}

# data<-cbind(rnorm(500, 50, 20), rep(c("clase 1", "clase 2","clase 3","clase4"),125))
# stratifiedsample(n=c(10,20,30,40), data=data)


