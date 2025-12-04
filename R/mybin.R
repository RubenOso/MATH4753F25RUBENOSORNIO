#' Title
#'
#' @param iter Number of simulated experiments. Default is 100.
#' @param n Number of trials per experiment. Default is 10.
#' @param p Probability of success for each trial. Default is 0.5.
#'
#' @return A named numeric vector giving the proportion of each possible
#' @export
#'
#' @examples
#' mybin(iter = 100, n = 10, p = 0.5)
#' mybin(iter = 1000, n = 18, p = 0.3)
mybin=function(iter=100,n=10, p=0.5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA, nrow = n, ncol = iter, byrow = TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}


