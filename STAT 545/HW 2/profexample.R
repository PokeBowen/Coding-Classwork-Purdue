## Author: Anindya Bhadra
## STAT 545
## Example of code profiling and timing: for loop vs. apply()
##Clear workspace and load required libraries
rm(list=ls())
library(MASS)

pow=function(x,lambda){x^lambda}

## Write a function called "rowstat" using for loop to compute row mean and row sd of a matrix
rowstat=function(x){
	rr=c(NA, length=nrow(x))
	rrsq=c(NA, length=nrow(x))
	for (i in 1:nrow(x))
		{rr[i] = sum(x[i,])
		 rrsq[i]=sum(pow(x[i,],2))
	}
	list(mean=rr/ncol(x),sd=sqrt((rrsq - ncol(x)*(pow(rr/ncol(x),2)))/(ncol(x)-1)))
}

Sigma <- matrix(c(10,3,3,2),2,2)

## Generate 10,000 samples from a bivariate Normal distribution with mean 0 and covariance matrix Sigma
xx=mvrnorm(n=10000,c(0,0), Sigma)

## Call the function "rowstat"
pp=rowstat(xx)

## Do the same operations using apply
llm=apply(xx,1,mean)
lls=apply(xx,1,sd)

## Check that they give the same result
head (pp$mean)
head(llm)
head(pp$sd)
head(lls)

## Check the differences in time
system.time(rowstat(xx))
system.time(apply(xx,1,mean)) +  system.time(apply(xx,1,sd)) 

## Use Rprof() to profile the code. Is Rprof() useful here? Where is the maximum time being spent?
Rprof(interval=0.002)
pp=rowstat(xx)
Rprof(NULL)
summaryRprof()

