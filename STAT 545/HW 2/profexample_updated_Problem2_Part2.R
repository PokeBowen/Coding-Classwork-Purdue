## Author: Anindya Bhadra
## STAT 545
## Example of code profiling and timing: for loop vs. apply()
##Clear workspace and load required libraries
rm(list=ls())
library(MASS)

pow=function(x,lambda){x^lambda}

## Write a function called "rowstat" using for loop to compute row medians
rowstat <- function(x){
  r_md <- numeric(length=nrow(x))
  for (i in 1:nrow(x))
  {r_md[i] <- median(x[i,])
  }
  list(median = r_md)
}

Sigma <- matrix(c(10,3,3,2),2,2)

## Generate 15,000 samples from a bivariate Normal distribution with mean 0 and covariance matrix Sigma
xx=mvrnorm(n=15000,c(0,0), Sigma)

## Call the function "rowstat"
pp=rowstat(xx)

## Do the same operations using apply
llm=apply(xx,1,median)

## Check that they give the same result
head (pp$median)
head(llm)

## Check the differences in time
system.time(rowstat(xx))
system.time(apply(xx,1,median))

## Use Rprof() to profile the code. Is Rprof() useful here? Where is the maximum time being spent?
Rprof(interval=0.002)
pp=rowstat(xx)
Rprof(NULL)
summaryRprof()