library(faraway)
attach(orings)

###Plotting the data
library(ggplot2)
plot(damage/6~orings$temp,orings,ylim=c(0,1),
     xlim=c(25,85),xlab="Temp",ylab="Prob of Damage", las=1)

###Fitting the GLM (as binomial)
modelo = glm(cbind(damage,6-damage)~orings$temp,family=binomial,orings)
summary(modelo)

###Plotting the fitted curve onto previous plot
xt =seq(25,85,.5)
lines(xt,ilogit(11.663-0.2162*xt), col="red")

###Deviance goodness of fit test
pchisq(deviance(modelo),df.residual(modelo),lower=FALSE)

###Pearson goodness of fit test
pchisq(sum(residuals(modelo,type="pearson")^2),21,lower=FALSE)

##########Simulation Study############
nsamp=23
pchisq = numeric(length=1000)
dev = numeric(length=1000)
x1 = orings$temp
for(i in 1:1000){
  
  logitp = 11.663 -0.2162*x1
  y = rbinom(nsamp,6,(1+exp(-logitp))^(-1))
  
  model1 = glm(cbind(y,6-y)~x1,family=binomial)   
  pchisq[i] = sum(residuals(model1,type = "pearson")^2)
  dev[i] = summary(model1)$deviance
}

par(mfrow=c(1,1))
hist(pchisq,prob=T,nclass=25,ylim=c(0,.07),las=1)
lines(seq(min(pchisq),max(pchisq),length=500),dchisq(seq(min(pchisq),max(pchisq),length=500),nsamp-2))

hist(dev,prob=T,nclass=25,ylim=c(0,.1),las=1)
lines(seq(min(dev),max(dev),length=500),dchisq(seq(min(dev),max(dev),length=500),nsamp-2))
##############################################

###Estimating the odds ratio, including a confidence interval
exp(coefficients(modelo)[2])
exp(confint(modelo)[2,])

###Creating new data set with binary responses
borings = with(orings,data.frame(temp=rep(orings$temp,each=6),damage=as.vector(sapply(orings$damage,function(x) rep(c(0,1),times=c(6-x,x))))))
str(borings)
detach(orings)
attach(borings)

###Fitting GLM assuming binary data
modelob = glm(damage~temp, family=binomial, borings)

detach(borings)
attach(troutegg)

###Summarizing the data in a table
ftable(xtabs(cbind(survive,total)~location+period,troutegg))

###Fitting the data using Binomial GLM
bmod = glm(cbind(survive,total-survive)~location+period, family=binomial, troutegg)

###Generating an interaction plot
losurvive = with(troutegg,logit((survive+2)/(total+4)))
with(troutegg,interaction.plot(period,location,losurvive))

###Generating a residual and QQplot
library(statmod)
qplot(bmod$fitted.values,qres.binom(bmod),geom=c('point'),ylab="Residual")
qqnorm(qres.binom(bmod))
abline(a=0,b=1,col="red")

###Estimating the dispersion parameter
phihat = sum(residuals(bmod,type="pearson")^2)/df.residual(bmod)

###Testing the significance of the factors
drop1(bmod,scale=phihat,test="F")

###Summarizing the t-tests
summary(bmod,dispersion=phihat)


###Read in the pesticide data set
library(readxl)
pest <- read_excel("U:/TeX/courses/st526/pest.xlsx")

###Perform a t-test to compare means
attach(pest)
t.test(live[trt==1],live[trt==2])

###Fit a GLM
mod1 = glm(cbind(live,total-live)~as.factor(trt),family=binomial,pest)
summary(mod1)

###Generate a couple diagnostic plots
# library(statmod)
qplot(mod1$fitted.values,qres.binom(mod1),geom=c('point'),
      ylab="Residual",xlab="Fitted Value")
qqnorm(qres.binom(mod1))
abline(a=0,b=1,col="red")
 
####Create a table of counts
bc <- table(trt,live)

####Use GLM estimates to compute expected counts 
nsamp = 100
ntrial=30
dist1 = dbinom(0:max(live),ntrial,mod1$fitted.values[1])
dist2 = dbinom(0:max(live),ntrial,mod1$fitted.values[nsamp+1])
bc1 = rbind(dist1,dist2)

####Group together columns with low counts
grp = c(1:4,rep(5,length=max(live)-3))
bcc = t(rowsum(t(bc),grp)) 
bc1c = t(rowsum(t(bc1),grp)) 

####Compute chi-square goodness of fit
sum((bcc-nsamp*bc1c)^2/(nsamp*bc1c))                        

####Consider quasi-binomial...determine dispersion parameter
phihat = sum(residuals(mod1,type="pearson")^2)/df.residual(mod1)


###Fitting a zero-inflated binomial
library(VGAM)
mod2a = vglm(cbind(live,total-live)~as.factor(trt),lprob="logitlink",
            lonempstro0="logitlink",
            zibinomial,data=pest,trace=TRUE)

mod2 = vglm(cbind(live,total-live)~as.factor(trt),lprob="logitlink",
            lonempstro0="logitlink",zero=null,
            zibinomialff,data=pest,trace=TRUE)


####Again compute the expected counts and assess goodness of fit
zbcoef = coef(mod2)

bc1cz = bc1c
p0 = 1/(1+exp(zbcoef[2]))
p1 = 1/(1+exp(-zbcoef[1]))
bc1cz[1,] = c(p0+(1-p0)*dbinom(0,30,p1),
              (1-p0)*dbinom(1,30,p1),
              (1-p0)*dbinom(2,30,p1),
              (1-p0)*dbinom(3,30,p1),
              (1-p0)*(1-pbinom(3,30,p1)))

p1 = 1/(1+exp(-(zbcoef[1]+zbcoef[3])))
bc1cz[2,] = c(p0+(1-p0)*dbinom(0,30,p1),
              (1-p0)*dbinom(1,30,p1),
              (1-p0)*dbinom(2,30,p1),
              (1-p0)*dbinom(3,30,p1),
              (1-p0)*(1-pbinom(3,30,p1)))  

sum((bcc-nsamp*bc1cz)^2/(nsamp*bc1cz))  


