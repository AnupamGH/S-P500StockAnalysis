
#import excel

library(readxl)

 stock_performance_pd4 <- read_excel("Courses/Probability/Project/stock_performance.xlsx", sheet = "4th period")
 stock_performance_pd3 <- read_excel("Courses/Probability/Project/stock_performance.xlsx", sheet = "3rd period")
 stock_performance_pd2 <- read_excel("Courses/Probability/Project/stock_performance.xlsx", sheet = "2nd period")
 stock_performance_pd1 <- read_excel("Courses/Probability/Project/stock_performance.xlsx", sheet = "1st period")
 stock_performance_all <- read_excel("Courses/Probability/Project/stock_performance.xlsx", sheet = "all period")

#stock_performance_pd1 <- read_excel("stock_performance.xlsx", sheet = "1st period")
#stock_performance_pd2 <- read_excel("stock_performance.xlsx", sheet = "2nd period")
#stock_performance_pd3 <- read_excel("stock_performance.xlsx", sheet = "3rd period")
#stock_performance_pd4 <- read_excel("stock_performance.xlsx", sheet = "4th period")
#stock_performance_all <- read_excel("stock_performance.xlsx", sheet = "all period")

# Remove space from column names
library(stringr)
names(stock_performance_pd1)<-str_replace_all(names(stock_performance_pd1), c(" " = "." , "," = "" ))
names(stock_performance_pd2)<-str_replace_all(names(stock_performance_pd2), c(" " = "." , "," = "" ))
names(stock_performance_pd3)<-str_replace_all(names(stock_performance_pd3), c(" " = "." , "," = "" ))
names(stock_performance_pd4)<-str_replace_all(names(stock_performance_pd4), c(" " = "." , "," = "" ))
names(stock_performance_all)<-str_replace_all(names(stock_performance_all), c(" " = "." , "," = "" ))

#***********************************************************************************************************
# 1. ECDF of the random variable: 'Annual return' for all periods

library(lattice)
install.packages('latticeExtra')
library(latticeExtra)

set.seed(42)
vals <- data.frame(annual.return.period1 = stock_performance_pd1$Annual.Return,
                   annual.return.period2 = stock_performance_pd2$Annual.Return,
                   annual.return.period3 = stock_performance_pd3$Annual.Return,
                   annual.return.period4 = stock_performance_pd4$Annual.Return,
                   annual.return.combined = stock_performance_all$Annual.Return)

ecdfplot(~ annual.return.period1 + annual.return.period2 + annual.return.period3 
                      + annual.return.period4 + annual.return.combined, data=vals,
                      auto.key=list(space='top'),xlab = 'Annual returns')

#***********************************************************************************************************
# 2.a Non-Parametric Bootstrapping for standard errors and CI of statistic: Annual return

install.packages("bootstrap")
library(bootstrap)

# plug-in estimator
mu.hat.pd1<-mean(stock_performance_pd1$Annual.Return)
mu.hat.pd2<-mean(stock_performance_pd2$Annual.Return)
mu.hat.pd3<-mean(stock_performance_pd3$Annual.Return)
mu.hat.pd4<-mean(stock_performance_pd4$Annual.Return)
mu.hat.combined<-mean(stock_performance_all$Annual.Return)

theta<-function(x){
  mean(x)
}

results.pd1 <- bootstrap(stock_performance_pd1$Annual.Return,3200,theta)
results.pd2 <- bootstrap(stock_performance_pd2$Annual.Return,3200,theta)
results.pd3 <- bootstrap(stock_performance_pd3$Annual.Return,3200,theta)
results.pd4 <- bootstrap(stock_performance_pd4$Annual.Return,3200,theta)
results.combined <- bootstrap(stock_performance_all$Annual.Return,3200,theta)

#standard error of sampling statistic
se.boot.pd1=sqrt(var(results.pd1$thetastar))
se.boot.pd2=sqrt(var(results.pd2$thetastar))
se.boot.pd3=sqrt(var(results.pd3$thetastar))
se.boot.pd4=sqrt(var(results.pd4$thetastar))
se.boot.combined=sqrt(var(results.combined$thetastar))

# Bootstrap confidence intervals
#period1
normal.ci.pd1<-c(mu.hat.pd1-2*se.boot.pd1, mu.hat.pd1+2*se.boot.pd1)
pivatol.ci.pd1<-c(2*mu.hat.pd1-quantile(results.pd1$thetastar,0.975), 2*mu.hat.pd1-quantile(results.pd1$thetastar,0.025))
quantile.ci.pd1<-quantile(results.pd1$thetastar, c(0.025, 0.975))
normal.ci.pd1;pivatol.ci.pd1;quantile.ci.pd1

#period2
normal.ci.pd2<-c(mu.hat.pd2-2*se.boot.pd2, mu.hat.pd2+2*se.boot.pd2)
pivatol.ci.pd2<-c(2*mu.hat.pd2-quantile(results.pd2$thetastar,0.975), 2*mu.hat.pd2-quantile(results.pd2$thetastar,0.025))
quantile.ci.pd2<-quantile(results.pd2$thetastar, c(0.025, 0.975))
normal.ci.pd2;pivatol.ci.pd2;quantile.ci.pd2

#period3
normal.ci.pd3<-c(mu.hat.pd3-2*se.boot.pd3, mu.hat.pd3+2*se.boot.pd3)
pivatol.ci.pd3<-c(2*mu.hat.pd3-quantile(results.pd3$thetastar,0.975), 2*mu.hat.pd3-quantile(results.pd3$thetastar,0.025))
quantile.ci.pd3<-quantile(results.pd3$thetastar, c(0.025, 0.975))
normal.ci.pd3;pivatol.ci.pd3;quantile.ci.pd3

#period4
normal.ci.pd4<-c(mu.hat.pd4-2*se.boot.pd4, mu.hat.pd4+2*se.boot.pd4)
pivatol.ci.pd4<-c(2*mu.hat.pd4-quantile(results.pd4$thetastar,0.975), 2*mu.hat.pd4-quantile(results.pd4$thetastar,0.025))
quantile.ci.pd4<-quantile(results.pd4$thetastar, c(0.025, 0.975))
normal.ci.pd4;pivatol.ci.pd4;quantile.ci.pd4

#combined period
normal.ci.combined<-c(mu.hat.combined-2*se.boot.combined, mu.hat.combined+2*se.boot.combined)
pivatol.ci.combined<-c(2*mu.hat.combined-quantile(results.combined$thetastar,0.975), 2*mu.hat.combined-quantile(results.combined$thetastar,0.025))
quantile.ci.combined<-quantile(results.combined$thetastar, c(0.025, 0.975))
normal.ci.combined;pivatol.ci.combined;quantile.ci.combined

#density plots
myData <- data.frame(results.pd1$thetastar,results.pd2$thetastar,results.pd3$thetastar,
                     results.pd4$thetastar,results.combined$thetastar)

dens <- apply(myData, 2, density)

plot(NA, xlim=range(sapply(dens, "[", "x")), ylim=range(sapply(dens, "[", "y")),xlab = 'Annual Returns',ylab = 'Density')
mapply(lines, dens, col=1:length(dens))
i <- c('annual.return.period1','annual.return.period2','annual.return.period3','annual.return.period4','annual.return.combined')

legend("topright",legend=i, fill=1:length(i))

#***********************************************************************************************************

# 3. MLE point estimation and asymptotic distributions

#identify the distribution of random variables: 'Annual return'
par(mfrow=c(2,3))
qqnorm(stock_performance_pd1$Annual.Return,main = 'annual.return.period1')
qqline(stock_performance_pd1$Annual.Return)

qqnorm(stock_performance_pd2$Annual.Return,main = 'annual.return.period2')
qqline(stock_performance_pd2$Annual.Return)

qqnorm(stock_performance_pd3$Annual.Return,main = 'annual.return.period3')
qqline(stock_performance_pd3$Annual.Return)

qqnorm(stock_performance_pd4$Annual.Return,main = 'annual.return.period4')
qqline(stock_performance_pd4$Annual.Return)

qqnorm(stock_performance_all$Annual.Return,main = 'annual.return.combined')
qqline(stock_performance_all$Annual.Return)

#shapiro.test for normal
shapiro.test(stock_performance_pd1$Annual.Return)
shapiro.test(stock_performance_pd2$Annual.Return)
shapiro.test(stock_performance_pd3$Annual.Return)
shapiro.test(stock_performance_pd4$Annual.Return)
shapiro.test(stock_performance_all$Annual.Return)


# The QQ plot and MLE estimate indicates that annual return in all the periods except period 3 is 
# approximately normally distributed, MLE is calculated as per the formula used below:

n <- length(stock_performance_pd1$Annual.Return)

mu_mle.pd1<-mean(stock_performance_pd1$Annual.Return)
mu_mle.pd2<-mean(stock_performance_pd2$Annual.Return)
mu_mle.pd4<-mean(stock_performance_pd4$Annual.Return)
mu_mle.combined<-mean(stock_performance_all$Annual.Return)

sigma_hat.pd1<-sqrt((1/n)*sum((stock_performance_pd1$Annual.Return-mu_mle.pd1)^2))
sigma_hat.pd2<-sqrt((1/n)*sum((stock_performance_pd2$Annual.Return-mu_mle.pd2)^2))
sigma_hat.pd3<-sqrt((1/n)*sum((stock_performance_pd3$Annual.Return-mu_mle.pd3)^2))
sigma_hat.pd4<-sqrt((1/n)*sum((stock_performance_pd4$Annual.Return-mu_mle.pd4)^2))
sigma_hat.combined<-sqrt((1/n)*sum((stock_performance_all$Annual.Return-mu_mle.combined)^2))

#Parametric bootstrapping using MLE estimates

pboot <- function (mu,sigma){
   theta.hat_Pbootstrap = vector()
   for(i in 1:3000){
      X_i=rnorm(n,mu,sigma)
      theta.hat_Pbootstrap[i] = mean(X_i)
   }
   theta.hat_Pbootstrap
}

theta.hat.period1=pboot(mu_mle.pd1,sigma_hat.pd1)
theta.hat.period2=pboot(mu_mle.pd2,sigma_hat.pd2)
theta.hat.period3=pboot(mu_mle.pd3,sigma_hat.pd3)
theta.hat.period4=pboot(mu_mle.pd4,sigma_hat.pd4)
theta.hat.combined=pboot(mu_mle.combined,sigma_hat.combined)

theta.hat.period1.se=sd(theta.hat.period1)
theta.hat.period2.se=sd(theta.hat.period2)
theta.hat.period3.se=sd(theta.hat.period3)
theta.hat.period4.se=sd(theta.hat.period4)
theta.hat.combined.se=sd(theta.hat.combined)

Pbootstrap_CI.period1<-c(mu_mle.pd1-2*theta.hat.period1.se,mu_mle.pd1+2*theta.hat.period1.se)
Pbootstrap_CI.period2<-c(mu_mle.pd2-2*theta.hat.period2.se,mu_mle.pd2+2*theta.hat.period2.se)
Pbootstrap_CI.period3<-c(mu_mle.pd3-2*theta.hat.period3.se,mu_mle.pd3+2*theta.hat.period3.se)
Pbootstrap_CI.period4<-c(mu_mle.pd4-2*theta.hat.period4.se,mu_mle.pd4+2*theta.hat.period4.se)
Pbootstrap_CI.combined<-c(mu_mle.combined-2*theta.hat.combined.se,mu_mle.combined+2*theta.hat.combined.se)

mu_mle.pd1;mu_mle.pd2;mu_mle.pd3;mu_mle.pd4;mu_mle.combined
theta.hat.period1.se;theta.hat.period2.se;theta.hat.period3.se;theta.hat.period4.se;theta.hat.combined.se
Pbootstrap_CI.period1;Pbootstrap_CI.period2;Pbootstrap_CI.period3;Pbootstrap_CI.period4;Pbootstrap_CI.combined

#asymptotic normality
par(mfrow = c(2,2))
hist(theta.hat.period1,xlab="annual.return.period.1",main="Sampling distribution of MLE estimate",probability = T)
lines(density(theta.hat.period1, adjust=2))

hist(theta.hat.period2,xlab="annual.return.period.2",main="Sampling distribution of MLE estimate",probability = T)
lines(density(theta.hat.period2, adjust=2))

hist(theta.hat.period4,xlab="annual.return.period.4",main="Sampling distribution of MLE estimate",probability = T)
lines(density(theta.hat.period4, adjust=2))

hist(theta.hat.combined,xlab="annual.return.combined",main="Sampling distribution of MLE estimate",probability = T)
lines(density(theta.hat.combined, adjust=2))

#***********************************************************************************************************

# 5. Hypothesis Testing

annual.return.period1 <- stock_performance_pd1$Annual.Return
annual.return.period2 <- stock_performance_pd2$Annual.Return
annual.return.period3 <- stock_performance_pd3$Annual.Return
annual.return.period4 <- stock_performance_pd4$Annual.Return
annual.return.combined <- stock_performance_all$Annual.Return

#wilcoxon test for comparing median between period 3 and combined period
wilcox.test(annual.return.period3,annual.return.combined,paired = TRUE, exact = F,alternative = "less")  

#paired t-tests for comapring average annual returns

t.test(stock_performance_pd1$Annual.Return,stock_performance_all$Annual.Return, paired = TRUE, alternative = "greater",conf.level = 0.95)
t.test(stock_performance_pd2$Annual.Return,stock_performance_all$Annual.Return, paired = TRUE, alternative = "greater",conf.level = 0.95)
t.test(stock_performance_pd3$Annual.Return,stock_performance_all$Annual.Return, paired = TRUE, alternative = "greater",conf.level = 0.95)
t.test(stock_performance_pd4$Annual.Return,stock_performance_all$Annual.Return, paired = TRUE, alternative = "greater",conf.level = 0.95)

# t-test for testing whether average excess returns are greater than 0

t.test(x = stock_performance_pd1$Excess.Return, alternative = "greater", mu = 0,
       conf.level = 0.95)
t.test(x = stock_performance_pd2$Excess.Return, alternative = "greater", mu = 0,
       conf.level = 0.95)
t.test(x = stock_performance_pd3$Excess.Return, alternative = "greater", mu = 0, 
       conf.level = 0.95)
t.test(x = stock_performance_pd4$Excess.Return, alternative = "greater", mu = 0,
       conf.level = 0.95)
t.test(x = stock_performance_all$Excess.Return, alternative = "greater", mu = 0,
       conf.level = 0.95)

#hypothesis test for testing average absolute winning rate is greater than 0
wilcoxon.test
t.test(x = stock_performance_pd1$Abs..Win.Rate, alternative = "greater", mu = 0.7,
       conf.level = 0.95)
t.test(x = stock_performance_pd2$Abs..Win.Rate, alternative = "greater", mu = 0.7,
       conf.level = 0.95)
t.test(x = stock_performance_pd3$Abs..Win.Rate, alternative = "greater", mu = 0.7,
       conf.level = 0.95)
t.test(x = stock_performance_pd4$Abs..Win.Rate, alternative = "greater", mu = 0.7,
       conf.level = 0.95)
t.test(x = stock_performance_all$Abs..Win.Rate, alternative = "greater", mu = 0.7,
       conf.level = 0.95)

#***********************************************************************************************************
# 5. Bayesian Analysis

install.packages("Bolstad")
library("Bolstad")

par(mfrow=c(1,1))
# Unknown parameter: Mean of Annual Return in period 1
se <- sd(stock_performance_pd1$Annual.Return)
x<-stock_performance_pd1$Annual.Return

##posterior density for mu, the mean of a normal distribution, with a discrete prior on mu
# find the posterior density with a uniform prior on mu
normdp(x,sigma.x=se)

## find the posterior density with a non-uniform prior on mu
mu = seq(-2,2,by=0.2)
length(mu)
mu.prior = runif(length(mu))
mu.prior = sort(mu.prior/sum(mu.prior))
normdp(x,sigma.x=se,mu,mu.prior)

##posterior density for mu, the mean of a normal distribution, with a normal prior on mu
#find the posterior density with a N(0,1) prior on mu
normnp(x,sigma=1)

## find the posterior density with N(0.5,3) prior on mu
normnp(x,0.5,3,1)

# varying the standard deviation of the prior: increase in se_prior results in a decrease in se_posterior 
# varying the mean did not produce any significant difference
normnp(x,0.5,1,1)
normnp(x,0.5,3,1)
normnp(x,0.5,5,1)

# almost similar results are produced when mean annual returns of other periods are considered
#******************************************************************************




