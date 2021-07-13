#### Visual comparison of whether sampling distribution is close to Normal via Bootstrap
# a function to compare the bootstrap sampling distribution with
#   a normal distribution with mean and SEM estimated from the data
bs.one.samp.dist <- function(dat, N = 1e4){
  n <- length(dat);
  # resample from data
  sam <- matrix(sample(dat, size = N*n, replace = TRUE), ncol = N);
  # draw a histogram of the means
  sam.mean <- colMeans(sam);
  # save par() settings
  old.par <- par(no.readonly = TRUE)
  # make smaller margins
  par(mfrow=c(2,1), mar=c(3,2,2,1),oma=c(1,1,1,1))
  # Histogram overlaid with kernel density curve
  hist(dat, freq = FALSE, breaks = 6, main = "Plot of data with smoothed density curve")
  points(density(dat), type = "l")
  rug(dat)

  hist(sam.mean, freq = FALSE, breaks = 25, main = "Bootstrap sampling distribution of the mean"
       , xlab = paste("Data: n=", n, ",mean=", signif(mean(dat),digits = 5),
                      ",se =", signif(sd(dat)/sqrt(n)),digits=5))
  # overlay a density curve for the sample means
  points(density(sam.mean), type = "l")
  # overlay a normal distribution, bold and red
  x <- seq(min(sam.mean), max(sam.mean), length=1000)
  points(x, dnorm(x, mean = mean(dat), sd = sd(dat)/sqrt(n)),type = "l", lwd = 2,col ="red")
  # place a rug of points under the plot
  rug(sam.mean)
  # restore par() settings
  par(old.par)
}

# example data, skewed ---- try others datasets to develop your intuition
x <- rgamma(10, shape = .5,scale = 20)
bs.one.samp.dist(x)









#### Example: Age at First Transplant
# enter data as a vector
age <- c(54, 42, 51, 54, 49, 56, 33, 58, 54, 64, 49)
par(mfrow=c(2,1))
# Histogram overlaid with kernel density curve
hist(age, freq = FALSE, breaks = 6)
points(density(age),type = "l")
rug(age)

# violin plot
library(vioplot)
vioplot(age, horizontal=TRUE, col="gray")











# stem-and-leaf plot
stem(age, scale = 2)

## 
## The decimal point is 1 digit(s) to the right of the |
##
## 3 | 3
## 3 |
## 4 | 2
## 4 | 99
## 5 | 1444
## 5 | 68
## 6 | 4

# t.crit
qt(1 - 0.05/2, df = length(age) - 1)
## [1] 2.228

# look at help for t.test
?t.test
 # defaults include: alternative = "two.sided", conf.level = 0.95
t.summary <- t.test(age, mu=50)
t.summary
##
## One sample t-test
##
## data: age
## t = 0.5111, df = 10, p-value = 0.6204
## alternative hypothesis: true mean is not equal to 50
## 95 percent confidence interval:
##   45.72  56.82
## sample estimates
## mean of x 
##   51.27
summary(age)
## Min. 1st Qu. Median Mean 3rd Qu. Max.
## 33.0   49.0   54.0  51.3  55.0   64.0
bs.one.samp.dist(age)









# Function ot plot t-distribution with shaded p-value
t.dist.pval <- function(t.summary){
  par(mfrow=c(1,1))
  lim.extreme <- max(4, abs(t.summary$statistic) + 0.5)
  lim.lower <- -lim.extreme;
  lim.upper <-  lim.extreme;
  x.curve <- seq(lim.lower, lim.upper, length=200)
  y.curve <- dt(x.curve, df = t.summary$parameter)
  plot(x.curve, y.curve, type = "n", ylab = paste("t-dist( df=",signif(t.summary$parameter,3),")"),
        xlab= paste("t-stat=",signif(t.summary$statistic,5),
                    ",Shaded area is p-value =", signif(t.summary$p.value,5)))
  if ((t.summary$alternative == "less")
      | (t.summary$alternative == "two.sided")){
    x.pval.l <- seq(lim.lower, -abs(t.summary$statistic), length=200)
    y.pval.l <- dt(x.pval.l, df = t.summary$parameter)
    polygon(c(lim.lower, x.pval.l, -abs(t.summary$statistic))
            ,c(0,y.pval.l,0),col = "gray")
  }
  if ((t.summary$alternative == "greater")|(t.summary$alternative == "two.sided"))
  {
    x.pval.u <- seq(abs(t.summary$statistic), lim.upper, length=200)
    y.pval.u <- dt(x.pval.u, df = t.summary$parameter)
    polygon(c(abs(t.summary$statistic),x.pval.u,lim.upper),c(0, y.pval.u, 0),col = "gray")
  }
  points(x.curve, y.curve, type = "l", lwd =2, col = "blue")
}
# for the age example
t.dist.pval(t.summary)






names(t.summary)

## [1] "statistic"  "parameter"   "p.value"      "conf.int"
## [5] "estimate"   "null.value"  "alternative"  "method"
## [9] "data.name"

t.summary$statistics

##
## 0.5111

t.summary$parameter

## df
## 10

t.summary$p.value

## [1] 0.6204


t.summary$conf.int

## [1] 45.72  56.82
## attr(,"conf.level")
## [1] 0.95

t.summary$estimate

## mean of x
##     51.27

t.summary$null.value

## mean
##   50

t.summary$alternative

## [1] "two.sided"

t.summary$method

## [1] "One sample t-test"

t.summary$data.name

## [1] "age"












#### Example : Meteorites
# enter data as a vector
toco <- c(5.6, 2.7, 6.2, 2.9, 1.5, 4.0, 4.3, 3.0, 3.6, 2.4, 6.7, 3.8)
par(mfrow=c(2,1))
# Histofram overlaid with kernel density curve
hist(toco, freq = FALSE, breaks = 6)
points(density(toco), type="l")
rug(toco)

# violin plot
library(vioplot)
vioplot(toco, horizontal=TRUE, col="gray")

# stem-and-leaf plot
stem(toco, scale = 2)
# t.crit
qt(1 -0.05/2,df=length(toco)-1)
t.summary <- t.test(toco, mu=0.54)
t.summary
summary(toco)
t.dist.pval(t.summary)
bs.one.samp.dist(toco)










