#### Power calculations(that you can do on your own)
# install.packages("pwr")
library(pwr)
?power.t.test
power.t.test(n=NULL, delta = 1.00 - 0.54, sd = sd(toco),
             , sig.level = 0.05, power = 0.50
             , type = "one.sample", alternative = "two.sided")
power.t.test(n = NULL,delta = 0.75 - 0.54, sd = sd(toco),
             , sig.level = 0.05, power = 0.50
             , type = "one.sample", alternative = "two.sided")


install.packages("TeachingDemos")
library(TeachingDemos)
# Demonstrate concepts of Power
?power.examp








#### Example: Weights of canned tomatoes
tomato <- c(20.5, 18.5, 20.0, 19.5, 19.5, 21.0, 17.5
            , 22.5, 20.0, 19.5, 18.5, 20.0, 18.0, 20.5)
par(mfrow=c(2,1))
# Histogram overlaid with kernel density curve
hist(tomato, freq = FALSE, breaks = 6)
points(density(tomato), type = "l")
rug(tomato)

# violin plot
library(vioplot)
vioplot(tomato, horizontal = TRUE, col = "gray")


# t.crit
qt(1 - 0.05/2, df = length(tomato) - 1)
t.summary <- t.test(tomato, mu = 20, alternative = "less")
t.summary
summary(tomato)

t.dist.pval(t.summary)
bs.one.samp.dist(tomato)
