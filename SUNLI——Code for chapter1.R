#### Numerical summaries
#### mean
y <- c(5,9,12,30,14,18,32,40)
mean(y)


#### variance
var(y)
sd(y)


#### sorting
sort(y)


#### quartiles
median(y)
fivenum(y)
summary(y)
#IQR
fivenum(y)[c(2,4)]
fivenum(y)[4] - fivenum(y)[2]
diff(fivenum(y)[c(2,4)])


#### remove largest
# remove the largest observation by removing the last of the sorted values
y2<- sort(y)[-length(y)]
y2
median(y2)
fivenum(y2)
diff(fivenum(y2)[c(2,4)])



#### Englishmen
hb <- c(141,148,132,138,154,142,150,146,155,158,150,140,147,148,144,150,149,145)

# see sorted values
sort(hb)

# number of obserbations is the length of the vector (when no missing values)
length(hb)

#default quartiles
summary(hb)

#standard quartiles
fivenum(hb)

#range() gives the min and max values
range(hb)

#the range of the data is the (max-min), calculated using diff()
diff(range(hb))
mean(hb)

#standard deviation
sd(hb)

#standard error of the mean
se <- sd(hb)/sqrt(length(hb))
se





####stripchart -ggplot
# stripchart (dotplot) using R base graphics
# 3 rows, 1 column
par(mfrow=c(3,1))
stripchart(hb,main="Modern Englishman", xlab="head breadth(mm)")
stripchart(hb,method="stack",cex=2,main="larger points(cex=2),method is stack")
stripchart(hb,method="jitter",cex=2,frame.plot=FALSE,main="no frame, method is jitter")

#dotplot using ggplot
library(ggplot2)
# first put hb vector into a data.frame
hb_df <- data.frame(hb)
p1 <- ggplot(hb_df,aes(x = hb))
p1 <- p1 + geom_dotplot(binwidth = 2)
p1 <- p1 + xlab("head breadth(mm)")

p2 <- ggplot(hb_df, aes(x = hb))
p2 <- p2 + geom_dotplot(binwidth = 2, stackdir = "center")
p2 <- p2 + labs(title = "Mondern Englishman head breadth, stackdir=center")
p2 <- p2 + xlab("head breadth(mm)")

p3 <- ggplot(hb_df, aes(x = hb))
p3 <- p3 + geom_dotplot(binwidth = 2, stackdir = "centerwhole")
p3 <- p3 + labs(title = "Modern Englishman head breadth, stackdir=centerwhole")
p3 <- p3 + xlab("head breadth(mm)")

library(gridExtra)
grid.arrange(p1,p2,p3,ncol=1)





#### hist
# histogram using R base graphics
  # par() gives graphical options
  #  mfrow = "multifigure by row" with 1 row and 3 columns
par(mfrow=c(1,3))
  # main is the title, xlab is x-axis label (ylab also available)
hist(hb, main="Modern Englishman", xlab="head breadth(mm)")
  # breaks are how many bins-1 to use
hist(hb, breaks = 15, main = "Histogram, 15 breaks")
  # freq=FALSE changes the vertical axis to density,
  #   so the total area of the bars is now equal to 1
hist(hb, breaks = 8, freq = FALSE, main="Histogram,density")

# histogram using ggplot
library(ggplot2)
# first put hb vector into a data.frame
hb_df <- data.frame(hb)
p <- ggplot(hb_df,aes(x = hb))
  # always specify  a binwidth for the histogram (default is range/30)
  #  try several binwidths
p <- p + geom_histogram(binwidth = 5)
p <- p + labs(title = "Modern Englishman head breadth")
print(p)





#### stem-and-leaf
# stem-and-leaf plot
stem(hb)
# scale=2 makes plot roughly twice as wide
stem(hb,scale = 2)
# scale=5 makes plot roughly five times as wide
stem(hb,scale = 5)




#### boxplot
fivenum(hb)
# boxplot using R base graphics
par(mfrow=c(1,1))
boxplot(hb,horizontal=TRUE,main="Modern Englishman",xlab="head breadth(mm)")
# boxplot using ggplot
library(ggplot2)
# first put hb vector into a data.frame
hb_df <- data.frame(hb)
p <- ggplot(hb_df, aes(x = "hb", y=hb))
p <- p + geom_boxplot()
p <- p + coord_flip()
p <- p + labs(title="Modern Englishman head breadth")
print(p)




#### vioplot
# vioplot using R base graphics
# 3 rows, 1 column
par(mfrow=c(3,1))

# histogram
hist(hb, freq = FALSE, main = "Histogram with kernel density plot, Modern Englishman")
# Histogram overlaid with kernel density curve
points(density(hb),type="l")
# rug of points under histogram
rug(hb)

# violin plot
library(vioplot)
vioplot(hb, horizontal = TRUE, col = "gray")
title("Violin plot, Modern Englishman")

# boxplot
boxplot(hb, horizontal = TRUE, main="Boxplot, Modern Englishman", xlab="head breadth(mm)")




#### Income examples
income <- c(7,1110,7,5,8,12,0,5,2,2,46,7)
# sort in decreasing order
income <- sort(income, decreasing = TRUE)
income
summary(income)
# stem-and-leaf plot
stem(income)




#### remove largest
# remove two largest values (the first two)
income2 <- income[-c(1,2)]
income2
summary(income2)

#stem-and-leaf plot
stem(income2)
# scale=2 makes plot roughly twice as wide
stem(income2, scale = 2)




#### income-boxplot
# boxplot using R base graphics
# 1 row, 3 columns
par(mfrow=c(1,3))
boxplot(income, main="Income")
boxplot(income[-1], main="(remove largest)")
boxplot(income2, main="(remove 2 largest)")




#### Unimodal, symmetric, bell-shaped, and no outliers(Normal distribution)
## base graphics
# sample from normal distribution
x1 <- rnorm(250, mean = 100, sd = 15)

par(mfrow=c(3,1))
# Histogram overlaid with kernel density curve
hist(x1, freq = FALSE, breaks = 20)
points(density(x1), type = "l")
rug(x1)
# boxplot
boxplot(x1,horizontal = TRUE)


## ggplot
# Histogram overlaid with kernel density curve
x1_df <- data.frame(x1)
library(ggplot2)
p1 <- ggplot(x1_df, aes(x = x1))
  # Histogram with density instead of count on y-axis
p1 <- p1 + geom_histogram(aes(y=..density..), binwidth=5, colour="black",fill="white")
  # Overlay with transparent density plot
p1 <- p1 + geom_density(alpha = 0.1, fill="#FF6666")
p1 <- p1 + geom_point(aes(y = -0.001), position = position_jitter(height = 0.0005),alpha = 1/5)


# violin plot
p2 <- ggplot(x1_df, aes(x = "x1",y = x1))
p2 <- p2 + geom_violin(fill = "gray50")
p2 <- p2 + geom_boxplot(width = 0.2, alpha = 3/4)
p2 <- p2 + coord_flip()


#boxplot
p3 <- ggplot(x1_df, aes(x = "x1", y= x1))
p3 <- p3 + geom_boxplot()
p3 <- p3 + coord_flip()

library(gridExtra)
grid.arrange(p1, p2, p3, ncol=1)




#### Central statistical moments
# moments package for 3rd and 4th moments: skewness() and kurtosis()
library(moments)
summary(x1)
sd(x1)
skewness(x1)
kurtosis(x1)
stem(x1)





####Unimodal,symmetric,heavy-tailed
# sample from normal distribution
x2.temp <- rnorm(250,mean = 0, sd = 1)
x2 <- sign(x2.temp)*x2.temp^2*15 + 100
par(mfrow=c(3,1))
# Histogram overlaid with kernel density curve
hist(x2, freq = FALSE, breaks = 20)
points(density(x2), type = "l")
rug(x2)

# violin plot
library(vioplot)
vioplot(x2, horizontal = TRUE, col = "gray")

# boxplot
boxplot(x2, horizontal = TRUE)


# Histogram overlaid with kernel density curve
x2_df <- data.frame(x2)
p1 <- ggplot(x2_df, aes(x = x2))
 #Histogram with density instead of count on y-axis
p1 <- p1 + geom_histogram(aes(y=..density..),binwidth = 5, colour="black",fill="white")
 #Overlay with transparent density plot
p1 <- p1 + geom_density(alpha=0.1,fill="#FF6666")
p1 <- p1 + geom_point(aes(y = -0.001),position = position_jitter(height = 0.0005),alpha = 1/5)

#violin plot
p2 <- ggplot(x2_df, aes(x = "x2",y = x2))
p2 <- p2 + geom_violin(fill = "gray50")
p2 <- p2 + geom_boxplot(width = 0.2,alpha = 3/4)
p2 <- p2 + coord_flip()

#boxplot
p3 <- ggplot(x2_df,aes(x="x2", y=x2))
p3 <- p3 + geom_boxplot()
p3 <- p3 + coord_flip()

library(gridExtra)
grid.arrange(p1,p2,p3,ncol=1)


summary(x2)
sd(x2)
skewness(x2)
kurtosis(x2)
stem(x2)




#### Symmetric,(uniform,) short-tailed
# sample from uniform distribution
x3 <- runif(250, min=50, max=150)
par(mfrow=c(3,1))
# Histogram overlaid with kernel density curve
hist(x3, freq = FALSE, breaks = 20)
points(density(x3),type = "l")
rug(x3)

# violin plot
library(vioplot)
vioplot(x3, horizontal=TRUE, col = "gray")

#bosplot
boxplot(x3, horizontal = TRUE)



# Histogram overlaid with kernel density curve
library(ggplot2)
x3_df <- data.frame(x3)
p1 <- ggplot(x3_df,aes(x = x3))
  # Histogram with density instead of count on y-axis
p1 <- p1 + geom_histogram(aes(y=..density..),binwidth=5,colour="black",fill="white")
  # Overlay with transparent density plot
p1 <- p1 + geom_density(alpha=0.1, fill="#FF6666")
p1 <- p1 + geom_point(aes(y= -0.001),position = position_jitter(height = 0.0005),alpha = 1/5)

# violin plot
p2 <- ggplot(x3_df,aes(x="x3",y=x3))
p2 <- p2+geom_violin(fill="gray50")
p2 <- p2 + geom_boxplot(width = 0.2,alpha = 3/4)
p2 <- p2 + coord_flip()

#boxplot
p3 <- ggplot(x3_df,aes(x = "x3",y = x3))
p3 <- p3 + geom_boxplot()
p3 <- p3 + coord_flip()

library(gridExtra)
grid.arrange(p1, p2, p3, ncol=1)



summary(x3)
sd(x3)
skewness(x3)
kurtosis(x3)
stem(x3)




#### Unimodal, skewed right
# sample from exponential distribution
x4 <- rexp(250, rate = 1)
par(mfrow=c(3,1))
# Histogram overlaid with kernel density curve
hist(x4, freq = FALSE, breaks = 20)
points(density(x4), type = "l")
rug(x4)

# violin plot
library(vioplot)
vioplot(x4, horizontal = TRUE, col = "gray")

# boxplot
boxplot(x4, horizontal = TRUE)

# Histogram overlaid with kernel density curve
x4_df <- data.frame(x4)
p1 <- ggplot(x4_df,aes(x=x4))
  # Histogram with density instead of count on y-axis
p1 <- p1 + geom_histogram(aes(y=..density..),binwidth = 0.5, colour="black", fill="white")
#violin plot
p2 <- ggplot(x4_df, aes(x = "x4",y = x4))
p2 <- p2 + geom_violin(fill = "gray50")
p2 <- p2 + geom_boxplot(width = 0.2,alpha = 3/4)
p2 <- p2 + coord_flip()

# boxplot
p3 <- ggplot(x4_df, aes(x = "x4", y= x4))
p3 <- p3+geom_boxplot()
p3 <- p3 + coord_flip()

library(gridExtra)
grid.arrange(p1, p2, p3, ncol=1)


summary(x4)
sd(x4)
skewness(x4)
kurtosis(x4)
stem(x4)




#### Unimodal, skewed left
# sample from uniform distribution
x5 <- 15- rexp(250,rate = 0.5)
par(mfrow=c(3,1))
# Histogram overlaid with kernel density curve
hist(x5,freq = FALSE, breaks = 20)
points(density(x5), type = "l")
rug(x5)

# violin plot
library(vioplot)
vioplot(x5, horizontal = TRUE, col = "gray")

# boxplot
boxplot(x5, horizontal = TRUE)


# Histogram overlaid with kernel density curve
x5_df <- data.frame(x5)
p1 <- ggplot(x5_df,aes(x=x5))
  #Histogram with density instead of count on y-axis
p1 <- p1 + geom_histogram(aes(y=..density..), binwidth = 0.5, colour="black",fill="white")
  # Overlay with transparent density plot
p1 <- p1 + geom_density(alpha=0.1, fill="#FF6666")
p1 <- p1 + geom_point(aes(y= -0.001),position = position_jitter(height = 0.0005), alpha = 1/5)

# violin plot
p2 <- ggplot(x5_df, aes(x = "x5",y = x5))
p2 <- p2 + geom_violin(fill = "gray50")
p2 <- p2 + geom_boxplot(width=0.2,alpha=3/4)
p2 <- p2 + coord_flip()

# boxplot
p3 <- ggplot(x5_df, aes(x="x5", y= x5))
p3 <- p3 + geom_boxplot()
p3 <- p3 + coord_flip()

library(gridExtra)
grid.arrange(p1, p2, p3, ncol=1)


summary(x5)
sd(x5)
skewness(x5)
kurtosis(x5)
stem(x5)


#### Bimodal (multi-modal)
# sample from uniform distribution
x6 <- c(rnorm(150, mean = 100, sd = 15), rnorm(150, mean = 150, sd = 15))
par(mfrow=c(3,1))
# Histogram overlaid with kernel density curve
hist(x6, freq = FALSE, breaks = 20)
points(density(x6), type = "l")
rug(x6)


# violin plot
library(vioplot)
vioplot(x6, horizontal = TRUE, col = "gray")

# boxplot
boxplot(x6, horizontal = TRUE)


# Histogram overlaid with kernel density curve
x6_df <- data.frame(x6)
p1 <- ggplot(x6_df, aes(x= x6))
    # Histogram with density instead of count on y-axis
p1 <- p1 + geom_histogram(aes(y=..density..),binwidth = 5, colour="black", fill="white")
  #Overlay with transparent density plot
p1 <- p1 + geom_density(alpha=0.1, fill="#FF6666")
p1 <- p1 + geom_point(aes(y = -0.001),position = position_jitter(height = 0.0005), alpha = 1/5)


#violin plot
p2 <- ggplot(x6_df,aes(x="x6",y= x6))
p2 <- p2 + geom_violin(fill = "gray50")
p2 <- p2 + geom_boxplot(width = 0.2,alpha = 3/4)
p2 <- p2 + coord_flip()

# boxplot
p3 <- ggplot(x6_df, aes(x="x6", y=x6))
p3 <- p3 + geom_boxplot()
p3 <- p3 + coord_flip()

library(gridExtra)
grid.arrange(p1,p2,p3,ncol=1)


summary(x6)
sd(x6)
skewness(x6)
kurtosis(x6)
stem(x6)
