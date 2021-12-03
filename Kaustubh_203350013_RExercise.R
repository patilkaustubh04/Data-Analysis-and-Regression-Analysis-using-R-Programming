#Name: Kaustubh Patil; Roll. no.: 203350013
# Set the working directory
setwd("C:\\Users\\Kaustubh Patil\\OneDrive\\Desktop\\GNR640\\R")

#import the libraries
library(readxl)
library(ggplot2)

#Read the data
rain = read_excel('data_annual_precipitation.xlsx')

#Convert data into vectior format

temp = unlist(rain[,-1])
rain_vec = matrix(temp, ncol=1)

#1) Plot histogram and add title in histogram

hist(rain_vec, freq=FALSE, main = 'Histogram of Annual precipitation from year 1910-1970', col='blue', xlab= 'Annual precipitation', ylab='Relative frequence')

#2) Change the number of bins in histogram
hist(rain_vec, freq=FALSE,breaks= 5, main = 'Histogram of Annual precipitation from year 1910-1970', col='blue', xlab= 'Annual precipitation', ylab='Relative frequence')

#3) Plot histogram of logarithm of data
log_rain = log10(rain_vec)
hist(log_rain, freq=FALSE,breaks= 5, main = 'Histogram of Annual precipitation from year 1910-1970', col='blue', xlab= 'Log Annual precipitation', ylab='Relative frequence')

#4) Plot a boxplot. Change boxplot labels on x axis. Replace years with alphabets a, b, c, ...
boxplot(rain[,-1], names=c("a","b","c","d","e","f","g"))

#5) Explore Chi Square test in R. 
#Null hypothesis (H0): the row and the column variables are independent.
#(H1): row and column variables are dependent


chtest = chisq.test(rain[,-1:-2])
chtest

# Observed counts
chtest$observed
# As p-value is close to 0,  the row and the column variables are significantly associated.

# Expected counts
round(chtest$expected,2)

#Pearson residuals
round(chtest$residuals, 3)

#visualize Pearson residuals
library(corrplot)
corrplot(chtest$residuals, is.cor = FALSE)


#Plot empirical and theoretical CDFs. Both plots should be overlayed under single axes.
x=rgamma(rain_vec, 2, 1)
tiff("ecdf and cdf.tiff", width = 4, height = 4, pointsize = 1/300, units = 'in', res = 300)
plot(ecdf(x), xlab = 'Sample', ylab = '', main = 'Empirical Cumluative Distribution\n Precipitation')
curve(pgamma(x, shape = 2, scale = 1), -1, 10, add=TRUE, col="red")

#7) Slide 6 of Lecture 9
#1) Plot scatter diagram between Hydrocarbon level and purity
hc_purity = read_excel('data_linear_regression.xlsx')
#Assign column header
colnames(hc_purity) = c("o","hc","pu")
plot(hc_purity$hc,hc_purity$pu, main = "Scatter plot between Hydrocarbon level and purity",pch=19, frame = FALSE, xlab ="Hydrocarbon level(%)", ylab = "Purity (%)")

#2) Fit linear regression between Hydrocarbon level and purity
model.1 = lm( pu ~ hc, data = hc_purity)
summary(model.1)

#3) Check residuals and verify if the linear regression model is adequate or not
model1.res = resid(model.1)r1 = model.1$residuals
qqnorm(model1.res, main = "Model.1 Residuals")
qqline(model1.res, col="red")
hist(model1.res, breaks=13, col='red')
shapiro.test(model1.res)
#Ans: Residual is approximately normal from qqplot and histogram

#Graph between fitted values and residuals
plot(model.1$fitted.values, model1.res, ylab="Residuals", xlab="Fitted values", main="Graph between fitted values and residuals") 
abline(0, 0)
#Ans: There is no significant pattern observed (error normally distributed) as residuals and the fitted values are uncorrelated

#8) Fit linear regression model with N, E, N^2, E^2, E*N as inputs and Aquifer height as output. 1) Comment if this model resulted in any improvement in R^2. 2) Conduct residual analysis (as in Q. 7)
aq = read.table("AQUIFER.txt", skip=1)
##Assign column header
colnames(aq) = c("E","N","wt")
#convert water table height in ft to m
aq$wt=aq$wt*0.3048

##sumarise data
summary(aq)
##Fit linear regression
N_sq = aq$N * aq$N
E_sq = aq$E * aq$E
ExN = aq$E * aq$N 
model.2 = lm(wt ~ N+E+N_sq +E_sq+ ExN, data = aq)
summary((model.2))

#Check residuals and verify if the linear regression model is adequate or not
model2.res = resid(model.2)
qqnorm(model2.res, main = "Model.2 Residuals")
qqline(model2.res, col="red")
hist(model2.res, breaks=15, col='red')
shapiro.test(model2.res)
#Ans: p-value from shaprio test is 0.003 which is less than 0.05, thus error seems not normaly dist. from qqplot and skewed from histogram

#Graph between fitted values and residuals
plot(model.2$fitted.values, model2.res, ylab="Residuals", xlab="Fitted values", main="Graph between fitted values and residuals") 
abline(0, 0)
#Ans: As the error was not normally distributed, the points are denser at some location as data is skewed.

