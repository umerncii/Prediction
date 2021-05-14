# First Name: Umer

# Last Name: Iqbal

# Email: umeriqbal0101@gmail.com

# Student No: x17111854
#Reference{
    #https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
    #http://www.r-tutor.com/elementary-statistics/numerical-measures/interquartile-range
    #https://www.linkedin.com/learning/the-data-science-of-marketing/cluster-analysis-with-r?u=2204057
    #https://www.youtube.com/watch?v=qaZNDKFnX_Y&t=489s
#}

library(readxl)
library(readr)
library(xlsx)
library(tidyverse)
library(modeest)
library(dplyr)
library(magrittr)
library(ggplot2)
library(cluster)
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)
library(C50)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(caret)
library(ROCR)
library(rattle)
library(caret)
library(parallel)
library(doParallel)
library(gbm)
library(earth)
library(fastDummies)
library(mlbench)
library(MLmetrics)
install.packages('MLmetrics')
install.packages('timeseries')
install.packages('tseries')
install.packages('quantmod')
install.packages('modeest')
install.packages('rattle')

#Here I'm setting my working directory 
setwd("D:/4th Year/Final Year Project/Mid-Implementation/NewYork Stock Exchange")

#Here I'm importing my prices datasets in RStudio
prices <- read.csv("D:/4th Year/Final Year Project/Mid-Implementation/NewYork Stock Exchange/prices.csv")

#Here I am importing my apple stock financial dataset, which is taken from yahoo.finaance.com
Apple <- read.csv("D:/4th Year/Final Year Project/Mid-Implementation/NewYork Stock Exchange/Apple.csv")

amazon <- read.csv("D:/4th Year/Final Year Project/Mid-Implementation/NewYork Stock Exchange/AMZN.csv")

View(prices)#With this line of codes I can see my prices dataset in new tab 
View(Apple)#We can see our apple dataset in new tab

print(prices)#with this line of code I can see my prices dataset in console tab
print(Apple)#we can see dataset in console tab

#Removing some rows from prices dataset
prices[-c(851264, 851263), ]
prices <- prices[-c(851264, 851263), ]

#With this line of codes we can see values in our prices dataset column
prices[,1]#we can see what is at our index 1
prices[,2]#we can see what is at our index 2
prices[,3]#we can see what is at our index 3
prices[,4]#we can see what is at our index 4
prices[,5]#we can see what is at our index 5
prices[,6]#we can see what is at our index 6
prices[,7]#we can see what is at our index 7

#With this codes we can see values in our Apple dataset
Apple[,1]#we can see what is at our index 1
Apple[,2]#we can see what is at our index 2
Apple[,3]#we can see what is at our index 3
Apple[,4]#we can see what is at our index 4
Apple[,5]#we can see what is at our index 5
Apple[,6]#we can see what is at our index 6
Apple[,7]#we can see what is at our index 7

#Getting our columns names pf stock prices and Apple datasets
colnames(prices)
colnames(Apple)

#Reordering column of prices dataset by their position
prices<-prices[, c(2, 1, 3, 4, 5, 6, 7)]                # All columns in my dataset are re-ordered as I want them to be 
#Reordering column of Apple dataset by their positions
Apple<-Apple[, c(1, 2, 3, 4, 5, 7, 6)]

#Renaming our prices dataset columns names 
names(prices)[names(prices)== "symbol"] <- "Symbol"
names(prices)[names(prices)== "date"] <- "Date"
names(prices)[names(prices)== "open"] <- "Open"
names(prices)[names(prices)== "close"] <- "Close"
names(prices)[names(prices)== "low"] <- "Low"
names(prices)[names(prices)== "high"] <- "High" 
names(prices)[names(prices)== "volume"] <- "Volume"

#Checking names of column
names(Apple)
names(amazon)

#Renaming some apple dataset column names 
names(Apple)[names(Apple)=="Date"] <- "Dates (2010-2016)"

#Adding new columns 
prices[,3]*0.84#Converting Dollars to Euros
prices$euros <- prices[,3]*0.84#Adding column in dataset
names(prices)[names(prices)== "euros"] <- "Opening(In Euros)"#Changing the names of column

prices[,3]*0.75#Converting Dollars to Britan pound
prices$pounds <- prices[,3]*0.75#Adding a new column in dataset
names(prices)[names(prices)== "pounds"] <- "Opening(In pounds)"# changing the name of column

#Checking empty values in Stock prices and Apple datasets
sapply(prices, function(x) sum(is.na(x)))# With this line of code I can check is there any empty rows in my prices dataset
sapply(Apple, function(x) sum(is.na(x)))#With this code I can see that there is no null values in Apple dataset
                                      
                            #Discriptive Statistics Analysis for Stock prices dataset

#count values for Stock prices dataset
count_symbol <- count(prices[,1], na.rm=TRUE)#With this line of code we can see how many symbol values we have.
print(count_symbol)#with print value we can see at bottom we have 851262 values in symbol column

count_Date <- count(prices[,2], na.rm=TRUE)#With this line of code we can see how many dates values we have.
print(count_Date)#with print value we can see at bottom we have 851262 values in Date column

count_Open_Stock <- count(prices[,3], na.rm=TRUE)#With this line of code we can see how many open stock values we have values we have.
print(count_Open_Stock)#with print value we can see at bottom we have 851262 values in Open column

count_close_Stock <- count(prices[,4], na.rm=TRUE)#With this line of code we can see how many close stock values we have values we have.
print(count_close_Stock)#with print value we can see at bottom we have 851262 values in close column

count_low_Stock <- count(prices[,5], na.rm=TRUE)#With this line of code we can see how many low stock values we have values we have.
print(count_low_Stock)#with print value we can see at bottom we have 851262 values in low column

count_high_Stock <- count(prices[,6], na.rm=TRUE)#With this line of code we can see how many high stock values we have values we have.
print(count_high_Stock)#with print value we can see at bottom we have 851262 values in high column

count_volume_Stock <- count(prices[,7], na.rm=TRUE)#With this line of code we can see how many volume stock values we have values we have.
print(count_volume_Stock)#with print value we can see at bottom we have 851262 values in volume column

count_open_euros_Stock <- count(prices[,8], na.rm=TRUE)#With this line of code we can see how many opening in euros stock values we have values we have.
print(count_open_euros_Stock)#with print value we can see at bottom we have 851262 values in opening in euros column

count_open_pounds_Stock <- count(prices[,9], na.rm=TRUE)#With this line of code we can see how many opening in pounds stock values we have values we have.
print(count_open_pounds_Stock)#with print value we can see at bottom we have 851262 values in opening in pounds column

#Mean Values for stock prices dataset
mean_Open <- mean(prices$Open, na.rm = TRUE)#We have our mean value for open column from Stock prices dataset
mean_Close <- mean(prices$Close, na.rm = TRUE)#We have our mean value for close column from Stock prices dataset
mean_Low <- mean(prices$Low, na.rm = TRUE)#We have our mean value for low column from Stock prices dataset
mean_High <- mean(prices$High, na.rm = TRUE)#We have our mean value for High column from Stock prices dataset
mean_Volume <- mean(prices$Volume, na.rm = TRUE)#We have our mean value for Volume column from Stock prices dataset
mean_Euros_Opening <- mean(prices$`Opening(In Euros)`, na.rm = TRUE)#We have our mean value for opening in euros column from Stock prices dataset
mean_Pounds_Opening <- mean(prices$`Opening(In pounds)`, na.rm = TRUE)#We have our mean value for Opening in pounds column from Stock prices dataset

#Median Vaues for stock prices dataset
median_Open <- median(prices$Open, na.rm = TRUE)#We have our median value for open column from Stock prices dataset
median_Close <- median(prices$Close, na.rm = TRUE)#We have our median value for close column from Stock prices dataset
median_Low <- median(prices$Low, na.rm = TRUE)#We have our median value for Low column from Stock prices dataset
median_High <- median(prices$High, na.rm = TRUE)#We have our median value for High column from Stock prices dataset
median_Volume <- median(prices$Volume, na.rm = TRUE)#We have our median value for Volume column from Stock prices dataset
median_Euros_Opening <- median(prices$`Opening(In Euros).Open`, na.rm = TRUE)#We have our median value for opening in euros column from Stock prices dataset
median_Pounds_Opening <- median(prices$`Opening(In Pounds).Open`, na.rm = TRUE)#We have our median value for opening in pounds column from Stock prices dataset

#Mode Values for Stock prices dataset
mlv(prices$Symbol, method = "mfv")#Mode value for column Symbol from stock prices dataset
mlv(prices$Open, method = "mfv")#Mode value for column Open from stock prices dataset
mlv(prices$Close, method = "mfv")#Mode value for column Close from stock prices dataset
mlv(prices$Low, method = "mfv")#Mode value for column Low from stock prices dataset
mlv(prices$High, method = "mfv")#Mode value for column High from stock prices dataset
mlv(prices$Volume, method = "mfv")#Mode value for column Volume from stock prices dataset

#Weighted Mean Values forStock prices dataset
wg_Mean_Open <- weighted.mean(prices$Open, na.rm = TRUE)#Weighted mean value for column open from stock prices dataset
wg_Mean_Close <- weighted.mean(prices$Close, na.rm = TRUE)#Weighted mean value for column close from stock prices dataset
wg_Mean_Low <- weighted.mean(prices$Low, na.rm = TRUE)#Weighted mean value for column low from stock prices dataset
wg_Mean_High <- weighted.mean(prices$High, na.rm = TRUE)#Weighted mean value for column high from stock prices dataset
wg_Mean_Volume <- weighted.mean(prices$Volume, na.rm = TRUE)#Weighted mean value for column volume from stock prices dataset

#Range Values for Stock prices dataset
range_open_prices <- range(prices$Open, na.rm = TRUE)#Range value for column open from stock prices dataset
range_close_prices <- range(prices$Close, na.rm = TRUE)#Range value for column close from stock prices dataset
range_low_prices <- range(prices$Low, na.rm = TRUE)#Range value for column low from stock prices dataset
range_high_prices <- range(prices$High, na.rm = TRUE)#Range value for column high from stock prices dataset
range_volume_prices <- range(prices$Volume, na.rm = TRUE)#Range value for column volume from stock prices dataset

#Standard Deviation for stock prices dataset
sd_open_prices <- sd(prices$Open, na.rm = TRUE)#Standard Deviation value for column open from stock prices dataset
sd_close_prices <- sd(prices$Close, na.rm = TRUE)#Standard Deviation value for column close from stock prices dataset
sd_low_prices <- sd(prices$Low, na.rm = TRUE)#Standard Deviation value for column low from stock prices dataset
sd_high_prices <- sd(prices$High, na.rm = TRUE)#Standard Deviation value for column high from stock prices dataset
sd_volume_prices <- sd(prices$Volume, na.rm = TRUE)#Standard Deviation value for column Volume from stock prices dataset

#Variance for stock prices dataset
variance_Open <- var(prices$Open, na.rm = TRUE)#Variance value for column open from stock prices dataset
variance_Close <- var(prices$Close, na.rm = TRUE)#Variance value for column close from stock prices dataset
variance_Low <- var(prices$Low, na.rm = TRUE)#Variance value for column low from stock prices dataset
variance_High <- var(prices$High, na.rm = TRUE)#Variance value for column high from stock prices dataset
variance_Volume <- var(prices$Volume, na.rm = TRUE)#Variance value for column volume from stock prices dataset

#Quartile for Stock price dataset
quantile_Open <- quantile(prices$Open, na.rm = TRUE)#Quantile value for column open from stock prices dataset
print(quantile_Open)
quantile_Close <- quantile(prices$Close, na.rm = TRUE)#Quantile value for column close from stock prices dataset
print(quantile_Close)
quantile_Low <- quantile(prices$Low, na.rm = TRUE)#Quantile value for column low from stock prices dataset
print(quantile_Low)
quantile_High <- quantile(prices$High, na.rm = TRUE)#Quantile value for column high from stock prices dataset
print(quantile_High)
quantile_Volume <- quantile(prices$Volume, na.rm = TRUE)#Quantile value for column volume from stock prices dataset
print(quantile_Volume)

#Interquartile Range for Stock prices dataset
iqr_Open <- IQR(prices$Open, na.rm = TRUE)#Interquartile Range for column Open from stock prices dataset
print(iqr_Open)
iqr_Close <- IQR(prices$Close, na.rm = TRUE)#Interquartile Range for column close from stock prices dataset
print(iqr_Close)
iqr_Low <- IQR(prices$Low, na.rm = TRUE)#Interquartile Range for column low from stock prices dataset
print(iqr_Low)
iqr_High <- IQR(prices$High, na.rm = TRUE)#Interquartile Range for column high from stock prices dataset
print(iqr_High)
iqr_Volume <- IQR(prices$Volume, na.rm = TRUE)#Interquartile Range for column volume from stock prices dataset
print(iqr_Volume)

                        #Discriptive Statistics Analysis for Apple stock 2010-2016

#Sum values for Apple Stock dataset
sum_Open<-sum(Apple$Open, na.rm = TRUE)#With this code I can see total sum of open column from Apple dataset
print(sum_Open)
sum_High<-sum(Apple$High, na.rm = TRUE)#With this code I can see total sum of High column from Apple dataset
print(sum_High)
sum_Low<-sum(Apple$Low, na.rm = TRUE)#With this code I can see total sum of Low column from Apple dataset
print(sum_Low)
sum_Close<-sum(Apple$Close, na.rm = TRUE)#With this code I can see total sum of Close column from Apple dataset
print(sum_Close)
sum_volume<-sum(Apple$Volume, na.rm = TRUE)#With this code I can see total sum of Volume column from Apple dataset
print(sum_volume)
sum_Adj<-sum(Apple$Adj, na.rm = TRUE)#With this code I can see total sum of Adj.close column from Apple dataset
print(sum_Adj)

#Mean values for Apple Stock Dataset
mean_Apple_Open<-mean(Apple$Open, na.rm = TRUE)#We have our mean value for open column from Apple stock dataset
print(mean_Apple_Open)
mean_Apple_High<-mean(Apple$High, na.rm = TRUE)#We have our mean value for High column from Apple stock dataset
print(mean_Apple_High)
mean_Apple_Low<-mean(Apple$Low, na.rm = TRUE)#We have our mean value for Low column from Apple stock dataset
print(mean_Apple_Low)
mean_Apple_Close<-mean(Apple$Close, na.rm = TRUE)#We have our mean value for close column from Apple stock dataset
print(mean_Apple_Close)
mean_Apple_Volume<-mean(Apple$Volume, na.rm = TRUE)#We have our mean value for volume column from Apple stock dataset
print(mean_Apple_Volume)
mean_Apple_Adj<-mean(Apple$Adj.Close, na.rm = TRUE)#We have our mean value for Adj.Open column from Apple stock dataset
print(mean_Apple_Adj)

#Median Values for Apple stock dataset
median_Apple_Open<- median(Apple$Open, na.rm = TRUE)#We have our median value for open column from Apple stock dataset
print(median_Apple_Open)
median_Apple_High<- median(Apple$High, na.rm = TRUE)#We have our median value for High column from Apple stock dataset
print(median_Apple_High)
median_Apple_Low<- median(Apple$Low, na.rm = TRUE)#We have our median value for Low column from Apple stock dataset
print(median_Apple_Low)
median_Apple_Close<- median(Apple$Close, na.rm = TRUE)#We have our median value for Close column from Apple stock dataset
print(median_Apple_Close)
median_Apple_Volume<- median(Apple$Volume, na.rm = TRUE)#We have our median value for Volume column from Apple stock dataset
print(median_Apple_Volume)
median_Apple_Adj<- median(Apple$Adj.Close, na.rm = TRUE)#We have our median value for Adj.Open column from Apple stock dataset
print(median_Apple_Adj)

#Mode Values for Apple Stock dataset
mlv(Apple$Open, method = "mfv")#Mode value for column Open from Apple dataset
mlv(Apple$High, method = "mfv")#Mode value for column High from Apple dataset
mlv(Apple$Low, method = "mfv")#Mode value for column Low from Apple dataset
mlv(Apple$Close, method = "mfv")#Mode value for column Close from Apple dataset
mlv(Apple$Volume, method = "mfv")#Mode value for column volume from Apple dataset
mlv(Apple$Adj.Close, method = "mfv")#Mode value for column Open from Apple dataset

#Weighted Mean Values forApple Stock dataset
wm_Apple_Open <- weighted.mean(Apple$Open, na.rm = TRUE)#Weighted mean value for column open from Apple Stock dataset
print(wm_Apple_Open)
wm_Apple_High<- weighted.mean(Apple$High, na.rm = TRUE)#Weighted mean value for column High from Apple Stock dataset
print(wm_Apple_High)
wm_Apple_Low <- weighted.mean(Apple$Low, na.rm = TRUE)#Weighted mean value for column Low from Apple Stock dataset
print(wm_Apple_Low)
wm_Apple_Close <- weighted.mean(Apple$Close, na.rm = TRUE)#Weighted mean value for column Close from Apple Stock dataset
print(wm_Apple_Close)
wm_Apple_Volume <- weighted.mean(Apple$Volume, na.rm = TRUE)#Weighted mean value for column Volume from Apple Stock dataset
print(wm_Apple_Volume)
wm_Apple_Adj <- weighted.mean(Apple$Adj.Close, na.rm = TRUE)#Weighted mean value for column Adj.close from Apple Stock dataset
print(wm_Apple_Adj)

#Range Values from Apple Stock Dataset
range_Apple_Open <- range(Apple$Open, na.rm = TRUE)#range for open column from Apple Stock dataset
print(range_Apple_Open)
range_Apple_High <- range(Apple$High, na.rm = TRUE)#range for High column from Apple Stock dataset
print(range_Apple_High)
range_Apple_Low <- range(Apple$Low, na.rm = TRUE)#range for Low column from Apple Stock dataset
print(range_Apple_Low)
range_Apple_Close <- range(Apple$Close, na.rm = TRUE)#range for Close column from Apple Stock dataset
print(range_Apple_Close)
range_Apple_Volume <- range(Apple$Volume, na.rm = TRUE)#range for Volume column from Apple Stock dataset
print(range_Apple_Volume)
range_Apple_Adj <- range(Apple$Adj.Close, na.rm = TRUE)#range for Adj.Open column from Apple Stock dataset
print(range_Apple_Adj)

#Standard deviation Values from Apple Stock dataset
sd_Apple_Open<-sd(Apple$Open, na.rm = TRUE)#Standatd Deviation for Open column
print(sd_Apple_Open)
sd_Apple_High<-sd(Apple$High, na.rm = TRUE)#Standatd Deviation for High column
print(sd_Apple_High)
sd_Apple_Low<-sd(Apple$Low, na.rm = TRUE)#Standatd Deviation for Low column
print(sd_Apple_Low)
sd_Apple_close<-sd(Apple$Close, na.rm = TRUE)#Standatd Deviation for Close column
print(sd_Apple_close)
sd_Apple_Volume<-sd(Apple$Volume, na.rm = TRUE)#Standatd Deviation for Volume column
print(sd_Apple_Volume)
sd_Apple_Adj<-sd(Apple$Adj.Close, na.rm = TRUE)#Standatd Deviation for Adj.Open column
print(sd_Apple_Adj)

#Variance values for Apple Stock dataset
var_Apple_Open<- var(Apple$Open, na.rm = TRUE)#Variance value for open column.
print(var_Apple_Open)
var_Apple_High<- var(Apple$High, na.rm = TRUE)#Variance value for high column.
print(var_Apple_High)
var_Apple_Low<- var(Apple$Low, na.rm = TRUE)#Variance value for low column.
print(var_Apple_Low)
var_Apple_Close<- var(Apple$Close, na.rm = TRUE)#Variance value for Close column.
print(var_Apple_Close)
var_Apple_Volume<- var(Apple$Volume, na.rm = TRUE)#Variance value for Volume column.
print(var_Apple_Volume)
var_Apple_Adj<- var(Apple$Adj.Close, na.rm = TRUE)#Variance value for Adj.open column.
print(var_Apple_Adj)

#Quantile values for Apple Stock dataset
qan_Apple_Open<- quantile(Apple$Open, na.rm = TRUE)#Quantile value for open column.
print(qan_Apple_Open)
qan_Apple_High<- quantile(Apple$High, na.rm = TRUE)#Quantile value for High column.
print(qan_Apple_High)
qan_Apple_Low<- quantile(Apple$Low, na.rm = TRUE)#Quantile value for Low column.
print(qan_Apple_Low)
qan_Apple_Close<- quantile(Apple$Close, na.rm = TRUE)#Quantile value for close column.
print(qan_Apple_Close)
qan_Apple_Volume<- quantile(Apple$Volume, na.rm = TRUE)#Quantile value for volume column.
print(qan_Apple_Volume)
qan_Apple_Adj<- quantile(Apple$Adj.Close, na.rm = TRUE)#Quantile value for Adj.open column.
print(qan_Apple_Adj)

#Interquartile range for Apple Stock dataset
iqr_Apple_Open<- IQR(Apple$Open, na.rm = TRUE)#Interqurtile value for open column.
print(iqr_Apple_Open)
iqr_Apple_High<- IQR(Apple$High, na.rm = TRUE)#Interqurtile value for High column.
print(iqr_Apple_High)
iqr_Apple_Low<- IQR(Apple$Low, na.rm = TRUE)#Interqurtile value for Low column.
print(iqr_Apple_Low)
iqr_Apple_Close<- IQR(Apple$Close, na.rm = TRUE)#Interqurtile value for Close column.
print(iqr_Apple_Close)
iqr_Apple_Volume<- IQR(Apple$Volume, na.rm = TRUE)#Interqurtile value for Volume column.
print(iqr_Apple_Volume)
iqr_Apple_Adj<- IQR(Apple$Adj.Close, na.rm = TRUE)#Interqurtile value for Adj.open column.
print(iqr_Apple_Adj)

                                  #Regression Analysis 

#It is widely used in statistical tool to establish a relationship model between two model.

#Drawing plot
plot(Apple$High, Apple$Low, main ="Regression Analysis",  xlab="Apple Highest stock point", ylab = "Apple lowest stock point", col="red")
#Fitting a line
linearMod <- lm(Apple$Low ~ Apple$High)
#Displaying our line
lines(Apple$High, linearMod$fitted)
#Coefficent
linearMod$coeff

           #K-means Clustering Analysis 
#Let's See our dataset
head(Apple)
#Standarize my data
Apple_cluster_Analysis<- scale(Apple[-1])
#Running KMeans in our standarize data
ourGroups <- kmeans(Apple_cluster_Analysis, 3)
#Visualizing our cluster Analysis 
clusplot(Apple_cluster_Analysis, ourGroups$cluster)
#Summarize our data
ourGroups$size#It gives us size of our three groups that we just created above
ourGroups$betweenss
            #hierarchical clustering 

idx <- sample(1:dim(Apple)[2], 40, replace = TRUE)
appleSample <- Apple[idx,]
appleSample$Open <- NULL

hc <- hclust(dist(appleSample), method="ave")
plot(hc, hang = -1, labels = Apple$Open[idx])

                            #Principal Component Analysis for Apple

PCA=prcomp(Apple[c("Low","High", "Open", "Close")])
PCA
summary(PCA)
plot(PCA)
plot(PCA, type="l")
biplot(PCA)
attributes(PCA)
PCA$rotation
cormat = corFA(Apple)
plot(PCA$x[,1],PCA$x[,2])
pca.var=PCA$sdev^2
pca.var.per<-round(pca.var/sum(pca.var)*100,2)
barplot(pca.var.per,main="Scree plot",xlab="principal component",ylab="percentage variation")
loadingscores<-PCA$rotation[,1]
loadingscores
plot(loadingscores)


                        #Principal Component Analysis for Amazon

PCA=prcomp(amazon[c("Low","High", "Open", "Close")])
PCA
summary(PCA)
plot(PCA)
plot(PCA, type="l")
biplot(PCA)
attributes(PCA)
PCA$rotation
cormat = corFA(amazon)
plot(PCA$x[,1],PCA$x[,2])
pca.var=PCA$sdev^2
pca.var.per<-round(pca.var/sum(pca.var)*100,2)
barplot(pca.var.per,main="Scree plot",xlab="principal component",ylab="percentage variation")
loadingscores<-PCA$rotation[,1]
loadingscores
plot(loadingscores)




                          #Amazon Closing price forecasting analysis 
#Let's load our data 
getSymbols('AMZN', from='2015-01-01', to='2020-12-17')#this my amazon dataset of last 5 years which is directly coming from yahoo finance
class(AMZN)#Class xts zoo
#Lets get the closing prices from our data
AMZN_Close_Price=AMZN[,5]
plot(AMZN_Close_Price)
class(AMZN_Close_Price)#Class is xts 
#Drawing ACF and PACF graph 
par(mfrow=c(1,2))
Acf(AMZN_Close_Price, main='ACF Differentiate')
Pacf(AMZN_Close_Price, main='PACF Differentiate')
#ADF p-value 
print(adf.test(AMZN_Close_Price))#p=0.01
auto.arima(AMZN_Close_Price, seasonal = FALSE)#arima=1,1,3


fitA=auto.arima(AMZN_Close_Price, seasonal = FALSE)
tsdisplay(residuals(fitA), lag.max=4, main='(3,1,4)Residuals')
auto.arima(AMZN_Close_Price, seasonal = FALSE)#AIC/BIC47259.56/47286.13

fitB= arima(AMZN_Close_Price, order = c(1,2,4))
tsdisplay(residuals(fitB), lag.max=4, main='(1,2,4)Residuals')

fitC=arima(AMZN_Close_Price, order = c(5,1,4))
tsdisplay(residuals(fitC), lag.max=4, main='(5,1,4)Residuals')

fitD=arima(AMZN_Close_Price, order = c(1,1,1))
tsdisplay(residuals(fitD), lag.max=4, main='(1,1,1)Residuals')

par(mfrow= c(2,2))
term <- 100
fcast1 <- forecast(fitA, h=term)
plot(fcast1)

fcast2 <- forecast(fitB, h=term)
plot(fcast2)

fcast3 <- forecast(fitC, h=term)
plot(fcast3)

fcast4 <- forecast(fitD, h=term)
plot(fcast4)

#Mape accuracy
accuracy(fcast1)
accuracy(fcast2)
accuracy(fcast3)
accuracy(fcast4)

                        
                    #Amazon high prices (ARIMA forecasting analysis)

#Let's load our data 
getSymbols('AMZN', from='2015-01-01', to='2021-05-04')#this my amazon dataset of last 5 years which is directly coming from yahoo finance
class(AMZN)#Class xts zoo
#Lets get the high prices from our data
AMZN_high_Price=AMZN[,3]
plot(AMZN_high_Price)
class(AMZN_high_Price)#Class is xts 
#Drawing ACF and PACF graph 
par(mfrow=c(1,2))
Acf(AMZN_high_Price, main='ACF Differentiate')
Pacf(AMZN_high_Price, main='PACF Differentiate')
#ADF p-value 
print(adf.test(AMZN_high_Price))#p=0.01
auto.arima(AMZN_high_Price, seasonal = FALSE)#arima=0,1,3

fitA=auto.arima(AMZN_high_Price, seasonal = FALSE)
tsdisplay(residuals(fitA), lag.max=4, main='(3,1,4)Residuals')
auto.arima(AMZN_high_Price, seasonal = FALSE)#AIC/BIC15426.84/15453.71

fitB= arima(AMZN_high_Price, order = c(1,2,4))
tsdisplay(residuals(fitB), lag.max=4, main='(1,2,4)Residuals')

fitC=arima(AMZN_high_Price, order = c(5,1,4))
tsdisplay(residuals(fitC), lag.max=4, main='(5,1,4)Residuals')

fitD=arima(AMZN_high_Price, order = c(1,1,1))
tsdisplay(residuals(fitD), lag.max=4, main='(1,1,1)Residuals')

par(mfrow= c(2,2))
term <- 100
fcast1 <- forecast(fitA, h=term)
plot(fcast1)

fcast2 <- forecast(fitB, h=term)
plot(fcast2)

fcast3 <- forecast(fitC, h=term)
plot(fcast3)

fcast4 <- forecast(fitD, h=term)
plot(fcast4)
#Mape accuracy
accuracy(fcast1)
accuracy(fcast2)
accuracy(fcast3)
accuracy(fcast4)
                    
                      #ARIMA Forecasting of Apple closing prices
#Let's load our data 
getSymbols('AAPL', from='2015-01-01', to='2021-05-04')#this my apple dataset of last 5 years which is directly coming from yahoo finance
class(AAPL)#Class xts zoo
#Lets get the high prices from our data
Apple_closing_Price=AAPL[,5]
plot(Apple_closing_Price)
class(Apple_closing_Price)#Class is xts 
#Drawing ACF and PACF graph 
par(mfrow=c(1,2))
Acf(Apple_closing_Price, main='ACF Differentiate')
Pacf(Apple_closing_Price, main='PACF Differentiate')
#ADF p-value 
print(adf.test(Apple_closing_Price))#p=0.01
auto.arima(Apple_closing_Price, seasonal = FALSE)#arima=1,1,1

fitA=auto.arima(Apple_closing_Price, seasonal = FALSE)
tsdisplay(residuals(fitA), lag.max=4, main='(3,1,4)Residuals')
auto.arima(Apple_closing_Price, seasonal = FALSE)#AIC/BIC60989.93/61006.05

fitB= arima(Apple_closing_Price, order = c(1,2,4))
tsdisplay(residuals(fitB), lag.max=4, main='(1,2,4)Residuals')

fitC=arima(Apple_closing_Price, order = c(5,1,4))
tsdisplay(residuals(fitC), lag.max=4, main='(5,1,4)Residuals')

fitD=arima(Apple_closing_Price, order = c(1,1,1))
tsdisplay(residuals(fitD), lag.max=4, main='(1,1,1)Residuals')

par(mfrow= c(2,2))
term <- 100
fcast1 <- forecast(fitA, h=term)
plot(fcast1)

fcast2 <- forecast(fitB, h=term)
plot(fcast2)

fcast3 <- forecast(fitC, h=term)
plot(fcast3)

fcast4 <- forecast(fitD, h=term)
plot(fcast4)
#Mape accuracy
accuracy(fcast1)
accuracy(fcast2)
accuracy(fcast3)
accuracy(fcast4)


                    #ARIMA forecasting of Apple high prices.
#Let's load our data 
getSymbols('AAPL', from='2015-01-01', to='2021-05-04')#this my apple dataset of last 5 years which is directly coming from yahoo finance
class(AAPL)#Class xts zoo
#Lets get the high prices from our data
Apple_high_Price=AAPL[,3]
plot(Apple_high_Price)
class(Apple_high_Price)#Class is xts 
#Drawing ACF and PACF graph 
par(mfrow=c(1,2))
Acf(Apple_high_Price, main='ACF Differentiate')
Pacf(Apple_high_Price, main='PACF Differentiate')
#ADF p-value 
print(adf.test(Apple_high_Price))#p=0.01
auto.arima(Apple_high_Price, seasonal = FALSE)#arima=1,2,0

fitA=auto.arima(Apple_high_Price, seasonal = FALSE)
tsdisplay(residuals(fitA), lag.max=4, main='(3,1,4)Residuals')
auto.arima(Apple_high_Price, seasonal = FALSE)#AIC/BIC60989.93/61006.05

fitB= arima(Apple_high_Price, order = c(1,2,4))
tsdisplay(residuals(fitB), lag.max=4, main='(1,2,4)Residuals')

fitC=arima(Apple_high_Price, order = c(5,1,4))
tsdisplay(residuals(fitC), lag.max=4, main='(5,1,4)Residuals')

fitD=arima(Apple_high_Price, order = c(1,1,1))
tsdisplay(residuals(fitD), lag.max=4, main='(1,1,1)Residuals')

par(mfrow= c(2,2))
term <- 100
fcast1 <- forecast(fitA, h=term)
plot(fcast1)

fcast2 <- forecast(fitB, h=term)
plot(fcast2)

fcast3 <- forecast(fitC, h=term)
plot(fcast3)

fcast4 <- forecast(fitD, h=term)
plot(fcast4)
#Mape accuracy
accuracy(fcast1)
accuracy(fcast2)
accuracy(fcast3)
accuracy(fcast4)


                        #Decision tree algorithm for Apple dataset
View(Apple) #Let's have a look at datasets

#Here we are creating train and test datasets
set.seed(28022020)
apple.train.index <- sample(1:nrow(Apple),ceiling(0.8*nrow(Apple)))
apple.train <- Apple[apple.train.index,]
apple.test <- Apple[-apple.train.index,]


treeControl <- C5.0Control(
  subset = FALSE,
  bands = 0,
  winnow = FALSE,
  noGlobalPruning = FALSE,
  CF = 0.25,
  minCases = 2,
  fuzzyThreshold = FALSE,
  earlyStopping = TRUE
)
apple.train$Close<-as.factor(apple.train$Close)
str(apple.train$Close)

fit.C5.0 <- C5.0(
  x = apple.train[, -5],
  y = apple.train$Close,
  control = treeControl,
)

summary(fit.C5.0)


plot(fit.C5.0)


C5.0.predict <- predict(
  fit.C5.0,
  apple.test[, -5],
  type = "class"
)
confusionMatrix(C5.0.predict,apple.test[,5])

                          #Random Forest

#Setting up parallel processing
num_cores <- 6
cluster <- makePSOCKcluster(num_cores)
registerDoParallel(cluster)

#Encoding
Apple2_one_hot <- dummy_cols(Apple)
Apple2_dummies <- dummy_cols(Apple, remove_first_dummy = TRUE) 

#Create train and test partitions 
set.seed(42)
train_index <- createDataPartition(
  Apple$Close, 
  p = 0.75,
  list = FALSE
)
apple_train <- Apple[train_index,]
apple_test <- Apple[-train_index,]
apple_train_one_hot <- Apple[train_index,]
apple_test_one_hot <- Apple[-train_index,]
apple_train_dummies <- Apple[train_index,]
apple_test_dummies <- Apple[-train_index,]


repeats = 5
folds = 10

fit_control <- trainControl(
  method = 'repeatedcv',
  number = folds,
  repeats = repeats,
  search = 'random'
)

ptm <- proc.time()

model_tree_fit <- train(
  Close ~ ., 
  data = Apple, 
  method = 'cubist',
  metric = 'MAE',
  tuneLength = 40,
  allowParallel = TRUE,
  trControl = fit_control
)

model_tree_time <- proc.time() - ptm

#Random forest
ptm <- proc.time()

rf_fit <- train(
  Close ~ ., 
  data = Apple,
  method = 'rf',
  metric = 'MAE',
  tuneLength = 40,
  allowParallel = TRUE,
  verbose = FALSE,
  trControl = fit_control
)

random_forest_time <- proc.time() - ptm



