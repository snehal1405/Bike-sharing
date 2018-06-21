####Source: https://www.kaggle.com/marklvl/bike-sharing-dataset 

#Read day.csv file
bikedata <- read.csv("day.csv") 
#Total number of rows in day.csv using nrow()
nrow(bikedata)
#Total number of columns in day.csv using ncol()
ncol(bikedata)
#Dimension of day.csv using dim()
dim(bikedata)
#Summary of bikedata using summary()
summary(bikedata)
#Names of fields using names()
names(bikedata)
#Structure of data using str()
str(data)
#Attributes of data using attributes()
attributes(bikedata)
#First few entries using head()
head(bikedata)
#First 5 entries using head(,5)
head(bikedata,5)
#Last few entries using tail()
tail(bikedata)
#Last few entries using tail(,5)
tail(bikedata,5)
#Matrix of correlation between fields using pairs()
pairs(bikedata)

#####Pre-processing
bikedata$season <- factor(format(bikedata$season, format="%A"),levels = c("1", "2","3","4") , labels = c("Spring","Summer","Fall","Winter"))
table(bikedata$season)

bikedata$holiday <- factor(format(bikedata$holiday, format="%A"),levels = c("0", "1") , labels = c("Working Day","Holiday"))
table(bikedata$holiday)

bikedata$weathersit <- factor(format(bikedata$weathersit, format="%A"),levels = c("1", "2","3","4") , labels = c("Good:Clear/Sunny","Moderate:Cloudy/Mist","Bad: Rain/Snow/Fog","Worse: Heavy Rain/Snow/Fog"))
table(bikedata$weathersit)

bikedata$yr <- factor(format(bikedata$yr, format="%A"),levels = c("0", "1") , labels = c("2011","2012"))
table(bikedata$yr)

bikedata$actual_temp <- bikedata$temp*41
bikedata$actual_feel_temp <- bikedata$atemp*50
bikedata$actual_windspeed <- bikedata$windspeed*67
bikedata$actual_humidity <- bikedata$hum*100
bikedata$mean_acttemp_feeltemp <- (bikedata$actual_temp+bikedata$actual_feel_temp)/2
str(bikedata)

##Visualizations
#Boxplot
boxplot(bikedata$cnt ~ bikedata$season,
        data = bikedata,
        main = "Total Bike Rentals Vs Season",
        xlab = "Season",
        ylab = "Total Bike Rentals",
        col = c("coral", "coral1", "coral2", "coral3")) 


boxplot(bikedata$cnt ~ bikedata$holiday,
        data = bikedata,
        main = "Total Bike Rentals Vs Holiday/Working Day",
        xlab = "Holiday/Working Day",
        ylab = "Total Bike Rentals",
        col = c("pink", "pink1", "pink2", "pink3")) 

boxplot(bikedata$cnt ~ bikedata$weathersit,
        data = bikedata,
        main = "Total Bike Rentals Vs Weather Situation",
        xlab = "Weather Situation",
        ylab = "Total Bike Rentals",
        col = c("purple", "purple1", "purple2", "purple3")) 

##Histogram
h <- hist(bikedata$cnt, breaks = 25, ylab = 'Frequency of Rental', xlab = 'Total Bike Rental Count', main = 'Distribution of Total Bike Rental Count', col = 'red' )
xfit <- seq(min(bikedata$cnt),max(bikedata$cnt), length = 50)
yfit <- dnorm(xfit, mean =mean(bikedata$cnt),sd=sd(bikedata$cnt))
yfit <- yfit*diff(h$mids[1:2])*length(bikedata$cnt)
lines(xfit,yfit, col='blue', lwd= 3)

h <- hist(bikedata$registered, breaks = 25, ylab = 'Frequency of Rental', xlab = 'Registered Bike Rental Count', main = 'Distribution of Total Bike Rental Count', col = 'yellow' )
xfit <- seq(min(bikedata$cnt),max(bikedata$cnt), length = 50)
yfit <- dnorm(xfit, mean =mean(bikedata$cnt),sd=sd(bikedata$cnt))
yfit <- yfit*diff(h$mids[1:2])*length(bikedata$cnt)
lines(xfit,yfit, col='green', lwd= 3)

#Scatter-Plots
plot(bikedata$dteday, bikedata$cnt,type = "p",
     main = "Total Bike Rentals Vs DateDay",
     xlab = "Year",
     ylab = "Total Bike Rentals",
     col  = "orange",
     pch  = 19)


library(ggplot2)
plot(bikedata$actual_temp, bikedata$cnt ,type = 'h', col= 'red', xlab = 'Actual Temperature', ylab = 'Total Bike Rentals')
plot(bikedata$actual_feel_temp, bikedata$cnt ,type = 'h', col= 'blue', xlab = 'Actual Feel Temperature', ylab = 'Total Bike Rentals')
plot(bikedata$actual_windspeed, bikedata$cnt ,type = 'h', col= 'green3', xlab = 'Actual Windspeed', ylab = 'Total Bike Rentals')
plot(bikedata$actual_humidity, bikedata$cnt ,type = 'h', col= 'yellow', xlab = 'Actual Humidity', ylab = 'Total Bike Rentals')


plot(bikedata$cnt,bikedata$actual_temp, pch=21, bg=c("red","green3","blue","yellow")
     [unclass(bikedata$season)], main="Effect of temperature on bike sharing", xlab="Count", 
     ylab="Actual Temperature")
abline(lm(actual_temp ~ cnt, data=bikedata)$coefficients, col="black")
abline(lm(actual_temp ~ cnt, data=bikedata[which(bikedata$season=="Fall"),])$coefficients, col="blue")
abline(lm(actual_temp ~ cnt, data=bikedata[which(bikedata$season=="Winter"),])$coefficients, col="yellow")
abline(lm(actual_temp ~ cnt, data=bikedata[which(bikedata$season=="Summer"),])$coefficients, col="green3")
abline(lm(actual_temp ~ cnt, data=bikedata[which(bikedata$season=="Spring"),])$coefficients, col="red")


plot(bikedata$cnt,bikedata$actual_windspeed, pch=21, bg=c("red","green3","blue","yellow")
     [unclass(bikedata$season)], main="Effect of windspeed on bike sharing", xlab="Count", 
     ylab="Actual Windspeed")
abline(lm(actual_windspeed ~ cnt, data=bikedata)$coefficients, col="black")
abline(lm(actual_windspeed ~ cnt, data=bikedata[which(bikedata$season=="Fall"),])$coefficients, col="Pink")
abline(lm(actual_windspeed ~ cnt, data=bikedata[which(bikedata$season=="Winter"),])$coefficients, col="Wheat")
abline(lm(actual_windspeed ~ cnt, data=bikedata[which(bikedata$season=="Summer"),])$coefficients, col="Brown")
abline(lm(actual_windspeed ~ cnt, data=bikedata[which(bikedata$season=="Spring"),])$coefficients, col="yellow")

plot(bikedata$cnt,bikedata$actual_humidity, pch=21, bg=c("red","green3","blue","yellow")
     [unclass(bikedata$season)], main="Effect of humditiy on bike sharing", xlab="Count", 
     ylab="Actual Humidity")
abline(lm(actual_windspeed ~ cnt, data=bikedata)$coefficients, col="black")
abline(lm(actual_windspeed ~ cnt, data=bikedata[which(bikedata$season=="Fall"),])$coefficients, col="Pink")
abline(lm(actual_windspeed ~ cnt, data=bikedata[which(bikedata$season=="Winter"),])$coefficients, col="Wheat")
abline(lm(actual_windspeed ~ cnt, data=bikedata[which(bikedata$season=="Summer"),])$coefficients, col="Brown")
abline(lm(actual_windspeed ~ cnt, data=bikedata[which(bikedata$season=="Spring"),])$coefficients, col="yellow")


##Linear Regression Plots
lm_test<- lm(bikedata$cnt~bikedata$actual_temp)
summary(lm_test)
plot(lm_test, col = "green")

##Linear Regression tests (different equations) 
lm_test1<- lm(sqrt(bikedata$cnt)~bikedata$actual_temp+bikedata$actual_humidity+bikedata$actual_windspeed)
lm_test1
summary(lm_test1)

lm_test2<- lm(((bikedata$cnt)^2)~bikedata$actual_temp+bikedata$actual_humidity+bikedata$actual_windspeed)
lm_test2
summary(lm_test2)

lm_test3<- lm((log(bikedata$cnt))~bikedata$actual_temp+bikedata$actual_humidity+bikedata$actual_windspeed)
lm_test3
summary(lm_test3)

