getwd()
setwd("C:/Users/Srikanth B V/Desktop/Data Science Assignments/Data Sets")

Comp <- read.csv("Computer_Data.csv")
View(Comp)

colnames(Comp)

# Remove X as it is serial number
comp_a <- subset(Comp, select = c(price, speed, hd,  ram,  screen,  cd,  multi,  premium,  ads,  trend))
str(comp_a)
attach(comp_a)

par(mfrow = c(3,5))
hist(price)
hist(speed)
hist(hd)
hist(ram)
hist(screen)
hist(cd)
hist(multi)
hist(premium)
hist(ads)
hist(trend)



# Convert factors to nunmeric
comp_a$cd  <- as.numeric(comp_a$cd) # no = 1, yes = 2
comp_a$multi  <- as.numeric(comp_a$multi) # no = 1, yes = 2
comp_a$premium  <- as.numeric(comp_a$premium) # no = 1, yes = 2

boxplot(comp_a$speed, horizontal = T) # No outliers
boxplot(comp_a$hd, horizontal = T) # Outliers exists
boxplot(comp_a$ram, horizontal = T) # Outliers exists
boxplot(comp_a$screen, horizontal = T) # Outliers exists

cor(comp_a)

model1_comp <- lm(price~., data = comp_a) # Variable X is serial number and hence removed.
summary(model1_comp)

aggregate(price~trend, data = comp_a, mean)

library(car)
vif(model1_comp) # All the values are <10. No colinearity problem
avPlots(model1_comp) # All variables have atleast effect on output.

pairs(comp_a)

library(car)
# Check for influentials
influence.measures(model1_comp)
influenceIndexPlot(model1_comp)
influencePlot(model1_comp)


#applying log transformation
model3_comp <- lm(price~log(speed)+log(hd)+ram+screen+cd+multi+premium+ads+trend, data = comp_a[-c(1441, 1701, 3784, 4478)])
summary(model3_comp)

predict(model3_comp, interval="predict")

plot(model3_comp)
# Residual plots = Line is horizontally straight then the errors are constant and not fluctuating.
# QQ Plot = plotting the errors and are normally distributed.
## Standardised erros plot and not fluctuating.
# Residual vs leverage plot. Any data falls beyond 0.5 is influential


#applying exponential transformation
model4_comp <- lm(log(price)~log(speed)+log(hd)+ram+screen+cd+multi+premium+ads+trend, data = comp_a[-c(1441, 1701, 3784, 4478)])
summary(model4_comp)

predict(model4_comp)

#convert exp values to normal
pr <- exp(predict(model4_comp))
pr

par(mfrow = c(1,1))
plot(model4_comp)
# Residual plots = Line is horizontally straight then the errors are constant and not fluctuating.
# QQ Plot = plotting the errors and are normally distributed.
## Standardised erros plot and not fluctuating.
# Residual vs leverage plot. Any data falls beyond 0.5 is influential

sum(model4_comp$residuals) 
# 1.334798e-13 which is approx 0.


exp(predict(model4_comp, data.frame("speed"= 25,"hd"= 80,"ram"= 4,"screen"= 14,"cd"= 1,"multi"= 1,"premium"= 2,"ads"= 94,"trend"= 1)))
# 1721.445 