#### Regression Group Work ####
#### NR2990 ####

library(tidyverse)
library(readxl)
library(car)


#Load data
BAI_NDWI <- read_xlsx("PIRU_BAI.xlsx")


#This makes a plot with our regression line.
ggplot(XXX) +
  aes(x = BAI, y = min_NDWI) +
  geom_point() +
  theme(legend.position = "none") + 
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") 


#this simply creates a 'model' for our regression model
#This is the main think we are looking at!
model <- lm(XXX ~ XXX, data = XXX)

summary(XXX)

##the Breusch-Pagan test to assess homoscedasticity
ncvTest(XXX)


#get list of residuals to nest normality
res <- resid(XXX)
shapiro.test(XXX)

#create residual plot
ggplot(XXX, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0)


#Visualizing residuals
BAI_NDWI <-
  BAI_NDWI |> 
  mutate(yhat = fitted(model),
         res_sqrt = sqrt(abs(rstandard(model))))
#Plotting the residuals
ggplot(BAI_NDWI, aes(yhat, res_sqrt)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") 


#A histogram of the residuals
ggplot(model, aes(model$residuals)) + geom_density()
shapiro.test(model$residuals)


#Cooks D
ggplot(as_tibble(cooks.distance(model)), aes(value)) + geom_boxplot()
plot(model,which=4)


#Jackknifing
jack.reg<-numeric(27)
for (i in 1:27) {
  model<-lm(BAI[-i]~min_NDWI[-i], data = BAI_NDWI)
  jack.reg[i]<-summary(model)$r.squared }

ggplot(as.tibble(jack.reg), aes(value)) + geom_histogram()

  
shapiro.test(jack.reg)


