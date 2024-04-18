#### Regression ####
#### NR2990 ####


#### Regression ####

#this simply creates a 'model' for our regression model
penguins <- na.omit(penguins)
model <- lm(body_mass_g ~ flipper_length_mm, data = penguins)
summary(model)
#this is the r-square value, "pulled out" of the model
summary(model)$r.squared

#This makes a plot with our regression line.
ggplot(penguins) +
  aes(x = body_mass_g, y = flipper_length_mm) +
  geom_point() +
  theme(legend.position = "none") + 
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") 

#the Breusch-Pagan test to assess homoscedasticity
library(car)
ncvTest(model)

#Visualizing residuals
penguins <- na.omit(penguins)
penguins <-
  penguins |> 
  mutate(yhat = fitted(model),
         res_sqrt = sqrt(abs(rstandard(model))))
ggplot(penguins, aes(yhat, res_sqrt)) +
  geom_point() +
  geom_smooth()

#A histogram of the residuals
ggplot(penguins, aes(res_sqrt)) + geom_density()
shapiro.test(penguins$res_sqrt)


#Cooks D
ggplot(as_tibble(cooks.distance(model)), aes(value)) + geom_boxplot()
plot(model,which=4)
# Calculate Cook's distance
cooksd <- cooks.distance(model)

# Identify outliers based on Cook's distance
# Replace 'df' with the name of your data.
outliers <- cooksd > 4/nrow(df)  # Using a threshold of 4/n

# Print outlier years
# Replace 'df' with the name of your data.
# Replace 'column' with the name of the column where the outlier is.
print(df$column[outliers]) 


#Remove potential ouliers
new_df <- df %>%
  filter(column != outlier) # Replace 'outlier' with the printed outlier above.


#PRESS_RMSE
press <- resid(model)/(1 - lm.influence(model)$hat)
PRESS <- sum(press^2)
PRESS
# PRESS_RMSE
PRESS_RMSE <- sqrt(PRESS/length(model$residuals))
PRESS_RMSE


#RMSE( Residual sum of squares)
sqrt(sum(model$residuals^2)/model$df) 