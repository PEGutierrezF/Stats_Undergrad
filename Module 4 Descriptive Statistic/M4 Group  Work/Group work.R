



# --------------------------------------------------------
# Group work
# Date: Wed Feb 05 2025 20:54:31
# Pablo E. Gutierrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# --------------------------------------------------------


library(tidyverse)
library(lessR)
library(readxl)

snowdata <- read_excel("MM Snow Depth.xlsx")
head(snowdata)

#this pivot function is from lessR
pivot(snowdata, c(mean, min,max,var,sd,skew,kurtosis,IQR,quantile), 
      Mean_Dec_May_snow_depth)


?pivot

#this quantile function is from lessR
quartiles <- quantile(snowdata$Mean_Dec_May_snow_depth, probs = c(0.25,0.5,0.75))

#these ggplot functions are from the tidyverse
ggplot(snowdata, aes(x = "", y = Mean_Dec_May_snow_depth)) + 
  geom_boxplot() + 
  stat_summary(fun.y = mean, geom = "point", 
               shape = 18, size = 3, color = "red") +
  geom_jitter(position=position_jitter(0.2)) +
  theme_bw()



# Quantiles ---------------------------------------------------------------


# Calculate quartiles and store them in a data frame
quartiles <- data.frame(
  y = quantile(snowdata$Mean_Dec_May_snow_depth, probs = c(0.25, 0.5, 0.75)),
  label = c("Q1", "Median", "Q3")
)

ggplot(snowdata, aes(x = "", y = Mean_Dec_May_snow_depth)) + 
  geom_boxplot() + 
  stat_summary(fun.y = mean, geom = "point", 
               shape = 18, size = 3, color = "red") +
  geom_jitter(position = position_jitter(0.2), alpha = 0.5) + 
  geom_text(
    data = quartiles,
    aes(x = "", y = y, label = label),
    nudge_x = 0.2, hjust = 0
  ) +
  theme_bw()




#these ggplot functions are from the tidyverse
ggplot(snowdata, aes(x=Year , y=Mean_Dec_May_snow_depth)) + geom_point()

ggplot(snowdata, aes(x=Mean_Dec_May_snow_depth)) + geom_histogram()

shapiro.test(snowdata$Mean_Dec_May_snow_depth)


snowdata$Mean_Dec_May_snow_depth[snowdata$Mean_Dec_May_snow_depth < 14]
