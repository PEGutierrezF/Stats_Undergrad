



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
pivot(snowdata, c(mean, min,max,var,sd,skew,kurtosis,IQR,quantile), Mean_Dec_May_snow_depth)
?pivot


#these ggplot functions are from the tidyverse
ggplot(snowdata, aes(x=Year , y=Mean_Dec_May_snow_depth)) + geom_point()

ggplot(snowdata, aes(x=Mean_Dec_May_snow_depth)) + geom_histogram()

shapiro.test(snowdata$Mean_Dec_May_snow_depth)
