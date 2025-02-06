
#### NR2400 ####
#### Group Work 7 ####
library(readr)
library(e1071)
library(tidyverse)
library(lessR)
library(BSDA)
nests <- read_csv("Laloë_et_al._2020_Ecol_Evol_dataset.csv")
pivot(nests, c(mean, sd), nestTemperature)

ggplot(nests, aes(x = nestTemperature)) + geom_density()

yourNests <- read_xlsx("yourTurtleData.xlsx")
pivot(yourNests, c(mean, sd), nestTemperature)

z.test(x=yourNests$nestTemperature,
       mu=31.2,
       alternative = "two.sided",
       sigma.x = 1.822)

