
library(readxl)
library(ggplot2)
library(RVAideMemoire)
hurricanes <- read_xlsx("hurricanes.xlsx")

#Visualize your distribution

ggplot(hurricanes, aes(x = wind, fill = Pattern)) + geom_density(alpha =0.7)

#Test for normality and other assumptions (equal variance) (mental check for paired or not)

#Shapiro-Wilks

byf.shapiro(wind~Pattern, data=hurricanes)
#OR
shapiro.test((filter(hurricanes, Pattern=="El_nino")$wind))
shapiro.test((filter(hurricanes, Pattern=="la_nina")$wind))

#var.test
var.test(wind~Pattern, data=hurricanes)

#Non-normal data, so Wilcoxon
wilcox.test(wind~Pattern, data=hurricanes,
            alternative = c("two.sided"),
            mu = 0, 
            paired = FALSE, 
            var.equal = TRUE,
            conf.level = 0.95)

cohens_d(wind~Pattern, data=hurricanes)