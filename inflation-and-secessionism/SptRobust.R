## Robustness Checks ##

# Outliers
library(car)
testmod1.1 <- lm(v2csantimv ~ inflatavg + debtm1 + popDensity + ctrySize
                 + e_Fiscal_Reliance + v2svinlaut + v2clrgunev + v2clsocgrp,
                 data=dtFull)
influencePlot(testmod1.1)
avPlots(testmod1.1)
outlierTest(testmod1.1)


# Cooks Distance
plot(cooks.distance(testmod1.1), pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooks.distance(testmod1.1), na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooks.distance(testmod1.1))+1, y=cooks.distance(testmod1.1), labels=ifelse(cooks.distance(testmod1.1)>4*mean(cooks.distance(testmod1.1), na.rm=T),names(cooks.distance(testmod1.1)),""), col="red")

# Cases as Outliers: 5448,5449,5450,5451,3467,212,213

# Panel Regression without Outliers and Inflation Correction
x <- subset(dtFull, dtFull$inflatavg < 300)
probdf <-  pdata.frame(x, index = c("country","year"))
pronmod1.1 <- plm(v2csanmvch_9 ~ inflationm1 * popDensity + debtm1 + ctrySize
                  + e_Fiscal_Reliance + v2svinlaut + v2clrgunev,
                  data=probdf, model = "within", effect = "twoways")
summary(pronmod1.1)

# Normality
qqnorm(residuals(ptestmod1.1), ylab = 'Residuals')
qqline(residuals(ptestmod1.1))



# Note: ####
# Make a sample
# dtsample <- dtFull[sample(nrow(dtFull), 2500),]
# sampledf <-  pdata.frame(dtsample, index = c("country","year"))
# psampmod1.1 <- plm(v2csanmvch_9 ~ inflatavg * popDensity + debtm1 + ctrySize
#                   + e_Fiscal_Reliance + v2svinlaut + v2clrgunev,
#                   data=sampledf, model = "within", effect = "twoways")
# psampmod1.2 <- lm(v2csanmvch_9 ~ inflatavg * popDensity + debtm1 + ctrySize
#                    + e_Fiscal_Reliance + v2svinlaut + v2clrgunev,
#                    data=dtsample)
# summary(psampmod1.1)
# summary(psampmod1.2)