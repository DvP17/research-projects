## Models and Statistics ##

## TESTING ####
testdf <- dtfull

# Show best Model (Kabacoff 205 f.)
library(leaps)
testdfleaps <- testdf[c("cname","entryforceyear","agreementsigned",
                        "depthacc", "polity","dunderval1", "dunderval2",
                        "dunderval3", "lvau_garriga", "nettrade", "gdp",
                        "chpratesplus1", "chpratesplus2","chpratesplus3",
                        "inflation", "debtext")]
testdfleaps <- regsubsets(dunderval1 ~ agreementsigned 
                          + depthacc*lvau_garriga
                          + polity + lvau_garriga + nettrade
                          + gdp + inflation + debtext, data = testdfleaps)
subsets(testdfleaps, statistic = "cp")
abline(1,1,lty=2, col="red") 


## INDUCTION PLOTTING ####
#boxplot(testdf$chreerplus1)
boxplot(testdf$depthacc)
boxplot(testdf$nettrade)

boxplot(chreerplus1 ~ agreements, data = testdf)
abline(h = 0, col = "green")

boxplot(chpratesplus1 ~ agreementsigned, data = testdf)

testdf <- subset(testdf, testdf$depthacc <= 50)
plot(intrate ~ depthacc, data = testdf)

library(ggplot2)
testdf <- dtfull
plot(intrate ~ depthacc, col = as.numeric(cut_number(testdf$nettrade,3)),
     data = testdf, pch = 5)
identify(testdf$depthacc, testdf$intrate)
abline(lm(testdf$intrate ~ testdf$depthacc), col="red")

testdf$nettradeCAT <- as.numeric(cut_number(testdf$nettrade,3))

plot(reer ~ depthacc, col = as.numeric(cut_number(testdf$nettrade,3)),
     data = testdf[testdf$depthacc < 20 & testdf$reer < 250,], 
     pch = 5, las=1)
plot(reer ~ depthsum, col = as.numeric(cut_number(testdf$nettrade,3)),
     data = testdf, pch = 5, las=1)
identify(testdf$depthacc, testdf$reer)
abline(h=100)
abline(h=mean(testdf[testdf$nettradeCAT == 1,]$reer, na.rm = T),
       col= "green")
axis(2, at=100,labels=100, las=1)
# Green is positive, red is neutral, black is negative


boxplot(chreerplus3 ~ depthacc, data = testdf)

plot(reer ~ depthacc, data = testdf)
abline(lm(testdf$reer ~ testdf$depthacc), col="red")


MeansByNum <- tapply(testdf$chreerplus1, testdf$depthacc, mean) #Create an array of means by Num
NewDF <- data.frame(cbind(Num = as.numeric(as.vector(names(MeansByNum))),
                          Percent = as.numeric(MeansByNum)))
plot(testdf$depthacc~testdf$chreerplus1)
points(testdf$chreerplus1, testdf$depthacc, col="red")

#Interplot
library(interplot)
interplot()

## 1 UNDERVAL ####
# Prepare
library(plm) #plmtest(between) or phtest(random, fixed) is recommended
negsqrt <- function(x){sign(x)*sqrt(abs(x))}
neglog <- function(x){sign(x)*log(abs(x)+1)}
testdf <- dtfull
ptestdf <-  pdata.frame(testdf, index = c("cname","entryforceyear"))

testdf$nettrade <- negsqrt(testdf$nettrade)
testdf$depthacc <- log1p(testdf$depthacc)

# Eliminate Outliers
# outrange <- function(x,down,up){x > up | x < down}
# x <- boxplot(testdf$dunderval1)$stats[c(1,5)]
# testdf <- subset(testdf, !outrange(testdf$dunderval1,-0.4,0.4))

# Test-Model
testdf1 <- dtfull
ptestdf1 <-  pdata.frame(testdf1, index = c("cname","entryforceyear"))
ptestmod <- plm(negsqrt(dunderval1) ~ log1p(depthacc)*lvau_garriga 
                 + log1p(depthacc)*debtgdp 
                 + inflation + log(gdp) + polity + crisis 
                 + agreementsigned,
                 data=ptestdf1, model = "within", effect = "twoways")
summary(ptestmod)

# t+x
modunval1 <- plm(undervalp1 ~ log1p(depthacc)*negsqrt(nettrade)
                 + log1p(depthacc)*lvau_garriga + log1p(depthacc)*debtgdp 
                 + inflation + log(gdp) + polity + crisis,
                 data=ptestdf, model = "within", effect = "twoways")
summary(modunval1)

# d DV
modunval2 <- plm(negsqrt(dunderval1) ~ log1p(depthacc)*negsqrt(nettrade)
                 #+ log1p(depthacc)*
                 + lvau_garriga 
                 #+ log1p(depthacc)*debtgdp 
                 + inflation 
                 #+ log(gdp) 
                 + polity + crisis,
                 data=ptestdf, model = "within", effect = "twoways")
summary(modunval2)

modunval3 <- plm(negsqrt(dunderval2) ~ log1p(depthacc)*negsqrt(nettrade)
                 + log1p(depthacc)*lvau_garriga 
                 #+ log1p(depthacc)*
                 #+ debtgdp 
                 + inflation + log(gdp) + polity + crisis,
                 data=ptestdf, model = "within", effect = "twoways")
summary(modunval3)

modunval4 <- plm(negsqrt(dunderval3) ~ log1p(depthacc)*negsqrt(nettrade)
                 + log1p(depthacc)*lvau_garriga 
                 + log1p(depthacc)*debtgdp 
                 + inflation + log(gdp) + polity + crisis 
                 + agreementsigned,
                 data=ptestdf, model = "within", effect = "twoways")
summary(modunval4)

# REER
modunval5 <- plm(negsqrt(chreerplus3) ~ log1p(depthacc)*negsqrt(nettrade)
                 + log1p(depthacc)*lvau_garriga + log1p(depthacc)*debtgdp 
                 + inflation + log(gdp) + polity + crisis
                 + agreementsigned,
                 data=ptestdf, model = "within", effect = "twoways")
summary(modunval5)

# # t+x
# fixedtest <- plm(reerp2 ~ log1p(depthacc)*nettrade
#                  + log1p(depthacc)*lvau_garriga + log1p(depthacc)*debtgdp 
#                  + inflation + log(gdp) + polity + crisis,
#                  data=ptestdf, model = "within")
# summary(fixedtest)

## 2 ROBUSTNESS ####
# Robustness Modell
testdf <- subset(dtfull, dtfull$underval > 1
                  #& dtfull$nettrade > 0
)
ptestdf <-  pdata.frame(testdf, index = c("cname","entryforceyear"))
modrobust1 <- plm(negsqrt(dunderval3) ~ log1p(depthacc)*lvau_garriga 
                  + log1p(depthacc)*debtgdp 
                  + inflation + log(gdp) + polity + crisis 
                  + agreementsigned,
                  data=ptestdf, model = "within", effect = "twoways")
summary(modrobust1)


testdf <- subset(dtfull, dtfull$underval > 1
                  & dtfull$nettrade > 0
)
ptestdf <-  pdata.frame(testdf, index = c("cname","entryforceyear"))
modrobust2 <- plm(negsqrt(dunderval3) ~ log1p(depthacc)*lvau_garriga 
                  + log1p(depthacc)*debtgdp 
                  + inflation + log(gdp) + polity + crisis 
                  + agreementsigned,
                  data=ptestdf, model = "within", effect = "twoways")
summary(modrobust2)


testdf <- subset(dtfull, dtfull$underval < 1
                  #& dtfull$nettrade > 0
)
ptestdf <-  pdata.frame(testdf, index = c("cname","entryforceyear"))
modrobust3 <- plm(negsqrt(dunderval3) ~ log1p(depthacc)*lvau_garriga 
                  + log1p(depthacc)*debtgdp 
                  + inflation + log(gdp) + polity + crisis 
                  + agreementsigned,
                  data=ptestdf, model = "within", effect = "twoways")
summary(modrobust3)


testdf <- subset(dtfull, dtfull$underval < 1
                  & dtfull$nettrade < 0
)
ptestdf <-  pdata.frame(testdf, index = c("cname","entryforceyear"))
modrobust4 <- plm(negsqrt(dunderval3) ~ log1p(depthacc)*lvau_garriga 
                  + log1p(depthacc)*debtgdp 
                  + inflation + log(gdp) + polity + crisis 
                  + agreementsigned,
                  data=ptestdf, model = "within", effect = "twoways")
summary(modrobust4)


## 3 INTEREST RATE MODELS ####
# Prepare
library(plm) #plmtest(between) or phtest(random, fixed) is recommended
negsqrt <- function(x){sign(x)*sqrt(abs(x))}
neglog <- function(x){sign(x)*log(abs(x)+1)}
testdf <- dtfull
  #subset(dtfull, dtfull$depthacc != 0)
testdf$nettrade <- negsqrt(testdf$nettrade)
testdf$depthacc <- loglp(testdf$depthacc)
  #log(testdf$depthacc+1)
  #sqrt(testdf$depthacc)
#testdf$chpratesplus1 <- neglog(testdf$chpratesplus1)
#testdf$chpratesplus2 <- neglog(testdf$chpratesplus2)
#testdf$chpratesplus3 <- neglog(testdf$chpratesplus3)

ptestdf <-  pdata.frame(testdf, index = c("cname","entryforceyear"))

# Test Model
library(plm)
testdf1 <- dtfull
ptestdf1 <-  pdata.frame(testdf1, index = c("cname","entryforceyear"))
ptestmod <- plm(negsqrt(dprat3) ~ log1p(depthacc)*lvau_garriga 
                + log1p(depthacc)*debtgdp 
                + log1p(depthacc)*negsqrt(nettrade)
                + inflation + log(gdp) + polity + crisis 
                + agreementsigned,
                data=ptestdf1, model = "within", effect = "twoways")
summary(ptestmod)

# T + 1 Models
modprat1 <- plm(pratp1 ~ log1p(depthacc)*lvau_garriga + 
                + log1p(depthacc)*debtgdp 
                + log1p(depthacc)*negsqrt(nettrade)
                + inflation + log(gdp) + polity + crisis 
                + agreementsigned,
             data=ptestdf, model = "within", effect = "twoways")
summary(modprat1)

modprat2 <- plm(pratp2 ~ log1p(depthacc)*lvau_garriga + 
                  + log1p(depthacc)*debtgdp 
                + log1p(depthacc)*negsqrt(nettrade)
                + inflation + log(gdp) + polity + crisis 
                + agreementsigned,
              data=ptestdf, model = "within", effect = "twoways")
summary(modprat2)

modprat3 <- plm(pratp3 ~ log1p(depthacc)*lvau_garriga + 
                + log1p(depthacc)*debtgdp 
                + log1p(depthacc)*negsqrt(nettrade)
                + inflation + log(gdp) + polity + crisis 
                + agreementsigned,
                data=ptestdf, model = "within", effect = "twoways")
summary(modprat3)

# Change Models
modprat4 <- plm(negsqrt(dprat1) ~ log1p(depthacc)*lvau_garriga 
                + log1p(depthacc)*debtgdp 
                + log1p(depthacc)*negsqrt(nettrade)
                + inflation + log(gdp) + polity + crisis 
                + agreementsigned,
                data=ptestdf, model = "within", effect = "twoways")
summary(modprat4)

modprat5 <- plm(negsqrt(dprat2) ~ log1p(depthacc)*lvau_garriga 
                + log1p(depthacc)*debtgdp 
                + log1p(depthacc)*negsqrt(nettrade)
                + inflation + log(gdp) + polity + crisis 
                + agreementsigned,
                data=ptestdf, model = "within", effect = "twoways")
summary(modprat5)

modprat6 <- plm(negsqrt(dprat3) ~ log1p(depthacc)*lvau_garriga 
                + log1p(depthacc)*debtgdp 
                + log1p(depthacc)*negsqrt(nettrade)
                + inflation + log(gdp) + polity + crisis 
                + agreementsigned,
                data=ptestdf, model = "within", effect = "twoways")
summary(modprat6)

## SAVE MODELS ####
save(list = c("modunval1","modunval2","modunval3","modunval4","modunval5",
              "modprat1","modprat2","modprat3","modprat4",
              "modprat5","modprat6",
              "modrobust1","modrobust2","modrobust3","modrobust4"),
              file = "models.RData")

## Hausman and F Test ####
testdf1 <- dtfull
ptestdf1 <-  pdata.frame(testdf1, index = c("cname","entryforceyear"))
fixed <- plm(negsqrt(dunderval3) ~ log1p(depthacc)*negsqrt(nettrade)
                 + log1p(depthacc)*lvau_garriga 
                 + log1p(depthacc)*debtgdp 
                 + inflation + log(gdp) + polity + crisis 
                 + agreementsigned,
                 data=ptestdf1, model = "within", effect = "twoways")
pooled <- plm(negsqrt(dunderval3) ~ log1p(depthacc)*negsqrt(nettrade)
                + log1p(depthacc)*lvau_garriga 
                + log1p(depthacc)*debtgdp 
                + inflation + log(gdp) + polity + crisis 
                + agreementsigned,
                data=ptestdf1, model = "pooling")
random <- plm(negsqrt(dunderval3) ~ log1p(depthacc)*negsqrt(nettrade)
                 + log1p(depthacc)*lvau_garriga 
                 + log1p(depthacc)*debtgdp 
                 + inflation + log(gdp) + polity + crisis 
                 + agreementsigned,
                 data=ptestdf1, model = "random")
pFtest(fixed, pooled)
phtest(random, fixed)
rm(fixed, pooled, random)

## Other (Appendix) ####
for (i in 1:nrow(dtfull)){
  if (isTRUE(dtfull$cname[i] == dtfull$cname[i+1])){
    dtfull$increasep1[i] <- dtfull$increase[i+1]
  }
}

for (i in 1:nrow(dtfull)){
  if (isTRUE(dtfull$cname[i] == dtfull$cname[i+1])){
    dtfull$decreasep1[i] <- dtfull$decrease[i+1]
  }
}

testdf <- dtfull
ptestdf <-  pdata.frame(testdf, index = c("cname","entryforceyear"))

modcbi1 <- plm(increase ~ log1p(depthacc)*debtgdp 
                + log1p(depthacc)*negsqrt(nettrade)
                + inflation + log(gdp) + polity + crisis 
                + agreementsigned,
                data=ptestdf, model = "within", effect = "twoways")
summary(modcbi1)

modcbi2 <- plm(increasep1 ~ log1p(depthacc)*debtgdp 
               + log1p(depthacc)*negsqrt(nettrade)
               + inflation + log(gdp) + polity + crisis 
               + agreementsigned,
               data=ptestdf, model = "within", effect = "twoways")
summary(modcbi2)

modcbi3 <- plm(decrease ~ log1p(depthacc)*debtgdp 
               + log1p(depthacc)*negsqrt(nettrade)
               + inflation + log(gdp) + polity + crisis 
               + agreementsigned,
               data=ptestdf, model = "within", effect = "twoways")
summary(modcbi2)

modcbi4 <- plm(decreasep1 ~ log1p(depthacc)*debtgdp 
               + log1p(depthacc)*negsqrt(nettrade)
               + inflation + log(gdp) + polity + crisis 
               + agreementsigned,
               data=ptestdf, model = "within", effect = "twoways")
summary(modcbi2)

stargazer::stargazer(modcbi1,modcbi2,modcbi3,modcbi4,
                     title = "Effects on CBI Increase", 
                     dep.var.labels = c("Increase","Increase+1",
                                        "Decrease","Decrease+1"),
                     type = "latex",
                     keep.stat = c("rsq","n")
)


moddepth1 <- plm(negsqrt(dunderval1) ~ log1p(depthacc)*negsqrt(nettrade)
                 + log1p(depthacc)*lvau_garriga 
                 + log1p(depthacc)*debtgdp 
                 + inflation + log(gdp) + polity + crisis 
                 + agreementsigned
                 + standards
                 + investments
                 + services
                 + procurement
                 + competition
                 + iprs,
                 data=ptestdf, model = "within", effect = "twoways")
summary(moddepth1)

moddepth2 <- plm(negsqrt(dunderval2) ~ log1p(depthacc)*negsqrt(nettrade)
                 + log1p(depthacc)*lvau_garriga 
                 + log1p(depthacc)*debtgdp 
                 + inflation + log(gdp) + polity + crisis 
                 + agreementsigned
                 + standards
                 + investments
                 + services
                 + procurement
                 + competition
                 + iprs,
                 data=ptestdf, model = "within", effect = "twoways")
summary(moddepth2)

moddepth3 <- plm(negsqrt(dprat1) ~ log1p(depthacc)*negsqrt(nettrade)
                 + log1p(depthacc)*lvau_garriga 
                 + log1p(depthacc)*debtgdp 
                 + inflation + log(gdp) + polity + crisis 
                 + agreementsigned
                 + standards
                 + investments
                 + services
                 + procurement
                 + competition
                 + iprs,
                 data=ptestdf, model = "within", effect = "twoways")
summary(moddepth3)

moddepth4 <- plm(negsqrt(dprat2) ~ log1p(depthacc)*negsqrt(nettrade)
                 + log1p(depthacc)*lvau_garriga 
                 + log1p(depthacc)*debtgdp 
                 + inflation + log(gdp) + polity + crisis 
                 + agreementsigned
                 + standards
                 + investments
                 + services
                 + procurement
                 + competition
                 + iprs,
                 data=ptestdf, model = "within", effect = "twoways")
summary(moddepth4)

stargazer::stargazer(moddepth1,moddepth2,moddepth3,moddepth4,
                     title = "Effects of Different Depth Variables", 
                     dep.var.labels = c("dunval1","dunval2",
                                        "dprat1","dprat1"),
                     type = "latex",
                     keep.stat = c("rsq","n")
)

## NOTES ####
# Notes: (PTA dichotom - ANOVA?)
#Numeric to catergoric
cut(testdf$reer, breaks = c(seq(5,255,10)), labels = c(seq(10,250,10)))

#3d Plots
library(rgl)
testdf <- subset(dtfull, chreerplus1 < 50 & chreerplus1 > -50 &
                   nettrade < 3209590992 & nettrade > -4374789039)
plot3d(testdf$chreerplus1, testdf$depthacc, testdf$nettrade)

library(car)
scatter3d(testdf$chreerplus1, testdf$nettrade, testdf$depthacc, 
          fit = "smooth")$plane3d(6,0,0)

# Videos
# PLM: https://www.youtube.com/watch?v=1pST2lUx6QM
# PLM Explanation: https://www.youtube.com/watch?v=f01WjeCdgEA