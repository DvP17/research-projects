## Data Management ##

#### 1. Loading #########################################################
# World Bank
library(WDI)
WDIsearch('inflation') #for example
dtInfl = WDI(indicator='FP.CPI.TOTL.ZG', country="all", start=1960, end=2017)
dtDebt = WDI(indicator='DT.DOD.DECT.CD', country="all", start=1960, end=2017)
dtSize = WDI(indicator='AG.SRF.TOTL.K2', country="all", start=1960, end=2017)
dtDens = WDI(indicator='EN.POP.DNST', country="all", start=1960, end=2017)

# V-Dem ###
# Find Variables:
# Antisystem Movement - v2csantimv; State Authority - v2svstterr;
# International Autonomy - v2svinlaut; National Civil Lib - v2clrgunev;
# Ethnic Civil Exclusion - v2clsocgrp; Ethnic Power Exclusion - v2pepwrsoc;
# Gov. Revenues Resources - e_Fiscal_Reliance; Civil War - e_Civil_War;
# Armed Conflict internal - e_miinterc; Separatist movement - v2csanmvch_9;
title.line <- readLines("C:/Users/Nutzer/Documents/Studium Salzburg/6 SS 18/SE Methods II/3 V-Dem/V-Dem-CY+Others-v8.csv", n=1)
title.line <- unlist(strsplit(title.line, '\",\"'))
which(title.line == "v2csanmvch_9") #for example
rm(title.line)

# Load Data
library(data.table)
dtvdem <- fread("C:/Users/Nutzer/Documents/Studium Salzburg/6 SS 18/SE Methods II/3 V-Dem/V-Dem-CY+Others-v8.csv",
             select = c("country_name","year","v2csantimv","v2svstterr",
                        "v2svinlaut","v2clrgunev","e_Fiscal_Reliance",
                        "e_Civil_War","e_miinterc","v2csanmvch_9",
                        "v2clsocgrp","v2pepwrsoc"))
  
# WVS (Attention: 1.9 GB)
load("C:/Users/Nutzer/Documents/Studium Salzburg/6 SS 18/SE Methods II/4 WVS/WVS_Longitudinal_1981_2014_R_v2015_04_18.rdata")
dtwvs1 = WVS_Longitudinal_1981_2014_R_v2015_04_18

library(countrycode)
dtwvs1[[4]] <- countrycode(dtwvs1[[4]],'wvs','country.name') 
dtwvs1[[38]] <- sub('.*(?=.{4}$)', '', dtwvs1[[38]], perl=T)
#x <- dtwvs1[1:5000,1:40]
#dtwvs1[order(dtwvs1$S003, dtwvs1$S025), ]

#Variables:
# Trust: A165, D001_B
# National: G006    which(colnames(dtwvs1)=="G006")
x = c(which(colnames(dtwvs1)=="S003"), which(colnames(dtwvs1)=="S025"),
      which(colnames(dtwvs1)=="G006"),
      which(colnames(dtwvs1)=="A165"), which(colnames(dtwvs1)=="D001_B"),
      which(colnames(dtwvs1)=="G019"))

dtwvs <- dtwvs1[x]

dtwvs2 <- aggregate(dtwvs[c('G006','A165','G019')],
                    by=list(dtwvs$S003,dtwvs$S025),FUN=mean)
dtwvs2 <- dtwvs2[order(dtwvs2$Group.1, dtwvs2$Group.2), ]
row.names(dtwvs2) <- NULL


#2. Comparing/Munching ##################################################
colnames(dtvdem)[1] <- "country"
colnames(dtwvs2)[c(1,2)] <- c("country","year")
dtwvs2$year <- as.numeric(dtwvs2$year)


#3. Merging #############################################################
colnames(dtInfl)[[3]] <- "inflation"
colnames(dtDebt)[[3]] <- "debt"
dtDebt$debt <- dtDebt$debt/1000000000
colnames(dtSize)[[3]] <- "ctrySize"
dtSize$ctrySize <- sqrt(dtSize$ctrySize)
colnames(dtDens)[[3]] <- "popDensity"

# inflation t-1,2,3
for (i in 1:(nrow(dtInfl)-1)) {
  for (j in 1:3){
    if (isTRUE(dtInfl$country[i] == dtInfl$country[i+j])){
      dtInfl[i,sprintf("inflationm%s",j)] <- dtInfl$inflation[i+j]
    }
  }
}
# dtInfl <- dtInfl[order(dtInfl$country, dtInfl$year),] #invert order
dtInfl$inflatavg <- rowMeans(dtInfl[5:7], na.rm = TRUE)

# debt t-1,2,3
for (i in 1:(nrow(dtDebt)-1)) {
  for (j in 1:3){
    if (isTRUE(dtDebt$country[i] == dtDebt$country[i+j])){
      dtDebt[i,sprintf("debtm%s",j)] <- dtDebt$debt[i+j]
    }
  }
}
dtDebt$debtavg <- rowMeans(dtDebt[5:7], na.rm = TRUE)


x <- merge(dtvdem,dtInfl[2:8])
x <- merge(x,dtDebt[2:8])
x <- merge(x,dtSize[2:4])
dtFull <- merge(x,dtDens[2:4])
dtFullwvs <- merge(dtFull,dtwvs2)


rm(dtInfl,dtDebt,dtSize,dtDens)

#5. Clean Data Set Version ##############################################
#WVS change variable names
colnames(dtFullwvs)[25:27] <- c("national","trust1","trust2")

#WVS clean of "-4" Values
dtFullwvs$trust1 <- ifelse(dtFullwvs$trust1 == -4, NA,
                           dtFullwvs$trust1)
dtFullwvs$trust2 <- ifelse(dtFullwvs$trust2 == -4, NA,
                           dtFullwvs$trust2)
dtFullwvs$national <- ifelse(dtFullwvs$national == -4, NA,
                             dtFullwvs$national)


#5. Write Final Data Set and Codebook ###################################
write.csv(dtFull, "dtFull.csv")
write.csv(dtFullwvs, "dtFullwvs.csv")