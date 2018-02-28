## Building the Data Set ##

#################################### LOAD ###################################
# 1. Load Data
## DESTA
dtptadyad <- read.csv("DatPtaDyadic.csv", header = T)

dtptadepth <- read.csv("DatPtaDepth.csv", header = T)

## IMF
dtprates <- read.csv("DatInterRates.csv", skip = 6, header = T, 
                  na.strings = c("...", "-"),
                  dec = ",", sep = ";")

## Polity
dtpolity <- read.csv("DatPolity.csv", header = T,
                     colClasses=c(rep('NULL', 3), rep(NA, 2),
                                  rep('NULL', 5), NA, rep('NULL', 25)),
                     check.names = F, sep = ";")

##Garriga
library(haven)
dtcbi <- read_dta("~/Studium Salzburg/5 WS 17/BA Arbeit/Quant Analys/Daten/CBI dataset full.dta")


## World Bank
dtreer <- read.csv("DatREER.csv", skip = 4, header = T, 
                     colClasses=c(NA, rep('NULL', 33), rep(NA, 27),
                                  rep('NULL', 2)),
                     check.names = F)

dtinflat <- read.csv("DatInflation.csv", skip = 4, header = T, 
                  colClasses=c(NA, rep('NULL', 33), rep(NA, 27),
                               rep('NULL', 2)),
                  check.names = F)

dtdebt <- read.csv("DatDebt.csv", skip = 4, header = T, 
                 colClasses=c(NA, rep('NULL', 33), rep(NA, 27),
                              rep('NULL', 2)),
                 check.names = F)

dtdebtext <- read.csv("DatDebtExt.csv", skip = 4, header = T, 
                 colClasses=c(NA, rep('NULL', 33), rep(NA, 27),
                              rep('NULL', 2)),
                 check.names = F)

dtbop <- read.csv("DatBoP.csv", skip = 4, header = T, 
                colClasses=c(NA, rep('NULL', 33), rep(NA, 27),
                             rep('NULL', 2)),
                check.names = F)

dtgdp <- read.csv("DatGDP.csv", skip = 4, header = T, 
                colClasses=c(NA, rep('NULL', 33), rep(NA, 27),
                             rep('NULL', 2)),
                check.names = F)

dtgdpcapreal <- read.csv("DatGDPcapReal.csv", skip = 4, header = T, 
                         colClasses=c(NA, rep('NULL', 33), rep(NA, 27),
                                    rep('NULL', 2)),
                         check.names = F)

dtgdpcapgr <- read.csv("DatGDPcapGr.csv", skip = 4, header = T, 
                  colClasses=c(NA, rep('NULL', 33), rep(NA, 27),
                               rep('NULL', 2)),
                  check.names = F)

dtnettrade <- read.csv("DatNetTrade.csv", skip = 4, header = T, 
                     colClasses=c(NA, rep('NULL', 33), rep(NA, 27),
                                  rep('NULL', 2)),
                     check.names = F)

dtppp <- read.csv("DatPPP.csv", skip = 4, header = T, 
                      colClasses=c(NA, rep('NULL', 33), rep(NA, 27),
                                   rep('NULL', 2)),
                      check.names = F)

dtfcr <- read.csv("DatFCR.csv", skip = 4, header = T, 
                  colClasses=c(NA, rep('NULL', 33), rep(NA, 27),
                               rep('NULL', 2)),
                  check.names = F)


################################# DATA MUNGING #############################
# 2. Data Munging
# 2.1 Merge Worldbank Data ####
library("tidyr")
vec <- list(dtreer, dtinflat, dtdebt, dtdebtext, dtbop, dtgdp,
            dtgdpcapreal, dtgdpcapgr, dtnettrade, dtppp, dtfcr)
names(vec) <- c("reer", "inflation", "debt","debtext", "bop", "gdp",
                "gdpcapreal", "gdpcapgr", "nettrade", "ppp", "fcr")
for (i in 1:length(vec)){
  vec[[i]] <- gather(vec[[i]], "placeholder", "placeholder", 2:28)
  colnames(vec[[i]]) <- c("cname", "year", names(vec[i]))
  if (i == 1){
    dtwb <- vec[[1]]
  } else {
    dtwb <- merge(dtwb, vec[[i]], all = T)
  }
}
rm(vec, i, dtreer, dtinflat, dtdebt, dtdebtext, dtbop, dtgdp,
   dtgdpcapreal, dtgdpcapgr, dtnettrade, dtppp, dtfcr)

# 2.2 Policy Rate Data ####
# 2.3.1 Drop rows that are not needed
dtprates <- dtprates[-which(dtprates$Scale == ""),]

# 2.3.2 Drop columns that are not needed
row.names(dtprates) <- NULL
dtprates <- dtprates[-c(1,3,4)]

# 2.3.3 Changing colnames changing classes
colnames(dtprates) <- c("cname", seq(1990,2017))
dtprates$cname <- as.character(dtprates$cname)

# 2.3.4 Tidy Data
library("tidyr")
dtprates <- gather(dtprates, "entryforceyear", "intrate", 2:ncol(dtprates))
dtprates <- dtprates[order(dtprates$cname),]
row.names(dtprates) <- NULL


# 2.3 PTA Data ####
# 2.1.1 Drop columns that are not needed
dtptadyad <- dtptadyad[,-16:-ncol(dtptadyad)] 
dtptadyad <- dtptadyad[,-8:-10]
dtptadyad <- dtptadyad[,-which(colnames(dtptadyad) == "language")]
dtptadyad <- dtptadyad[,-which(colnames(dtptadyad) == "year")]

# 2.1.3 Rm entries before 1990
dtptadyad <- dtptadyad[!is.na(dtptadyad$entryforceyear),] 
dtptadyad <- dtptadyad[!dtptadyad$entryforceyear < 1990,] 

# 2.1.4 Merge with PTA Depth
dtpta <- merge(dtptadyad, dtptadepth, all = T)
dtpta <- subset(dtpta, !is.na(dtpta$country1))
dtpta <- dtpta[,c(4, 5, seq(1,3), seq(6,ncol(dtpta)))]
row.names(dtpta) <- NULL
rm(dtptadyad, dtptadepth)

# 2.1.4 Seperate Dyad
colnames(dtpta)[1:2] <- rep("cname", 2)
dtpta <- rbind(dtpta[,-2], dtpta[,-1])

# 2.1.5 Compressing Pluri-Agreements
dtpta <- subset(dtpta, !duplicated(dtpta[c("cname", "base_treaty")]))

# 2.1.5 Ordering by country and year
dtpta <- dtpta[order(dtpta$cname, dtpta$entryforceyear),]

# 2.1.6 Merging Entries with same year and same country
i <- 2
counter <- 1
dtpta$agreements <- rep(1, nrow(dtpta))
while (i <= nrow(dtpta)) {
  if(dtpta$cname[i] == dtpta$cname[i-1] && 
     dtpta$entryforceyear[i] == dtpta$entryforceyear[i-1]) {
    dtpta$iso2[i-1] <- paste(dtpta$iso2[i-1], dtpta$iso2[i], sep = ";")
    dtpta$number[i-1] <- paste(dtpta$number[i-1], dtpta$number[i], sep = ";")
    dtpta$name[i-1] <- paste(dtpta$name[i-1], dtpta$name[i], sep = ";")
    dtpta$regioncon[i-1] <- ifelse(dtpta$regioncon[i-1] == dtpta$regioncon[i],
                                   as.character(dtpta$regioncon[i]),
                                   paste(dtpta$regioncon[i-1], dtpta$regioncon[i], sep = ";"))
    dtpta$depth_index[i-1] <- paste(dtpta$depth_index[i-1], dtpta$depth_index[i], sep = ";")
    counter <- counter + 1
    dtpta$agreements[i-1] <- counter
    dtpta <- dtpta[-i,]
  } else {
    counter <- 1
    dtpta$agreements[i] <- counter
    i <- i + 1
  }
} 
rm(i, counter)

# Depth Variables
for (i in 1:nrow(dtpta)){
  dtpta$depthavg[i] <- mean(as.numeric(unlist(strsplit(
    dtpta$depth_index[i], ";"))), na.rm = T)
  dtpta$depthsum[i] <- sum(as.numeric(unlist(strsplit(
    dtpta$depth_index[i], ";"))), na.rm = T)
}

# 2.1.7 Filling the gaps
x <- sort(rep(unique(dtpta$cname), 27))
y <- rep(seq(1990,2016), length(unique(dtpta$cname)))
dtptaframe <- data.frame(cname = x, entryforceyear = y)

dtpta <- merge(dtpta, dtptaframe, all = T)
rm(dtptaframe, x, y)

dtpta$agreements[which(is.na(dtpta$agreements))] <- 0

# Depth
dtpta$depthavg[which(is.na(dtpta$depthavg))] <- 0
dtpta$depthsum[which(is.na(dtpta$depthsum))] <- 0

# 2.1.8 Dichotom Variable
dtpta$agreementsigned <- ifelse(dtpta$agreements >= 1, 1, 0)

# 2.1.8 Accumulated Agreements
dtpta$agreementsnumber <- 0
dtpta$depthacc <- 0
j <- c(0,0)
k <- c(0,0)
for (i in 1:nrow(dtpta)){
  if (dtpta$entryforceyear[i] == 1990){
    j <- c(0,0)
    k <- c(0,0)
  }
  if (dtpta$agreements[i] != 0){
    j <- append(j, i)
    if (j[length(j)-1] == 0){
      dtpta$agreementsnumber[i] <- dtpta$agreements[i]
    } else {
      dtpta$agreementsnumber[i] <- dtpta$agreementsnumber[j[length(j)-1]] + dtpta$agreements[i]
    }
  }
  if (!is.na(dtpta$depth_index[i])){
    k <- append(k, i)
    if (k[length(k)-1] == 0){
      dtpta$depthacc[i] <- dtpta$depthsum[i]
    } else {
      dtpta$depthacc[i] <- dtpta$depthacc[k[length(k)-1]] + dtpta$depthsum[i]
    }
    
  }
}
rm(i, j, k)

utils::View(dtpta[c("cname","entryforceyear","depth_index","depthsum","depthacc")])


# 2.4 Polity Data ####
# Drop before 1990
dtpolity <- subset(dtpolity, dtpolity$year >= 1990)

# Rename Variable
colnames(dtpolity) <- c("cname", "entryforceyear", "polity")

################################### MERGING ################################
# 3. Data Merging
# 3.1 Compatibility
# 3.1.1 PTA Data ####
# Comparing
vector1 <- NULL
for (i in 1:length(unique(dtpta$cname))){
  if (!unique(dtpta$cname)[i] %in% unique(dtwb$cname)){
    vector1 <- append(vector1, as.character(unique(dtpta$cname)[i]))
  }
}
rm(i)

vector2 <- NULL
for (i in 1:length(unique(dtwb$cname))){
  if (!unique(dtwb$cname)[i] %in% unique(dtpta$cname)){
    vector2 <- append(vector2, as.character(unique(dtwb$cname)[i]))
  }
}
rm(i)

data.frame(dtpta = c(vector1, rep("NA", 65)), dtwb = vector2) %>%
  utils::View()

dtpta$cname <- as.character(dtpta$cname)

# Tidying
dtpta[which(dtpta$cname == "Bahamas"), 1] <- "Bahamas, The"
dtpta[which(dtpta$cname == "Bolivia, Plurinational State of"), 1] <- "Bolivia"
dtpta[which(dtpta$cname == "Congo"), 1] <- "Congo, Rep."
dtpta[which(dtpta$cname == "Congo, the Democratic Republic of the"), 1] <- "Congo, Dem. Rep."
dtpta[which(dtpta$cname == "Egypt"), 1] <- "Egypt, Arab Rep."
dtpta[which(dtpta$cname == "Gambia"), 1] <- "Gambia, The"
dtpta[which(dtpta$cname == "Hong Kong"), 1] <- "Hong Kong SAR, China"
dtpta[which(dtpta$cname == "Iran, Islamic Republic of"), 1] <- "Iran, Islamic Rep."
dtpta[which(dtpta$cname == "Korea, Republic of"), 1] <- "Korea, Rep."
dtpta[which(dtpta$cname == "Kyrgyzstan"), 1] <- "Kyrgyz Republic"
dtpta[which(dtpta$cname == "Lao People's Democratic Republic"), 1] <- "Lao PDR"
dtpta[which(dtpta$cname == "Macao"), 1] <- "Macao SAR, China"
dtpta[which(dtpta$cname == "Macedonia, the former Yugoslav Republic of"), 1] <- "Macedonia, FYR"
dtpta[which(dtpta$cname == "Moldova, Republic of"), 1] <- "Moldova"
dtpta[which(dtpta$cname == "Saint Kitts and Nevis"), 1] <- "St. Kitts and Nevis"
dtpta[which(dtpta$cname == "Saint Lucia"), 1] <- "St. Lucia"
dtpta[which(dtpta$cname == "Saint Vincent and the Grenadines"), 1] <- "St. Vincent and the Grenadines"
dtpta[which(dtpta$cname == "Slovakia"), 1] <- "Slovak Republic"
dtpta[which(dtpta$cname == "Tanzania, United Republic of"), 1] <- "Tanzania"
dtpta[which(dtpta$cname == "Venezuela, Bolivarian Republic of"), 1] <- "Venezuela, RB"
dtpta[which(dtpta$cname == "Viet Nam"), 1] <- "Vietnam"
dtpta[which(dtpta$cname == "Yemen"), 1] <- "Yemen, Rep."

# 3.1.2 Policy Rate Data ####
# Comparing
vector1 <- NULL
for (i in 1:length(unique(dtprates$cname))){
  if (!unique(dtprates$cname)[i] %in% unique(dtwb$cname)){
    vector1 <- append(vector1, as.character(unique(dtprates$cname)[i]))
  }
}
rm(i)

vector2 <- NULL
for (i in 1:length(unique(dtwb$cname))){
  if (!unique(dtwb$cname)[i] %in% unique(dtprates$cname)){
    vector2 <- append(vector2, as.character(unique(dtwb$cname)[i]))
  }
} 
rm(i)

data.frame(dtprates = c(vector1,
                        rep("NA",abs(length(vector1) - length(vector2)))),
           dtwb = vector2) %>% utils::View()

# Tidying
dtprates[which(dtprates$cname == "Afghanistan, Islamic Republic of"), 1] <- "Afghanistan"
dtprates[which(dtprates$cname == "Bahrain, Kingdom of"), 1] <- "Bahrain"
dtprates[which(dtprates$cname == "Congo, Democratic Republic of"), 1] <- "Congo, Dem. Rep."
dtprates[which(dtprates$cname == "Egypt"), 1] <- "Egypt, Arab Rep."
dtprates[which(dtprates$cname == "Korea, Republic of"), 1] <- "Korea, Rep."
dtprates[which(dtprates$cname == "Russia"), 1] <- "Russian Federation"
dtprates[which(dtprates$cname == "Serbia, Republic of"), 1] <- "Serbia"


# 3.1.3 CBI Data ####
# Comparing
vector1 <- NULL
for (i in 1:length(unique(dtcbi$cname))){
  if (!unique(dtcbi$cname)[i] %in% unique(dtwb$cname)){
    vector1 <- append(vector1, as.character(unique(dtcbi$cname)[i]))
  }
}
rm(i)

vector2 <- NULL
for (i in 1:length(unique(dtwb$cname))){
  if (!unique(dtwb$cname)[i] %in% unique(dtcbi$cname)){
    vector2 <- append(vector2, as.character(unique(dtwb$cname)[i]))
  }
}
rm(i)

data.frame(dtcbi = c(vector1,
                     rep("NA", abs(length(vector1) - length(vector2)))),
           dtwb = vector2) %>% utils::View()

# Tidying
dtcbi[which(dtcbi$cname == "United States of America"), 1] <- "United States"
dtcbi[which(dtcbi$cname == "Bahamas"), 1] <- "Bahamas, The"
dtcbi[which(dtcbi$cname == "Saint Lucia"), 1] <- "St. Lucia"
dtcbi[which(dtcbi$cname == "Antigua & Barbuda"), 1] <- "Antigua and Barbuda"
dtcbi[which(dtcbi$cname == "Venezuela"), 1] <- "Venezuela, RB"
dtcbi[which(dtcbi$cname == "Slovakia"), 1] <- "Slovak Republic"
dtcbi[which(dtcbi$cname == "Macedonia"), 1] <- "Macedonia, FYR"
dtcbi[which(dtcbi$cname == "Serbia "), 1] <- "Serbia"
dtcbi[which(dtcbi$cname == "Bosnia-Herzegovina"), 1] <- "Bosnia and Herzegovina"
dtcbi[which(dtcbi$cname == "Gambia"), 1] <- "Gambia, The"
dtcbi[which(dtcbi$cname == "Ivory Coast"), 1] <- "Cote d'Ivoire"
dtcbi[which(dtcbi$cname == "Congo, Republic of"), 1] <- "Congo, Rep."
dtcbi[which(dtcbi$cname == "Congo, Democratic Republic of / Za"), 1] <- "Congo, Dem. Rep."
dtcbi[which(dtcbi$cname == "Iran"), 1] <- "Iran, Islamic Rep."
dtcbi[which(dtcbi$cname == "Egypt"), 1] <- "Egypt, Arab Rep."
dtcbi[which(dtcbi$cname == "Syria"), 1] <- "Syrian Arab Republic"
dtcbi[which(dtcbi$cname == "Yemen"), 1] <- "Yemen, Rep."
dtcbi[which(dtcbi$cname == "Kyrgyzstan"), 1] <- "Kyrgyz Republic"
dtcbi[which(dtcbi$cname == "Korea, Republic of"), 1] <- "Korea, Rep."
dtcbi[which(dtcbi$cname == "Myanmar (Burma)"), 1] <- "Myanmar"
dtcbi[which(dtcbi$cname == "Laos"), 1] <- "Lao PDR"

# Remove years before 1990
dtcbi <- subset(dtcbi, dtcbi$year >= 1990)


# 3.1.4 Polity Data ####
vector1 <- NULL
for (i in 1:length(unique(dtpolity$cname))){
  if (!unique(dtpolity$cname)[i] %in% unique(dtwb$cname)){
    vector1 <- append(vector1, as.character(unique(dtpolity$cname)[i]))
  }
}
rm(i)

vector2 <- NULL
for (i in 1:length(unique(dtwb$cname))){
  if (!unique(dtwb$cname)[i] %in% unique(dtpolity$cname)){
    vector2 <- append(vector2, as.character(unique(dtwb$cname)[i]))
  }
}
rm(i)

data.frame(dtpolity = c(vector1,
                     rep("NA", abs(length(vector1) - length(vector2)))),
           dtwb = vector2) %>% edit()

dtpolity$cname <- as.character(dtpolity$cname)

# Tidying
dtpolity[which(dtpolity$cname == "Bosnia"), 1] <- "Bosnia and Herzegovina"
dtpolity[which(dtpolity$cname == "Congo Brazzaville"), 1] <- "Congo, Rep."
dtpolity[which(dtpolity$cname == "Egypt"), 1] <- "Egypt, Arab Rep."
dtpolity[which(dtpolity$cname == "Timor Leste"), 1] <- "Timor-Leste"
dtpolity[which(dtpolity$cname == "Gambia"), 1] <- "Gambia, The"
dtpolity[which(dtpolity$cname == "Iran"), 1] <- "Iran, Islamic Rep."
dtpolity[which(dtpolity$cname == "Cote D'Ivoire"), 1] <- "Cote d'Ivoire"
dtpolity[which(dtpolity$cname == "Kyrgyzstan"), 1] <- "Kyrgyz Republic"
dtpolity[which(dtpolity$cname == "Laos"), 1] <- "Lao PDR"
dtpolity[which(dtpolity$cname == "Macedonia"), 1] <- "Macedonia, FYR"
dtpolity[which(dtpolity$cname == "Myanmar (Burma)"), 1] <- "Myanmar"
dtpolity[which(dtpolity$cname == "Korea South"), 1] <- "Korea, Rep."
dtpolity[which(dtpolity$cname == "Russia"), 1] <- "Russian Federation"
dtpolity[which(dtpolity$cname == "Syria"), 1] <- "Syrian Arab Republic"
dtpolity[which(dtpolity$cname == "UAE"), 1] <- "United Arab Emirates"
dtpolity[which(dtpolity$cname == "Venezuela"), 1] <- "Venezuela, RB"
dtpolity[which(dtpolity$cname == "Yemen"), 1] <- "Yemen, Rep."
dtpolity[which(dtpolity$cname == "Congo Kinshasa"), 1] <- "Congo, Dem. Rep."


# 3.2 Merge ####
colnames(dtwb)[2] <- "entryforceyear"
colnames(dtcbi)[3] <- "entryforceyear"
#testdf <- merge(dtpta, dtwb, all = T)

vec <- list(dtpta, dtwb, dtcbi, dtprates, dtpolity)
for (i in 1:length(vec)){
  if (i == 1){
    dtfull <- vec[[1]]
  } else {
    dtfull <- merge(dtfull, vec[[i]], all = T)
  }
}
rm(vec, i)

# Rm all entries without country name
dtfull <- subset(dtfull, !is.na(dtfull$cname))

# Rm all entries without agreements
dtfull <- subset(dtfull, !is.na(dtfull$agreementsigned))

# Rm Ethiopia (2 times for same year in data)
dtfull <- dtfull[-1543,]

############################ CREATE NEW VARIABLES ##########################
# REER ####
# REER-Change to t+1, t+2, t+3
for (i in 1:nrow(dtfull)) {
  if (dtfull$entryforceyear[i] != "2016"){
    dtfull$chreerplus1[i] <- (dtfull$reer[i+1] - dtfull$reer[i]) /
      dtfull$reer[i] * 100 #creates t+1
  } 
  if (all(dtfull$entryforceyear[i] != c("2015","2016"))){
    dtfull$chreerplus2[i] <- (dtfull$reer[i+2] - dtfull$reer[i]) /
      dtfull$reer[i] * 100 #creates t+2
  } 
  if (all(dtfull$entryforceyear[i] != c("2014","2015","2016"))){
    dtfull$chreerplus3[i] <- (dtfull$reer[i+3] - dtfull$reer[i]) /
      dtfull$reer[i] * 100 #creates t+3
  }
}

# Undervalue ####
#Copelovitch und pevehouse Undervalued
dtfull$rer <- dtfull$fcr/dtfull$ppp

library(plm)
plmdtfull <-  pdata.frame(dtfull, index = c("cname","entryforceyear"))

#utils::View(table(index(plmdtfull), useNA = "ifany"))
#any(table(index(plmdtfull), useNA = "ifany") > 1)
x <- plm(log(rer) ~ log(gdpcapreal), data = plmdtfull,
         model = "within", effect = "time")

tempdata <- as.data.frame(x$residuals)
tempdata$rownam <- row.names(tempdata)
tempdata <- cbind(tempdata,fitted(x))

library(stringr)
tempdata$cname <- str_split_fixed(tempdata$rownam,"-", 2)[,1]
tempdata$entryforceyear <- str_split_fixed(tempdata$rownam,"-", 2)[,2]
tempdata <- tempdata[c(4,5,3)]
colnames(tempdata)[3] <- "fitted"
tempdata[1613:1639,1] <- "Guinea-Bissau"
tempdata[1613:1639,2] <- seq(1990,2016)

dtfull <- merge(dtfull, tempdata, all = T)
rm(tempdata)

dtfull$underval <- as.numeric(log(dtfull$rer) - dtfull$fitted)
rm(plmdtfull)

for (i in 1:(nrow(dtfull)-1)) {
  for (j in 1:5){
    if (isTRUE(dtfull$cname[i] == dtfull$cname[i+j])){
      dtfull[i,sprintf("dunderval%s",j)] <- (dtfull$underval[i+j] - dtfull$underval[i])
    }
  }
}

#t+1,+2,+3,+4,+5
for (i in 1:(nrow(dtfull)-1)) {
  for (j in 1:5){
    if (isTRUE(dtfull$cname[i] == dtfull$cname[i+j])){
      dtfull[i,sprintf("undervalp%s",j)] <- dtfull$underval[i+j]
    }
  }
}

# Policy Rates ####
# t+1,2,3,4,5
for (i in 1:(nrow(dtfull)-1)) {
  for (j in 1:5){
    if (isTRUE(dtfull$cname[i] == dtfull$cname[i+j])){
      dtfull[i,sprintf("pratp%s",j)] <- dtfull$intrate[i+j]
    }
  }
}

# Change
for (i in 1:(nrow(dtfull)-1)) {
  for (j in 1:5){
    if (isTRUE(dtfull$cname[i] == dtfull$cname[i+j])){
      dtfull[i,sprintf("dprat%s",j)] <- dtfull$intrate[i+j] - dtfull$intrate[i]
    }
  }
}


# Crisis ####
dtfull$crisis <- ifelse(dtfull$gdpcapgr < -15, 1, 0)

for (i in 2:nrow(dtfull)) {
  if ((dtfull$cname[i] == dtfull$cname[i-1]) &
      dtfull$inflation[i] > 40 &
      isTRUE(dtfull$inflation[i] - dtfull$inflation[i-1] > 25)){
    dtfull$crisis[i] <- 1
  }
}

# Debt ####
dtfull$debtgdp <- sqrt(dtfull$debtext/dtfull$gdp)

############################## DATA SET MUNGING ############################
# Tidy ####
# Rescale GDP and Nettrade in billion

dtfull$gdp <- dtfull$gdp/1000000000
dtfull$nettrade <- dtfull$nettrade/1000000000

# Rm eurozone entries
dtfull <- subset(dtfull, !(cname %in% c("Germany","France","Spain","Italy",
                            "Belgium","Finland","Greece","Portugal",
                            "Luxembourg","Netherlands","Austria",
                            "Ireland") &
         entryforceyear >= 2000))

dtfull <- subset(dtfull, !(cname %in% c("Slovenia","Malta", "Cyprus",
                              "Slovak Republic") &
                   entryforceyear >= 2007))

dtfull <- subset(dtfull, !(cname %in% c("Estonia","Slovak Republic") &
                   entryforceyear >= 2009))

dtfull <- subset(dtfull, !(cname %in% c("Latvia","Lithuania") &
                   entryforceyear >= 2014))

row.names(dtfull) <- NULL

############################### SAVE DATA SET ##############################
save(list = "dtfull", file = "DatFull.RData")
write.csv(dtfull, file = "DatFull.csv")
################################### NOTES #################################
#
# - I will use Panel Data
# - One Data Set with only floating currencies
# - One Data Set with only Developing COuntries
# 
# Filtering:
# - only base treaties
# - only english treaties
#
# Subsets:
# - Subset with only PTAs with two parties
#
# Document:
# - Which Countries where out of data because REER NAs
# - Show how the reidual Countries are dispersed (more emerging countries?)

# To access World Bank Data use the WDI package

# Data Tidying - 
# https://garrettgman.github.io/tidying/



testdf <- data.frame(cntry = c(rep("turk",2), rep("Albania", 3), rep("Israel", 4)),
                     year = c(2003, rep(2004, 4), 2007, 2008, 2008, 2009),
                     iso = sample(234:738, 9, replace = T))

testdf2 <- data.frame(cntry = x, year = y)
x <- sort(rep(unique(testdf$cntry), 6))
y <- rep(seq(2005,2010), 3)

merge(testdf, testdf2, all = T)

i <- 1
while (i <= nrow(ptabil)){
  if (testdf$year )
}




i <- 2
counter <- 1
testdf$agreements <- rep(1,nrow(testdf))
while (i <= nrow(testdf)) {
  if(testdf$cntry[i] == testdf$cntry[i-1] && 
     testdf$year[i] == testdf$year[i-1]) {
    testdf$iso[i-1] <- paste(testdf$iso[i-1], testdf$iso[i], sep = ";")
    counter <- counter + 1
    testdf$agreements[i-1] <- counter
    testdf <- testdf[-i,]
    print(i)
  } else {
    counter <- 1
    testdf$agreements[i] <- 1
    i <- i + 1
    print(i)
  }
}


testdf <- data.frame(x = c(0,1,0,0,2,0,4,0,2), y = rep(0, 9), 
                     z = c(seq(1990,1994),seq(1990,1993))) 
j <- c(0,0)
for (i in 1:9){
  if (testdf$z[i] == 1990){
    j <- c(0,0)
  }
  if (testdf$x[i] != 0){
    j <- append(j, i)
    if (j[length(j)-1] == 0){
      testdf$y[i] <- testdf$x[i]
    } else {
      testdf$y[i] <- testdf$y[j[length(j)-1]] + testdf$x[i]
    }
  }
}

#World Development Indicators
library(WDI)
nettrade <- WDI(country = "all", indicator = "BN.GSR.MRCH.CD",
                start = 1990, end = 2016, extra = FALSE, cache = NULL)

# Special PLM-Model for regressing undervaluation
for (i in 1:nrow(dtfull)){
  if (!is.na(dtfull$reer[i]) & !is.na(dtfull$gdpcapreal[i]) &
      isTRUE(dtfull$cname[i] == dtfull$cname[i+4])){
    x <- lm((log(reer[i:(i+4)]/100)) ~ log(gdpcapreal[i:(i+4)]),
            data = dtfull)
    dtfull$underval[i] <- as.numeric((dtfull$reer[i]/100) - coef(x)[[2]])
  } else {
    dtfull$underval[i] <- NA
  }
}