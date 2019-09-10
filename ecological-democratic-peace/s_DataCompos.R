###########################################################################|
###################   D A T A   C O M P O S I T I O N   ###################|
###########################################################################|


############################################################################
## LOAD AND CLEANING #######################################################
clean <- function(object, sectors = FALSE) {
  
  # Eora or Exio
  exio <- grepl("exio", object)
  pxp <- grepl("pxp", object)
  
  # Load
  object <- load(paste0("o_", object, ".RData"))
  object <- get(object)
  
  # Use Final Consumption and delete Rest-of-World rows
  if (exio) {
    object <- object[which(object$S2 == "Final consumption expenditure by households"),]
    if (pxp) {
      object <- object[-which(object$C1 %in% c("WA", "WE", "WF", "WL", "WM")),]
      object <- object[-which(object$C2 %in% c("WA", "WE", "WF", "WL", "WM")),]
      object$C1 <- countrycode::countrycode(object$C1, origin = "iso2c",
                                            destination = "iso3c")
      object$C2 <- countrycode::countrycode(object$C2, origin = "iso2c",
                                            destination = "iso3c")
    } else {
      object <- object[-which(object$C1 %in% c("WWA", "WWE", "WWF", "WWL", "WWM")),]
      object <- object[-which(object$C2 %in% c("WA", "WE", "WF", "WL", "WM")),]
      object$C2 <- countrycode::countrycode(object$C2, origin = "iso2c",
                                            destination = "iso3c")
      object[which(object$C1 == "ROM"),]$C1 <- "ROU"
    }
  } else {
    object <- object[which(object$S2 == "Household final consumption P.3h"),]
    object <- object[-which(object$C2 == "ROW"),]
    object <- object[-which(object$C1 == "ROW"),]
  }
  
  # Decide if sectors should be included
  if (!sectors) {
    # Aggregate Values for Each Country
    object <- aggregate(object$value, by = list(object$year, object$C1,
                                                object$C2), FUN = sum)
    
    # Change Variable Names and Only Exclude Domestic Production-Consumption
    colnames(object) <- c("year", "C1", "C2", "value")
  } else {
    object$S2 <- NULL
  }
  
  # Exclude Domestic Production-Consumption
  object <- object[-which(object$C1 == object$C2),]
  
  
  return(object)
}


# Eora --------------------------------------------------------------------
clean("eora_d_bw") -> eora_d_bw
clean("eora_d_cc") -> eora_d_cc
clean("eora_d_en") -> eora_d_en
clean("eora_d_lu") -> eora_d_lu
clean("eora_d_mf") -> eora_d_mf

colnames(eora_d_cc)[4] <- "cf_cc"
colnames(eora_d_mf)[4] <- "cf_mf"
colnames(eora_d_lu)[4] <- "cf_lu"
colnames(eora_d_bw)[4] <- "cf_bw"
colnames(eora_d_en)[4] <- "cf_en"

merge(eora_d_bw, eora_d_cc) -> eora_d_all
merge(eora_d_all, eora_d_en) -> eora_d_all
merge(eora_d_all, eora_d_lu) -> eora_d_all
merge(eora_d_all, eora_d_mf) -> eora_d_all

save(eora_d_all, file = "o_eora_d_all.RData")


# Exiobase ----------------------------------------------------------------
clean("exio_d_bl") -> exio_d_bl
clean("exio_d_bw") -> exio_d_bw
clean("exio_d_cc") -> exio_d_cc
clean("exio_d_en") -> exio_d_en
clean("exio_d_lu") -> exio_d_lu
clean("exio_d_mf") -> exio_d_mf
clean("exio_d_ws") -> exio_d_ws

colnames(exio_d_bl)[4] <- "cf_bl"
colnames(exio_d_bw)[4] <- "cf_bw"
colnames(exio_d_cc)[4] <- "cf_cc"
colnames(exio_d_en)[4] <- "cf_en"
colnames(exio_d_lu)[4] <- "cf_lu"
colnames(exio_d_mf)[4] <- "cf_mf"
colnames(exio_d_ws)[4] <- "cf_ws"

merge(exio_d_bl, exio_d_bw) -> exio_d_all
merge(exio_d_all, exio_d_cc) -> exio_d_all
merge(exio_d_all, exio_d_en) -> exio_d_all
merge(exio_d_all, exio_d_lu) -> exio_d_all
merge(exio_d_all, exio_d_mf) -> exio_d_all
merge(exio_d_all, exio_d_ws) -> exio_d_all

save(exio_d_all, file = "o_exio_d_all.RData")

  # Exiobase Sectors -------------------------------------------------------
  clean("exio_d_bl", sectors = T) -> exio_d_bl_s
  clean("exio_d_bw", sectors = T) -> exio_d_bw_s
  clean("exio_d_cc", sectors = T) -> exio_d_cc_s
  clean("exio_d_en", sectors = T) -> exio_d_en_s
  clean("exio_d_lu", sectors = T) -> exio_d_lu_s
  clean("exio_d_mf", sectors = T) -> exio_d_mf_s
  clean("exio_d_ws", sectors = T) -> exio_d_ws_s
  
  colnames(exio_d_bl_s)[5] <- "cf_bl"
  colnames(exio_d_bw_s)[5] <- "cf_bw"
  colnames(exio_d_cc_s)[5] <- "cf_cc"
  colnames(exio_d_en_s)[5] <- "cf_en"
  colnames(exio_d_lu_s)[5] <- "cf_lu"
  colnames(exio_d_mf_s)[5] <- "cf_mf"
  colnames(exio_d_ws_s)[5] <- "cf_ws"
  
  merge(exio_d_bl_s, exio_d_bw_s) -> exio_d_all_s
  merge(exio_d_all_s, exio_d_en_s) -> exio_d_all_s
  merge(exio_d_all_s, exio_d_lu_s) -> exio_d_all_s
  merge(exio_d_all_s, exio_d_mf_s) -> exio_d_all_s
  merge(exio_d_all_s, exio_d_ws_s) -> exio_d_all_s
  merge(exio_d_all_s, exio_d_cc_s) -> exio_d_all_s
  
  save(exio_d_all_s, file = "o_exio_d_all_s.RData")

  # Exiobase Product to Product --------------------------------------------
  # clean("exio_d_bl_pxp") -> exio_d_bl_pxp
  clean("exio_d_bw_pxp", sectors = T) -> exio_d_bw_pxp
  clean("exio_d_cc_pxp", sectors = T) -> exio_d_cc_pxp
  clean("exio_d_en_pxp", sectors = T) -> exio_d_en_pxp
  clean("exio_d_lu_pxp", sectors = T) -> exio_d_lu_pxp
  clean("exio_d_mf_pxp", sectors = T) -> exio_d_mf_pxp
  # clean("exio_d_ws_pxp") -> exio_d_ws_pxp
  
  # colnames(exio_d_bl_pxp)[5] <- "cf_bl_pxp"
  colnames(exio_d_bw_pxp)[5] <- "cf_bw"
  colnames(exio_d_cc_pxp)[5] <- "cf_cc"
  colnames(exio_d_en_pxp)[5] <- "cf_en"
  colnames(exio_d_lu_pxp)[5] <- "cf_lu"
  colnames(exio_d_mf_pxp)[5] <- "cf_mf"
  # colnames(exio_d_ws_pxp)[5] <- "cf_ws_pxp"
  
  # merge(exio_d_bl_pxp, exio_d_bw_pxp) -> exio_d_all_pxp
  merge(exio_d_bw_pxp, exio_d_cc_pxp) -> exio_d_all_pxp
  merge(exio_d_all_pxp, exio_d_en_pxp) -> exio_d_all_pxp
  merge(exio_d_all_pxp, exio_d_lu_pxp) -> exio_d_all_pxp
  merge(exio_d_all_pxp, exio_d_mf_pxp) -> exio_d_all_pxp
  # merge(exio_d_all_pxp, exio_d_ws_pxp) -> exio_d_all_pxp
  
  save(exio_d_all_pxp, file = "o_exio_d_all_pxp.RData")



###########################################################################|
#############   I N D E P E N D E N T     V A R I A B L E S   #############|
###########################################################################|
load("~/MA-Thesis/Empirical Analysis/o_eora_d_all.RData")
load("~/MA-Thesis/Empirical Analysis/o_exio_d_all.RData")
# load("~/MA-Thesis/Empirical Analysis/o_exio_d_all_s.RData")
# load("~/MA-Thesis/Empirical Analysis/o_exio_d_all_pxp.RData")


############################################################################
############################################################################
## GEOGRAPHY ###############################################################
dist <- readxl::read_excel("C:/Data/CEPII/dist_cepii/dist_cepii.xls")

# grav <- foreign::read.dta("C:/Data/CEPII/gravdata_cepii/gravdata.dta")

dist <- dist[, c(1,2,4,5,6,10,11)]
colnames(dist)[1:2] <- c("C1", "C2")
dist$distgroup <- dist$distgroup <- cut(dist$dist, breaks = 10, labels = 1:10)


# Merge
distmain_exio <- merge(exio_d_all, dist, all = T)
# distmain_exio_s <- merge(exio_d_all_s, dist, all = T)
# distmain_exio_pxp <- merge(exio_d_all_pxp, dist, all = T)
distmain_eora <- merge(eora_d_all, dist, all = T)
rm(dist)



## INCOME ##################################################################
# WDI::WDIsearch("gdp per capita")
inc <- WDI::WDI(indicator = "NY.GDP.PCAP.CD", start = 1989, end = 2015)
pop <- WDI::WDI(indicator = "SP.POP.TOTL", start = 1989, end = 2015)
prr <- WDI::WDI(indicator = "IQ.CPA.PROP.XQ", start = 1989, end = 2015)
ine <- WDI::WDI(indicator = "SI.DST.10TH.10", start = 1989, end = 2015)

# - inc: GDP per capita (constant 2010 US$)
# - pop: Total Popoulation
# - prr: CPIA property rights and rule-based governance rating (1=low to 6=high)
# - ine: Income share held by highest 10% 

# Merge
wdi <- merge(inc, pop)
wdi <- merge(wdi, prr)
wdi <- merge(wdi, ine); rm(inc, pop, prr, ine)

# Change country codes
wdi$iso2c <- countrycode::countrycode(wdi$iso2c, origin = "iso2c",
                                      destination = "iso3c")
colnames(wdi)[1] <- "C1"
colnames(wdi)[4:7] <- c("gdppc_C1", "pop_C1", "prop_C1", "ineq_C1")

wdi <- wdi[, -2]
wdi <- subset(wdi, year %in% 1989:2015)

wdi2 <- wdi
colnames(wdi2)[1] <- "C2"
colnames(wdi2)[3:6] <- c("gdppc_C2", "pop_C2", "prop_C2", "ineq_C2")

wdi <- merge(wdi, wdi2); rm(wdi2)
wdi$gdppcdiff <- wdi$gdppc_C2 - wdi$gdppc_C1 # from consumer perspective
wdi$propdiff <- wdi$prop_C2 - wdi$prop_C1 # from consumer perspective
wdi$ineqdiff <- wdi$ineq_C2 - wdi$ineq_C1 # from consumer perspective
wdi$popdiff <- wdi$pop_C2 - wdi$pop_C1 # from consumer perspective

# Merge
wdimain_exio <- merge(exio_d_all, wdi, all = T)
# wdimain_exio_s <- merge(exio_d_all_s, wdi, all = T)
# wdimain_exio_pxp <- merge(exio_d_all_pxp, wdi, all = T)
wdimain_eora <- merge(eora_d_all, wdi, all = T)
rm(wdi)



############################################################################
## REGIME ##################################################################
vdem <- readRDS("C:/Data/VDEM/V-Dem-CY-Full+Others-v9.rds")
dd <- readxl::read_xls("C:/Data/DD/ddrevisited_data_v1.xls")

# Clean DD
dd$country_text_id <- countrycode::countrycode(sourcevar = dd$cowcode,
                                               origin = "cown", destination = "iso3c")
dd <- subset(dd, year >= 1989)
dd <- dd[which(colnames(dd) %in% c("country_text_id", "year", "democracy"))]
'%ni%' <- Negate('%in%')
drop <- unique(dd$country_text_id)[which(unique(dd$country_text_id) 
                                         %ni% unique(vdem$country_text_id))]
dd <- subset(dd, country_text_id %ni% drop)

# Select relevant variables and year
vdem <- subset(vdem, year %in% 1989:2015)
vdem <- subset(vdem, select = c(country_text_id, year, v2x_regime_amb,
                                v2x_polyarchy, v2x_libdem, v2xeg_eqdr,
                                e_polity2, e_miurbani, e_total_resources_income_pc,
                                e_ti_cpi, e_peaveduc))

# Merge with DD
vdem <- merge(vdem, dd, all = T)
vdem <- subset(vdem, !is.na(country_text_id))


# Change and add variables
vdem2 <- vdem
colnames(vdem2)[c(1, 3:length(vdem))] <- paste0(colnames(vdem2)[c(1, 3:length(vdem))], 2)
vdem <- merge(vdem, vdem2); rm(vdem2)
lab <- c("dem_world", "dem_elec", "dem_lib", "eq_res", "dem_polity",
           "urban", "res", "cor", "educ", "dem_dd")
colnames(vdem)[2:ncol(vdem)] <- c("C1", paste0(lab, "_C1"), "C2", paste0(lab, "_C2"))

# Calculate Differences
l <- (length(vdem)-3)/2
vdem[(ncol(vdem)+1):(ncol(vdem)+length(lab))] <- vdem[(l+4):length(vdem)] -
                                                                   vdem[3:(l+2)]

colnames(vdem)[(ncol(vdem)+1-length(lab)):ncol(vdem)] <- lab; rm(lab)

# Merge
regmain_exio <- merge(exio_d_all, vdem, all = T)
# regmain_exio_s <- merge(exio_d_all_s, vdem, all = T)
# regmain_exio_pxp <- merge(exio_d_all_pxp, vdem, all = T)
regmain_eora <- merge(eora_d_all, vdem, all = T)
rm(vdem)

# VDEM Notes
# - v2x_regime_amb regimes of the world and compare them, into different categories
# - v2x_polyarchy for electoral democracy
# - v2x_libdem for liberal democracy
# - v2xeg_eqdr -> equal distribution of resources index
# - e-polity2: revised polity4 index
# - e_miurbani -> urbanization
# - e_total_resources_income_pc -> overall resource abundance
# - e_ti_cpi -> corruption perception index
# - e_peaveduc -> education


# # Democratic Peace Measures
# # being more democratic
# vdem[(ncol(vdem)+1):(ncol(vdem)+length(lab))] <- (vdem[(l+4):length(vdem)] -
#                                                   vdem[3:(l+2)])*vdem[(l+4):length(vdem)]
# # being dem peace dicho
# if (dem_world_C1 == 9 & dem_world_C2 == 9) {
#   peace
# }
# 
# # categorizing the dem peace
# vdem[(ncol(vdem)+1):(ncol(vdem)+length(lab))] <- vdem[(l+4):length(vdem)] + vdem[3:(l+2)]



############################################################################
## TRADE ###################################################################

# Load Base List
pta <- read.csv("C:/Data/DESTA/list_of_treaties_01_05_dyads.csv")
colnames(pta)[6] <- "number"
pta$iso1 <- countrycode::countrycode(pta$iso1, origin = "iso3n",
                                     destination = "iso3c")
pta$iso2 <- countrycode::countrycode(pta$iso2, origin = "iso3n",
                                     destination = "iso3c")
pta <- pta[order(pta$entryforceyear), c(3, 4, 6, 12)]
pta <- pta[!is.na(pta$entryforceyear),]
pta$dyad <- paste0(pta$iso1, "_", pta$iso2)
pta <- subset(pta, !is.na(pta$iso1) & !is.na(pta$iso2))

# Load depth
pta_dep <- read.csv("C:/Data/DESTA/depth_version_01_05.csv")
colnames(pta_dep)[2] <- "number"
pta_dep <- pta_dep[c(2, 13)]

# Load Environmental Provisions
pta_env <- read.csv("C:/Data/DESTA/nti_201711.csv")
pta_env <- pta_env[c(1, 4:6)]

# Merge
pta <- merge(pta, pta_dep)
pta <- merge(pta, pta_env)

# Change
pta <- pta[order(pta$entryforceyear),]

# Remove Duplicates and multiple entries
pta$dup <- paste0(pta$entryforceyear, pta$dyad)
for (i in unique(pta[which(duplicated(pta$dup)),]$dup)) {
  pta[pta$dup %in% i,]$depth_index <- mean(pta[pta$dup %in% i,]$depth_index)
  pta[pta$dup %in% i,]$ep_all_lta <- mean(pta[pta$dup %in% i,]$ep_all_lta)
  pta <- pta[-which(row.names(pta) %in% row.names(pta[pta$dup %in% i,])
                    [2:nrow(pta[pta$dup %in% i,])]),]
}
pta$dup <- NULL

# Depth times Environmental Provision
pta$ep_all_lta_dep <- pta$ep_all_lta * pta$depth_index

# Count Number of PTAs, Depth, and Environmental Provisions
for (i in unique(pta$dyad)) {
  
  # Number
  pta[which(pta$dyad == i), 11] <- seq(1:length(which(pta$dyad == i)))
  colnames(pta)[11] <- "pta_sum"
  
  # Depth
  de <- pta[which(pta$dyad == i),]$depth_index
  if (length(de) > 1) { for (j in 2:length(de)) {
    de[j] <- de[j] + de[j-1]
  }}
  pta[which(pta$dyad == i), 12] <- de
  colnames(pta)[12] <- "depth_sum"
  
  # Environmental Provision
  ep <- pta[which(pta$dyad == i),]$ep_all_lta
  if (length(de) > 1) { for (j in 2:length(ep)) {
    ep[j] <- ep[j] + ep[j-1]
  }}
  pta[which(pta$dyad == i), 13] <- ep
  colnames(pta)[13] <- "ep_sum"
}

# Cleaning and Dyads in both directions
colnames(pta)[1:4] <- c("pta_id", "C1", "C2", "year")
pta2 <- pta
colnames(pta2)[2:3] <- c("C2", "C1")
pta <- rbind(pta, pta2); rm(pta2, pta_dep, pta_env)

# Merge with main data from Eora --------------------------------------------
ptamain <- merge(eora_d_all, pta, all = T)

# Filling up empty rows with latest PTA-status
library(magrittr)
ptamain <- ptamain %>%
  dplyr::mutate(newid = as.numeric(factor(paste(pmin(ptamain$C1, ptamain$C2), 
                                                pmax(ptamain$C1, ptamain$C2),
                                                sep="_"))))

# Filling up - ATTENTION - THIS TAKES TIME!
for (i in sort(unique(ptamain$newid))) {
  col_n <- which(colnames(ptamain) %in% c("pta_sum", "depth_sum", "ep_sum"))
  ptamain[which(ptamain$newid == i), col_n] <- 
    zoo::na.locf(ptamain[which(ptamain$newid == i), col_n], na.rm = F)
  
  # Progress bar
  setTxtProgressBar(txtProgressBar(min = min(unique(ptamain$newid)) - 1,
                                   max = max(unique(ptamain$newid)), style = 3), i)
}

# ptamain <- ptamain[!is.na(ptamain$cf_cc),]
ptamain[which(is.na(ptamain[col_n])), col_n] <- 0
ptamain <- ptamain[ptamain$year %in% 1989:2015,]

ptamain_eora <- ptamain
ptamain_eora <- ptamain_eora[-which(duplicated(paste0(ptamain_eora$C1,
                           ptamain_eora$C2, ptamain_eora$year), fromLast = T)),]

# Merge with main data from Exiobase ----------------------------------------
ptamain <- merge(exio_d_all, pta, all = T)

# Filling up empty rows with latest PTA-status
library(magrittr)
ptamain <- ptamain %>%
  dplyr::mutate(newid = as.numeric(factor(paste(pmin(ptamain$C1, ptamain$C2), 
                                                pmax(ptamain$C1, ptamain$C2),
                                                sep="_"))))

# Filling up - ATTENTION - THIS TAKES TIME!
for (i in sort(unique(ptamain$newid))) {
  col_n <- which(colnames(ptamain) %in% c("pta_sum", "depth_sum", "ep_sum"))
  ptamain[which(ptamain$newid == i), col_n] <- 
    zoo::na.locf(ptamain[which(ptamain$newid == i), col_n], na.rm = F)
  
  # Progress bar
  setTxtProgressBar(txtProgressBar(min = min(unique(ptamain$newid)) - 1,
                                   max = max(unique(ptamain$newid)), style = 3), i)
}

# ptamain <- ptamain[!is.na(ptamain$cf_cc),]
ptamain[which(is.na(ptamain[col_n])), col_n] <- 0
ptamain <- ptamain[ptamain$year %in% 1994:2015,]

ptamain_exio <- ptamain
ptamain_exio <- ptamain_exio[-which(duplicated(paste0(ptamain_exio$C1,
                           ptamain_exio$C2, ptamain_exio$year), fromLast = T)),]


# Note:
# include pxp and s



############################################################################
## ENVIRONMENTAL TREATIES ##################################################
iea <- read.csv2("C:/Data/IEA/nccrd/1_data/iea-database/iea-dataset/IEA_Membership.csv")
iea$country <- countrycode::countrycode(iea$country, origin = "country.name", destination = "iso3c")
iea <- iea[!is.na(iea$country),]
iea <- iea[iea$agreement_action == "Entry Into Force",]
iea <- iea[order(iea$year), c(1, 3, 5)]

# Remove duplicates
iea$dup <- paste0(iea$iea_id., iea$country, iea$year)
iea <- iea[-which(duplicated(iea$dup)),]

# Count treaties
for (i in unique(iea$country)) {
  iea[which(iea$country == i), 5] <- seq(1:length(which(iea$country == i)))
}

# Remove multiple entries for same year
iea$dup <- paste0(iea$country, iea$year)
iea <- iea[-which(duplicated(iea$dup, fromLast = T)),]

iea <- iea[, c(2, 3, 5)]
iea2 <- iea
colnames(iea) <- c("C1", "year", "iea_sum_C1")
colnames(iea2) <- c("C2", "year", "iea_sum_C2")

# merge
ieamain_eora <- merge(eora_d_all, iea2, all = T)
ieamain_eora <- merge(ieamain_eora, iea, all = T)
ieamain_eora[which(is.na(ieamain_eora$iea_sum_C1)),]$iea_sum_C1 <- 0
ieamain_eora[which(is.na(ieamain_eora$iea_sum_C2)),]$iea_sum_C2 <- 0
# ieamain_eora <- ieamain_eora[which(!is.na(ieamain_eora$cf_cc)),]
ieamain_eora$iea_diff <- ieamain_eora$iea_sum_C2 - ieamain_eora$iea_sum_C1

ieamain_exio <- merge(exio_d_all, iea2, all = T)
ieamain_exio <- merge(ieamain_exio, iea, all = T)
ieamain_exio[which(is.na(ieamain_exio$iea_sum_C1)),]$iea_sum_C1 <- 0
ieamain_exio[which(is.na(ieamain_exio$iea_sum_C2)),]$iea_sum_C2 <- 0
# ieamain_exio <- ieamain_exio[which(!is.na(ieamain_exio$cf_cc)),]
ieamain_exio$iea_diff <- ieamain_exio$iea_sum_C2 - ieamain_exio$iea_sum_C1

# Note:
# include pxp and s


############################################################################
## MERGE AND SAVE ##########################################################

# Eora --------------------------------------------------------------------
main_eora <- merge(distmain_eora, wdimain_eora, all = T)
main_eora <- merge(main_eora, regmain_eora, all = T)
main_eora <- merge(main_eora, ptamain_eora, all = T)
main_eora <- merge(main_eora, ieamain_eora, all = T)
# rm(distmain_eora, wdimain_eora, regmain_eora, ptamain_eora, ieamain_eora)

# Exiobase ----------------------------------------------------------------
main_exio <- merge(distmain_exio, wdimain_exio, all = T)
main_exio <- merge(main_exio, regmain_exio, all = T)
main_exio <- merge(main_exio, ptamain_exio, all = T)
main_exio <- merge(main_exio, ieamain_exio, all = T)
# rm(distmain_exio, wdimain_exio, regmain_exio, ptamain_exio, ieamain_exio)

main_exio_s <- merge(distmain_exio_s, wdimain_exio_s, all = T)
main_exio_s <- merge(main_exio_s, regmain_exio_s, all = T)

main_exio_pxp <- merge(distmain_exio_pxp, wdimain_exio_pxp, all = T)
main_exio_pxp <- merge(main_exio_pxp, regmain_exio_pxp, all = T)


# Create List -------------------------------------------------------------
main <- list(eora = main_eora, exio = main_exio)
main_long <- list(exio_s = main_exio_s, exio_pxp = main_exio_pxp)
# rm(main_exio, main_eora)

# Creating dyads ----------------------------------------------------------
main <- lapply(main, function(x) {x$dyad <- paste0(x$C1, "_", x$C2); x})
main <- lapply(main, function(x) {x$dyad <- as.factor(x$dyad); x})

main_long <- lapply(main_long, function(x) {x$dyad <- paste0(x$C1, "_", x$C2); x})
main_long <- lapply(main_long, function(x) {x$dyad <- as.factor(x$dyad); x})

# Lag ---------------------------------------------------------------------
library(data.table)
nm1 <- colnames(main$eora)[-c(grep("year", colnames(main$eora)),
                              grep("cf_", colnames(main$eora)))]
main <- lapply(main, function(x) {x <- as.data.table(x)[, (nm1) := shift(.SD),
                                                        by = dyad, .SDcols=nm1]; x})

nm1 <- colnames(main_long$exio_s)[-c(grep("year", colnames(main_long$exio_s)),
                              grep("cf_", colnames(main_long$exio_s)))]
main_long <- lapply(main_long, function(x) {x <- as.data.table(x)[, (nm1) := shift(.SD),
                                                        by = dyad, .SDcols=nm1]; x})

# Cleaning empty rows -----------------------------------------------------
main <- lapply(main, function(x){x[!is.na(x$cf_cc),]})

main_long <- lapply(main_long, function(x){x[!is.na(x$cf_cc),]})

# Scaling -----------------------------------------------------------------
main$exio$cf_cc <- main$exio$cf_bl*100000000
main$exio$cf_cc <- main$exio$cf_cc/10
main$exio$cf_en <- main$exio$cf_en/1000
main$exio$cf_lu <- main$exio$cf_lu/10
main$exio$cf_mf <- main$exio$cf_mf/100
main$eora$cf_mf <- main$eora$cf_mf/10000
main$eora$cf_en <- main$eora$cf_en/10





# other -------------------------------------------------------------------
# main$dem_polity <- as.factor(main$dem_polity)
# main$dem_world <- as.factor(main$dem_world)


# Reverse Corruption into right direction ---------------------------------
main <- lapply(main, function(x) {x$cor <- x$cor * -1; x})

# Save --------------------------------------------------------------------
save(main, file = "o_main_world.RData")
save(main_long, file = "o_main_world_long.RData")



# Create List for countries -----------------------------------------------
main_c <- list(eora_d = main$eora[grep("DEU", main$eora$dyad),], 
               eora_c = main$eora[grep("CHE", main$eora$dyad),], 
               eora_a = main$eora[grep("AUT", main$eora$dyad),], 
               exio_d = main$exio[grep("DEU", main$exio$dyad),],
               exio_c = main$exio[grep("CHE", main$exio$dyad),],
               exio_a = main$exio[grep("AUT", main$exio$dyad),])

# Save --------------------------------------------------------------------
save(main_c, file = "o_main_countries.RData")



###########################################################################|
##############################   N O T E S   ##############################|
###########################################################################|


# Formerly in cleaning section



# # Extract Final Consumption
# exio_d_cc <- exio_d_cc[which(exio_d_cc$S2 == "Final consumption expenditure by households"),]
# exio_d_cc <- exio_d_cc[, -5]
# 
# # Delete Rest-of-World-Rows
# exio_d_cc <- exio_d_cc[-which(exio_d_cc$C2 %in% c("WA", "WE", "WF", "WL", "WM")),]
# exio_d_cc <- exio_d_cc[-which(exio_d_cc$C1 %in% c("WWA", "WWE", "WWF", "WWL", "WWM")),]
# 
# # Change Country Codes
# exio_d_cc$C2 <- countrycode::countrycode(exio_d_cc$C2, origin = "iso2c",
#                                          destination = "iso3c")
# 
# # Aggregate Values for Each Country
# exio_d_cc <- aggregate(exio_d_cc$value, by = list(exio_d_cc$year, exio_d_cc$C1,
#                                                   exio_d_cc$C2), FUN = sum)
# 
# # Change Variable Names and Only Exclude Domestic Production-Consumption
# colnames(exio_d_cc) <- c("year", "C1", "C2", "value")
# exio_d_cc <- exio_d_cc[-which(exio_d_cc$C1 == exio_d_cc$C2),]




# # Extract Final Consumption
# eora_d_cc <- eora_d_cc[which(eora_d_cc$S2 == "Household final consumption P.3h"),]
# eora_d_cc <- eora_d_cc[, -5]
# 
# # Delete Rest-of-World-Rows
# eora_d_cc <- eora_d_cc[-which(eora_d_cc$C2 == "ROW"),]
# eora_d_cc <- eora_d_cc[-which(eora_d_cc$C1 == "ROW"),]
# 
# # Aggregate Values for Each Country
# eora_d_cc <- aggregate(eora_d_cc$value, by = list(eora_d_cc$year, eora_d_cc$C1,
#                                                   eora_d_cc$C2), FUN = sum)
# 
# # Change Variable Names and Only Exclude Domestic Production-Consumption
# colnames(eora_d_cc) <- c("year", "C1", "C2", "value")
# eora_d_cc <- eora_d_cc[-which(eora_d_cc$C1 == eora_d_cc$C2),]


###############################
# Formerly in Democracy section

# vdem$country <- countrycode::countrycode(vdem$country_id, origin = "vdem",
#                                          destination = "iso3n") # normal country text id more accurate
# unique(vdem[which(vdem$country_id == 23),]$country_name)

# Differences
# vdem[14:18] <- vdem[9:13] - vdem[3:7] # check difference
# l <- (length(vdem)-3)/2
# vdem[3:(l+2)] <- vdem[(l+4):length(vdem)] - vdem[3:(l+2)] # from consumer perspective
# vdem[(ncol(vdem)+1):(ncol(vdem)+length(lab))] <- NULL

# vdem <- vdem[-c((l+4):length(vdem))]
# colnames(vdem)[2:length(vdem)] <- c("C1", "dem_world", "dem_elec", "dem_lib",
#                          "eq_res", "dem_polity", "urban", "res", "cor", "educ", "C2")
# rm(l)

#######################
# Fromerly PTA section


# -openness
# -capital controls
# -foreign direct investment
# -ptas
# -currency unions (gravdata)
# -membership in WTO or GATT (gravdata)/ or other institutions
# -dispute in wto
# -IPR

############################################
# Formerly Section on Environmental Treaties
# - WTO membership
# - WTO disputes with other country
# - Influence on WTO funding
# - Commitment with environmental agreements

####################
# Formerly Geography
# - common border
# -access to sea


############################################################################
## CULTURE #################################################################

# - common values
# - common language
# - colonial heritage

############################################################################
## TECHNOLOGY ##############################################################

# -technology difference