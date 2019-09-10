###########################################################################|
##############################   F I N A L   ##############################|
###########################################################################|


## LOAD ####################################################################
load("~/MA-Thesis/Empirical Analysis/o_main_world.RData")
# load("~/MA-Thesis/Empirical Analysis/o_main_world_s.RData")
# load("~/MA-Thesis/Empirical Analysis/o_main_world_pxp.RData")
wd <- "\\\\gess-fs.d.ethz.ch/home$/davidpre/Documents/MA-Thesis/Thesis/"


# subset Income groups ----------------------------------------------------
library(readxl)
group <- read_excel("C:/Data/WorldBank/OGHIST.xls",
                    sheet = "Country Analytical History", skip = 5, na = "..")
group <- group[-c(1:5),]
colnames(group)[1:2] <- c("country", "country_name")
group <- reshape2::melt(group, id.vars = c("country", "country_name"))
colnames(group) <- c("C2","name", "year", "group")
group$year <- as.numeric(as.character(group$year))
main_h <- lapply(main, function(x) {merge(x, group)})
main_h <- lapply(main_h, function(x) {x[which(x$group == "H"),]})
main <- append(main, list(eora_h = main_h$eora,  exio_h = main_h$exio)); rm(main_h)


# subset democracies ------------------------------------------------------
main_d <- lapply(main, function(x) {x[which(x$dem_world_C2 == 9),]})
main <- append(main, list(eora_d = main_d$eora,  exio_d = main_d$exio)); rm(main_d)

###########################################################################|
#############################   L A B E L S   #############################|
###########################################################################|
main <- lapply(main, function(x){ 
  colnames(x)[which(colnames(x) %in% c("depth_sum", "ep_sum", "iea_diff"))] <-
    c("pta", "ptaep", "iea"); x})


###########################################################################|
#############################   M O D E L S   #############################
###########################################################################|

# Formulas ----------------------------------------------------------------

# General Model
f1 <- cf_cc ~ gdppcdiff*dem_elec + pta + ptaep + iea | dyad + year
f2 <- cf_cc ~ gdppcdiff*dem_polity + pta + ptaep + iea + res + cor | dyad + year # + distgroup
f3 <- cf_cc ~ gdppcdiff*dem_world + pta + ptaep + iea + res + cor + educ + cor + res | dyad + year # + distgroup

f3.2 <- cf_bw ~ gdppcdiff*dem_world + pta + ptaep + iea + res + cor + educ + cor + res | dyad + year # + distgroup
f3.3 <- cf_en ~ gdppcdiff*dem_world + pta + ptaep + iea + res + cor + educ + cor + res | dyad + year # + distgroup
f3.4 <- cf_lu ~ gdppcdiff*dem_world + pta + ptaep + iea + res + cor + educ + cor + res | dyad + year # + distgroup
f3.5 <- cf_mf ~ gdppcdiff*dem_world + pta + ptaep + iea + res + cor + educ + cor + res | dyad + year # + distgroup

# Interactions Democracy 
f1.1 <- cf_cc ~ gdppcdiff*dem_elec_C1 + pta + ptaep + iea | dyad + year
f1.2 <- cf_cc ~ dem_elec_C1*gdppcdiff + pta + ptaep + iea | dyad + year
f1.3 <- cf_cc ~ dem_elec_C1*gdppcdiff + pta + ptaep + iea + res + cor + educ | dyad + year


# Interactions PTA, IEA interactions
f1.4 <- cf_cc ~ gdppcdiff*dem_elec + pta*gdppcdiff + ptaep*gdppcdiff + iea*gdppcdiff | dyad + year
f1.5 <- cf_cc ~ gdppcdiff*dem_elec + pta*gdppcdiff + ptaep*gdppcdiff + iea*gdppcdiff + res + cor + educ | dyad + year


# Model Calculation -------------------------------------------------------
library(lfe)
m1 <- lapply(main, function(x){felm(f1, data = x)})
m2 <- lapply(main, function(x){felm(f2, data = x)})
m3 <- lapply(main, function(x){felm(f3, data = x)})

m1.1 <- lapply(main, function(x){felm(f1.1, data = x)})
m1.2 <- lapply(main, function(x){felm(f1.2, data = x)})
m1.3 <- lapply(main, function(x){felm(f1.3, data = x)})
m1.4 <- lapply(main, function(x){felm(f1.4, data = x)})
m1.5 <- lapply(main, function(x){felm(f1.5, data = x)})

m4 <- list(felm(f3, data = main$eora),
           felm(f3.2, data = main$eora),
           felm(f3.3, data = main$eora),
           felm(f3.4, data = main$eora),
           felm(f3.5, data = main$eora))

# Notes
f4 <- cf_cc ~ gdppcdiff*dem_world + res + cor + educ + cor + res | dyad + year + S1 # + distgroup
f4 <- cf_cc ~ gdppcdiff*dem_world + res + cor + educ + cor + res | dyad + year + distgroup
m5 <- lapply(main_long, function(x){felm(f4, data = x)})

# Model Results -----------------------------------------------------------
summary(m1$eora)
summary(m2$eora)
summary(m3$eora)
summary(m1$exio)
summary(m2$exio)
summary(m3$exio)


# Notes
# - Interaction can be used for fixed effects (distance will not be used)
# - dist is shown when using only time effect


# # Which model should be picked
# form <- value ~ dist + colony + gdppcdiff + comlang_ethno
# fixed <- plm(form, data = plmdf, model = "within", effect = "twoways")
# pooled <- plm(form, data = plmdf, model = "pooling")
# random <- plm(form, data = plmdf, model = "random")
# plmtest(pooled)
# pFtest(fixed, pooled)
# phtest(random, fixed)
# # -> use fixed effects


###########################################################################|
#############################   T A B L E S   #############################
###########################################################################|
labsf <- function(...){
  x <- capture.output(stargazer::stargazer(..., type="text", table.layout = "t"))
  y <- gsub(" .*$", "", x)[which(gsub(" .*$", "", x) != "")]
  y <- paste0("\\small{\\textsc{", gsub("_", "", gsub(":", "*", y)), "}}")
  return(y)
}
fstat <- function(...) {
  x <- list(...)
  x1 <- lapply(x, function(x){summary(x)$F.fstat[[1]]}) 
  x1 <- lapply(x1, function(x){round(x, 2)})
  x2 <- lapply(x, function(x){summary(x)$F.fstat[[4]]})
  x2 <- lapply(x2, function(x) if (x >= 0.05) {""} else if (x >= 0.01) {"$^{*}$"}
               else if (x >= 0.005) {"$^{**}$"} else {"$^{***}$"})
  x3 <- lapply(x, function(x){format(summary(x)$N, big.mark = ",")})
  x4 <- lapply(x, function(x){sprintf("%.2f", round(summary(x)$r.squared, 2))})
  x5 <-lapply(x, function(x){sprintf("%.2f", round(summary(x)$adj.r.squared, 2))})
  return(list(c("Observations", unlist(x3)),
              c("R$^{2}$", unlist(x4)),
              c("Adjusted R$^{2}$", unlist(x5)),
              c("F statistic", paste0(x1, x2))))
}
tablewide <- function(string, padver = 0.8, padhor = 0.4){
  x <- readLines(paste0(wd, string))
  y <- readLines(paste0(wd, "tables/temp_table.txt"))
  y[1] <- gsub("\\d.\\d", as.character(padhor), readLines(paste0(wd, "tables/temp_table.txt"))[1])
  y[2] <- gsub("\\d.\\d", as.character(padver), readLines(paste0(wd, "tables/temp_table.txt"))[2])
  x[1:4] <- y[1:4]
  x[7] <- paste0(substr(y[7], 1, 61), nchar(gsub(".*\\}\\}(.+)\\}.*", "\\1", x[7])),
                 "}{@{}", gsub(".*\\}\\}(.+)\\}.*", "\\1", x[7]), "}@{}}")
  x[grep("tabular", x)[2]] <- y[10]
  writeLines(x, paste0(wd, string))
}
table_format<- function(model) {
  model[seq(2, 8, 2)] <- lapply(model[seq(2, 8, 2)], function(x){x$formula <- paste("value2 ~",
                                                                                    as.character(f1[[3]])[c(2,1,3)], collapse = " "); x})
  return(model)
}
m1 <- table_format(m1)
m2 <- table_format(m2)
m3 <- table_format(m3)

m1.1 <- table_format(m1.1)
m1.2 <- table_format(m1.2)
m1.3 <- table_format(m1.3)
m1.4 <- table_format(m1.4)
m1.5 <- table_format(m1.5)


# exio panel cc -----------------------------------------------------------|

# Main World Models
stargazer::stargazer(m1$eora, m2$eora, m3$eora, m1$exio, m2$exio, m3$exio,
                     digits = 2,
                     covariate.labels = labsf(m1$eora, m2$eora, m3$eora, m1$exio, m2$exio, m3$exio),
                     dep.var.labels = c("EBO (Eora)", "EBO (Exiobase)"),
                     star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                     star.char = c("\\odot","*","**", "***"),
                     notes = "$^{\\odot}$p $<$ 0.1; $^{*}$p $<$ 0.05; $^{**}$p $<$ 0.01; $^{***}$p $<$ 0.001",
                     notes.append = F,
                     type = "text",
                     title = "Global effects on offloading CCI.",
                     label = "tab:world",
                     omit.stat = c("n", "rsq", "adj.rsq", "f", "ser"),
                     add.lines = fstat(m1$eora, m2$eora, m3$eora, m1$exio, m2$exio, m3$exio),
                     # keep.stat = c("n", "rsq", "adj.rsq", "f"), # no f-statistic available for felm
                     out = c(paste0(wd, "tables/world.tex"),
                             paste0(substr(wd, 1, 55), "Presentation/assets/world.html")))
tablewide("tables/world.tex", padver = .83, padhor = .4)

# Interaction Effects of PTAs, IEA on GDP (Appendix) 
stargazer::stargazer(m1.4$eora, m1.5$eora, m1.4$exio, m1.5$exio,
                     digits = 2,
                     covariate.labels = labsf(m1.4$eora, m1.5$eora, m1.4$exio, m1.5$exio),
                     dep.var.labels = c("EBO (Eora)", "EBO (Exiobase)"),
                     star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                     star.char = c("\\odot","*","**", "***"),
                     notes = "$^{\\odot}$p $<$ 0.1; $^{*}$p $<$ 0.05; $^{**}$p $<$ 0.01; $^{***}$p $<$ 0.001",
                     notes.append = F,
                     type = "text",
                     title = "Global effects on offloading CCI with interaction effects between PTAs and GDPPCDIFF.",
                     label = "tab:world_app",
                     omit.stat = c("n", "rsq", "adj.rsq", "f", "ser"),
                     add.lines = fstat(m1.4$eora, m1.5$eora, m1.4$exio, m1.5$exio),
                     # keep.stat = c("n", "rsq", "adj.rsq", "f"), # no f-statistic available for felm
                     out = c(paste0(wd, "tables/world_appendix.tex"),
                             paste0(substr(wd, 1, 55), "Presentation/assets/world_appendix.html")))
tablewide("tables/world_appendix.tex", padver = .83, padhor = .4)


# High income Countries
stargazer::stargazer(m1$eora_h, m2$eora_h, m3$eora_h, m1$exio_h, m2$exio_h, m3$exio_h,
                     digits = 2,
                     covariate.labels = labsf(m1$eora_h, m2$eora_h, m3$eora_h, m1$exio_h, m2$exio_h, m3$exio_h),
                     dep.var.labels = c("EBO (Eora)", "EBO (Exiobase)"),
                     star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                     star.char = c("\\odot","*","**", "***"),
                     notes = "$^{\\odot}$p $<$ 0.1; $^{*}$p $<$ 0.05; $^{**}$p $<$ 0.01; $^{***}$p $<$ 0.001",
                     notes.append = F,
                     type = "text",
                     title = "Effects on offloading CCI for high-income countries.",
                     label = "tab:worldhigh",
                     omit.stat = c("n", "rsq", "adj.rsq", "f", "ser"),
                     add.lines = fstat(m1$eora_h, m2$eora_h, m3$eora_h, m1$exio_h, m2$exio_h, m3$exio_h),
                     # keep.stat = c("n", "rsq", "adj.rsq", "f"), # no f-statistic available for felm
                     out = c(paste0(wd, "tables/worldhigh.tex"),
                             paste0(substr(wd, 1, 55), "Presentation/assets/worldhigh.html")))
tablewide("tables/worldhigh.tex", padhor = .25)

# Democratic Countries
stargazer::stargazer(m1.1$eora_d, m1.2$eora_d, m1.3$eora_d,
                     digits = 2,
                     covariate.labels = labsf(m1.1$eora_d, m1.2$eora_d, m1.3$eora_d),
                     dep.var.labels = c("EBO (Eora)"),
                     star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                     star.char = c("\\odot","*","**", "***"),
                     notes = "$^{\\odot}$p $<$ 0.1; $^{*}$p $<$ 0.05; $^{**}$p $<$ 0.01; $^{***}$p $<$ 0.001",
                     notes.append = F,
                     type = "text",
                     title = "Effects on offloading CCI for established democracies.",
                     label = "tab:worlddem",
                     omit.stat = c("n", "rsq", "adj.rsq", "f", "ser"),
                     add.lines = fstat(m1.1$eora_d, m1.2$eora_d, m1.3$eora_d),
                     # keep.stat = c("n", "rsq", "adj.rsq", "f"), # no f-statistic available for felm
                     out = c(paste0(wd, "tables/worlddem.tex"),
                             paste0(substr(wd, 1, 55), "Presentation/assets/worlddem.html")))
tablewide("tables/worlddem.tex", padhor = 1.5)

# Comparing Indicators
stargazer::stargazer(m4[[1]], m4[[2]], m4[[3]], m4[[4]], m4[[5]],
                     digits = 2,
                     covariate.labels = labsf(m4[[1]], m4[[2]], m4[[3]], m4[[4]], m4[[5]]),
                     dep.var.labels = c("CC", "BW", "EN", "LU", "MF"),
                     star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                     star.char = c("\\odot","*","**", "***"),
                     notes = "$^{\\odot}$p $<$ 0.1; $^{*}$p $<$ 0.05; $^{**}$p $<$ 0.01; $^{***}$p $<$ 0.001",
                     notes.append = F,
                     type = "text",
                     title = "Effects on offloading different environmental burdens from Eora.",
                     label = "tab:indicatcomp",
                     omit.stat = c("n", "rsq", "adj.rsq", "f", "ser"),
                     add.lines = fstat(m4[[1]], m4[[2]], m4[[3]], m4[[4]], m4[[5]]),
                     # keep.stat = c("n", "rsq", "adj.rsq", "f"), # no f-statistic available for felm
                     out = c(paste0(wd, "tables/indicators_comp.tex"),
                             paste0(substr(wd, 1, 55), "Presentation/assets/indicators_comp.html")))
tablewide("tables/indicators_comp.tex", padhor = 0.25)



###########################################################################|
##############################   P L O T S   ##############################|
###########################################################################|


# Interaction Effects -----------------------------------------------------
load("~/MA-Thesis/Empirical Analysis/f_interactionplot.RData") # function for interaction plot

# Democracy * GDP
pdf("~/MA-Thesis/Thesis/figures/interaction.pdf", width = 11, height = 11)
par(mfrow=c(2,1))
interaction_plot_continuous(m1$eora,"dem_elec", "gdppcdiff", "gdppcdiff:dem_elec",
                            title = "a) Relationship between DEMELEC and EBO for GDP per capita differences (Eora)",
                            xlabel = "Difference in GDP per capita between dyad partners",
                            ylabel = "Marginal effect of electoral democracy differences on EBO")
interaction_plot_continuous(m1$exio,"dem_elec", "gdppcdiff", "gdppcdiff:dem_elec",
                            title = "b) Relationship between DEMELEC and EBO for GDP per capita differences (Exiobase)",
                            xlabel = "Difference in GDP per capita between dyad partners",
                            ylabel = "Marginal effect of electoral democracy in producer country on EBO")
while (!is.null(dev.list()))  dev.off() # normal dev.off() has not worked properly in this case

# PTAs, IEAs * GDP
pdf("~/MA-Thesis/Thesis/figures/interaction_pta.pdf", width = 11, height = 11)
par(mfrow=c(2,1))
interaction_plot_continuous(m1.4$eora, "ptaep", "gdppcdiff", "gdppcdiff:ptaep",
                            title = "a) Relationship between environmental provisions in PTAs and EBO for GDP per capita differences (Eora)",
                            xlabel = "Difference in GDP per capita between dyad partners",
                            ylabel = "Marginal effect of environmental provisions on EBO")
interaction_plot_continuous(m1.5$eora, "iea", "gdppcdiff", "gdppcdiff:iea",
                            title = "b) Relationship between IEA membership and EBO for GDP per capita differences (Eora)",
                            xlabel = "Difference in GDP per capita between dyad partners",
                            ylabel = "Marginal effect of IEA membership on EBO")
while (!is.null(dev.list()))  dev.off() # normal dev.off() has not worked properly in this case

# Democracy Subset
pdf("~/MA-Thesis/Thesis/figures/interaction_dem.pdf", width = 11, height = 11)
par(mfrow=c(2,1))
interaction_plot_continuous(m1.1$eora_d, "dem_elec_C1", "gdppcdiff", "gdppcdiff:dem_elec_C1",
                            title = "a) Relationship between democracy of producer and EBO for GDP per capita differences (Eora)
                            Subset of only established democracies",
                            xlabel = "Difference in GDP per capita between dyad partners",
                            ylabel = "Marginal effect of electoral democracy level of producer on EBO")
interaction_plot_continuous(m1.2$eora_d, "gdppcdiff", "dem_elec_C1", "dem_elec_C1:gdppcdiff",
                            title = "b) Relationship between GDP per capita and EBO for different levels of democracy (Eora)
                            Subset of only established democracies",
                            xlabel = "Democracy level of producer",
                            ylabel = "Marginal effect of GDP per capita differences on EBO")
while (!is.null(dev.list()))  dev.off() # normal dev.off() has not worked properly in this case


interaction_plot_continuous(m5$exio_s,"dem_world", "gdppcdiff", "gdppcdiff:dem_world",
                            title = "a) Relationship between DEMELEC and EBO for GDP per capita differences (Eora)",
                            xlabel = "Difference in GDP per capita between dyad partners",
                            ylabel = "Marginal effect of electoral democracy differences on EBO")


# Coefficients Plots -------------------------------------------------------

# Comparing the environmental indicators
library(ggplot2)
pdf("~/MA-Thesis/Thesis/figures/indicators_comp.pdf", width = 9, height = 6)
dotwhisker::dwplot(m4, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                   dot_args = list(aes(shape = model))) +
  scale_color_manual(name  ="Env. Impact",
                     values=c("#CC0000", "#006600", "#669999", "#00CCCC", "#660099"),
                     breaks=c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
                     labels = c("CC", "BW", "EN", "LU", "MF")) +
  scale_shape_manual(name  ="Env. Impact",
                     values = c(16, 1, 7, 17, 4),
                     breaks=c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
                     labels = c("CC", "BW", "EN", "LU", "MF")) +
  theme_bw() +
  theme(axis.ticks.y = element_blank()) +
  scale_y_discrete(breaks = row.names(m4[[1]]$coefficients),
    labels=c("GDPPCDIFF", "DEMWORLD", "PTA","PTAEP","IEA","RES","COR","EDUC","GDPPCDIFF*DEMWORLD"))
dev.off() # End
  

# Diagnostics -------------------------------------------------------------

# # Distribution of variables
# pdf(paste0(wd, "graphics/diag1.pdf"), width = 7, height = 5)
# hist(main$eora$dem_elec, main = "Histogram of Electoral Democracy Distribution", xlab = "DEM_ELEC")
# dev.off()
# 
# # Distribution of errors
# qqnorm(residuals(m1$eora), ylab = 'Residuals') #log1p does a better job than sqrt
# qqline(residuals(m1$eora))



###########################################################################|
#############   D E S C R I P T I V E   S T A T I S T I C S   #############
###########################################################################|
library(psych)
tbl_eo <- describe(subset(main$eora, select = c("cf_bw","cf_cc","cf_en","cf_lu","cf_mf",
                                      "gdppcdiff","dem_elec","dem_polity","dem_world",
                                      "pta","ptaep", "iea", "res", "cor", "educ")))
tbl_ex <- describe(subset(main$exio, select = c("cf_bl","cf_bw","cf_cc","cf_en","cf_lu","cf_mf","cf_ws",
                                      "gdppcdiff","dem_elec","dem_polity","dem_world",
                                      "pta","ptaep", "iea", "res", "cor", "educ")))

tabledescr <- function(string, padver = 0.9, padhor = 0.6){
  x <- readLines(paste0(wd, string))
  y <- readLines(paste0(wd, "tables/temp_table.txt"))
  y[1] <- gsub("\\d.\\d", as.character(padhor), readLines(paste0(wd, "tables/temp_table.txt"))[1])
  y[2] <- gsub("\\d.\\d", as.character(padver), readLines(paste0(wd, "tables/temp_table.txt"))[2])
  x[1:2] <- y[1:2]
  x[7] <- paste0(substr(y[7], 1, 61), 6, "}{@{}", substr(x[7], 17, nchar(x[7])), "@{}}")
  x[9] <- paste("\\emph{Variable} ", paste0("& \\emph{", c("Observations", "Mean",
            "Standard Deviation", "Minimum", "Maximum"), "} ", collapse = ""), "\\\\")
  x[grep("tabular", x)[2]] <- y[10]
  x[10] <- "\\hline \\vspace{-0.25cm} \\\\"
  x[8] <- "\\\\ [-1.8ex]\\hline"
  x <- append(x, "\\hline \\\\ [-1.8ex]", after = 8)
  writeLines(x, paste0(wd, string))
}

library(xtable)
rownames(tbl_eo) <- gsub("_", "", gsub("cf_", "", rownames(tbl_eo)))
print(xtable(tbl_eo[2:6], display = c("d","d","f","f","f", "f"), label = "tab:descreora", 
             caption = "Summary statistics for Eora.", align = c(">{\\textsc\\bgroup}l<{\\egroup}", rep("r", 5))),
      caption.placement = "top", label = "tab:descreora", file = paste0(wd, "tables/descr_eora.tex"))
tabledescr("tables/descr_eora.tex")

rownames(tbl_ex) <- gsub("_", "", gsub("cf_", "", rownames(tbl_ex)))
print(xtable(tbl_ex[c(2:4,8:9)], display = c("d","d","f","f","f", "f"), label = "tab:descrexio",
             caption = "Summary statistics for Exiobase.", align = c(">{\\textsc\\bgroup}l<{\\egroup}", rep("r", 5))),
      caption.placement = "top", file = paste0(wd, "tables/descr_exio.tex"))
tabledescr("tables/descr_exio.tex")
temp <- readLines(paste0(wd, "tables/descr_exio.tex"))
temp[12] <- paste(substr(temp[12], 1, 10), paste("&", format(c(tbl_ex[1,3], tbl_ex[1,4], tbl_ex[1,8],
                                tbl_ex[1,9]), digits = 3), " ", collapse = ""), "\\\\")
writeLines(temp, paste0(wd, "tables/descr_exio.tex")); rm(temp)



###########################################################################|
##################   R O B U S T N E S S   C H E C K S   ##################
###########################################################################|

# Exiobase without nowcasted
robust_nowcasted <- subset(main$exio, year %in% 1995:2011)

library(lfe)
m1 <- felm(f1, data = robust_nowcasted)
m2 <- felm(f2, data = robust_nowcasted)
m3 <- felm(f3, data = robust_nowcasted)

# Main World Models
stargazer::stargazer(m1, m2, m3,
                     digits = 2,
                     covariate.labels = labsf(m1, m2, m3),
                     dep.var.labels = c("EBO (Exiobase)"),
                     star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                     star.char = c("\\odot","*","**", "***"),
                     notes = "$^{\\odot}$p $<$ 0.1; $^{*}$p $<$ 0.05; $^{**}$p $<$ 0.01; $^{***}$p $<$ 0.001",
                     notes.append = F,
                     type = "text",
                     title = "Global effects on offloading CCI for Exiobase without nowcasted.",
                     label = "tab:world_robust1",
                     omit.stat = c("n", "rsq", "adj.rsq", "f", "ser"),
                     add.lines = fstat(m1, m2, m3),
                     # keep.stat = c("n", "rsq", "adj.rsq", "f"), # no f-statistic available for felm
                     out = c(paste0(wd, "tables/world_robust1.tex"),
                             paste0(substr(wd, 1, 55), "Presentation/assets/world_robust1.html")))
tablewide("tables/world_robust1.tex", padver = .83, padhor = 1.99)

# Binary Democracy
f1 <- cf_cc ~ gdppcdiff*dem_dd + pta + ptaep + iea | dyad + year
f2 <- cf_cc ~ gdppcdiff*dem_dd + pta + ptaep + iea + res + cor | dyad + year # + distgroup
f3 <- cf_cc ~ gdppcdiff*dem_dd + pta + ptaep + iea + res + cor + educ + cor + res | dyad + year # + distgroup

m1 <- lapply(main, function(x){felm(f1, data = x)})
m2 <- lapply(main, function(x){felm(f2, data = x)})
m3 <- lapply(main, function(x){felm(f3, data = x)})
m1 <- table_format(m1)
m2 <- table_format(m2)
m3 <- table_format(m3)


# Main World Models
stargazer::stargazer(m1$eora, m2$eora, m3$eora, m1$exio, m2$exio, m3$exio,
                     digits = 2,
                     covariate.labels = labsf(m1$eora, m2$eora, m3$eora, m1$exio, m2$exio, m3$exio),
                     dep.var.labels = c("EBO (Eora)", "EBO (Exiobase)"),
                     star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                     star.char = c("\\odot","*","**", "***"),
                     notes = "$^{\\odot}$p $<$ 0.1; $^{*}$p $<$ 0.05; $^{**}$p $<$ 0.01; $^{***}$p $<$ 0.001",
                     notes.append = F,
                     type = "text",
                     title = "Global effects on offloading CCI with democracy as binary variable.",
                     label = "tab:world_robust2",
                     omit.stat = c("n", "rsq", "adj.rsq", "f", "ser"),
                     add.lines = fstat(m1$eora, m2$eora, m3$eora, m1$exio, m2$exio, m3$exio),
                     # keep.stat = c("n", "rsq", "adj.rsq", "f"), # no f-statistic available for felm
                     out = c(paste0(wd, "tables/world_robust2.tex"),
                             paste0(substr(wd, 1, 55), "Presentation/assets/world_robust2.html")))
tablewide("tables/world_robust2.tex", padver = .83, padhor = .4)



# Without China
robust_china <- lapply(main, function(x){x <- subset(x, C1 != "CHN" & C2 != "CHN" ); x})

m1 <- lapply(robust_china, function(x){felm(f1, data = x)})
m2 <- lapply(robust_china, function(x){felm(f2, data = x)})
m3 <- lapply(robust_china, function(x){felm(f3, data = x)})
m1 <- table_format(m1)
m2 <- table_format(m2)
m3 <- table_format(m3)


stargazer::stargazer(m1$eora, m2$eora, m3$eora, m1$exio, m2$exio, m3$exio,
                     digits = 2,
                     covariate.labels = labsf(m1$eora, m2$eora, m3$eora, m1$exio, m2$exio, m3$exio),
                     dep.var.labels = c("EBO (Eora)", "EBO (Exiobase)"),
                     star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
                     star.char = c("\\odot","*","**", "***"),
                     notes = "$^{\\odot}$p $<$ 0.1; $^{*}$p $<$ 0.05; $^{**}$p $<$ 0.01; $^{***}$p $<$ 0.001",
                     notes.append = F,
                     type = "text",
                     title = "Global effects on offloading CCI without China.",
                     label = "tab:world_robust3",
                     omit.stat = c("n", "rsq", "adj.rsq", "f", "ser"),
                     add.lines = fstat(m1$eora, m2$eora, m3$eora, m1$exio, m2$exio, m3$exio),
                     # keep.stat = c("n", "rsq", "adj.rsq", "f"), # no f-statistic available for felm
                     out = c(paste0(wd, "tables/world_robust3.tex"),
                             paste0(substr(wd, 1, 55), "Presentation/assets/world_robust3.html")))
tablewide("tables/world_robust3.tex", padver = .83, padhor = .3)



###########################################################################|
#######################   D I A G N O S T I C S   #########################
###########################################################################|

# Homoscedasticity
# library(gvlma)
# summary(gvlma(m1[[1]]))
# fe <- rep(as.numeric(plm::fixef(m1[[1]])), each = 12)
fe <- rep(as.numeric(getfe(m1[[1]])), each = 21)
preds <- (new.X %*% coef(m1[[1]])) + fe + residuals(m1[[1]])


main1 <- main[[1]]
main1 <- main1[-which(duplicated(main1[c("year", "dyad")])),]
mtest<- plm(value ~ gdppcdiff + pta, data = main1,
            index = c("dyad","year"), model = "within", effect = "twoways")
fe <- rep(as.numeric(fixef(mtest), each = 21))
new.X <- as.matrix(dplyr::select(main1, gdppcdiff, pta))
preds <- (new.X %*% coef(mtest)) + fe + residuals(mtest)
plot(preds, residuals(mtest))


# Plot for FELM
windowsFonts(A = windowsFont("CMU Serif"))
par(family = "A")
plot(m1[[1]]$fitted.values, m1[[1]]$residuals, main="Residuals-Fitted Plot", xlim = c(0, 5000))


  # Rectify Heteroscedasticity
caret::BoxCoxTrans(main[[1]]$value)
rectify <- cbind(main[[1]], value_new = predict(caret::BoxCoxTrans(main[[1]]$value), main[[1]]$value))
rectifytest <- felm(value ~ gdppcdiff*dem_elec + pta | dyad + year, data = rectify)

plot(rectifytest$fitted.values, rectifytest$residuals, main="This plot uses LaTeX font!")






###########################################################################|
############################   T E S T I N G   ############################
###########################################################################|

# Transformations
# logz <- function(x, data){
#   y <- ifelse(d$x != 0, log(d$x), 0)
#   return(y)
# }


## plots ###################################################################
submain <- main[which(main$dyad %in% c("AUS_AUT", "AUS_CAN")),]
submain$id <- as.numeric(submain$dyad)
coplot(value ~ year | id, type = "b", pch=as.character(submain$id), data = submain)

submain <- main[grep("CHN_", main$dyad),]
submain <- main[grep("DEU", main$dyad),]
gplots::plotmeans(value ~ dyad, las=2, data = submain)

ols <- lm(value ~ gdppcdiff, data = submain)
yhat <-ols$fitted
fixed.dum <-lm(value ~ gdppcdiff + factor(dyad) - 1, data = submain)
car::scatterplot(yhat ~ submain$gdppcdiff| submain$dyad, boxplots=FALSE, xlab="x1",
                 ylab="yhat",smooth=FALSE)



## plm #####################################################################
m1 <- plm(value ~ gdppcdiff + dist + colony + comlang_ethno, 
          data = main, index = c("dyad","year"), model = "pooling",
          effect = "twoways")
m2 <- plm(value ~ gdppcdiff + dem_polity + dem_elec + pta, 
          data = main1, index = c("dyad","year"), model = "within",
          effect = "twoways")

main1 <- main1[-which(duplicated(main1[c("year", "dyad")])),]

data("Crime", package = "plm")
crbalt <- plm(lcrmrte ~ lprbarr + lpolpc + lprbconv + lprbpris + lavgsen +
                ldensity + lwcon + lwtuc + lwtrd + lwfir + lwser + lwmfg + lwfed +
                lwsta + lwloc + lpctymle + lpctmin + region + smsa + factor(year)
              | . - lprbarr - lpolpc + ltaxpc + lmix,
              data = Crime, model = "random", inst.method = "baltagi")



## R-Squared ###############################################################
sst <- with(main, sum((value - mean(value))^2))
m1b.sse <- t(residuals(m1b)) %*% residuals(m1b)
(sst - m1b.sse) / sst
m1b.r2 <- (sst - m1b.sse) / sst
N <- dim(main)[1]
1 - (1 - m1b.r2)*((N - 1)/(N - length(coef(m1b)) - 1))


## texreg ##################################################################
texreg::screenreg(m1b, stars = c(0.001, 0.01, 0.05, 0.1))
texreg::texreg(m1b, stars = c(0.001, 0.01, 0.05, 0.1), 
               out = "C:/Users/Nutzer/Documents/Studium Salzburg/MA-Thesis/Thesis/tables/pan_exio2")


## other dem differences #########################################################

main$eora$dem_world <- 0
main$eora[which(main$eora$dem_world_C2 %in% 0:1 &
                   main$eora$dem_world_C2 %in% 0:1),]$dem_world <- 1
main$eora[which(main$eora$dem_world_C2 %in% 8:9 &
                   main$eora$dem_world_C2 %in% 8:9),]$dem_world <- 2
main$eora$dem_world <- as.factor(main$eora$dem_world)
levels(main$eora$dem_world) <- c("nopeace", "autpeace", "dempeace")
