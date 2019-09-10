###########################################################################|
#####################   V I Z U A L I Z A T I O N S   #####################|
###########################################################################|

# Load
load("o_main_world.RData")
load("o_exio_d_all.RData")
load("o_eora_d_all.RData")

############################################################################
## FIGURE 1 - ACROSS THE WORLD ENVIRONMENTAL BURDEN SHIFTING ###############
prepare <- function(cf, threshold, drop.other = F){
  cf <- which(c(0, 0, 0, gsub(".*_", "", colnames(exio_d_all)[4:length(exio_d_all)])) == cf)
  exio_d_all[exio_d_all$year == 2015, c(2, 3, cf)] -> exio_d
  exio_d <- exio_d[exio_d$C1 != "E hh",]
  m <- reshape2::melt(exio_d, id.var = c("C1", "C2"))
  
  what <- rbind(aggregate(m$value, by = list(m$C1), FUN=sum), 
                aggregate(m$value, by = list(m$C2), FUN=sum))
  keep <- unique(what[what$x > threshold,]$Group.1 )
  other <- unique(what[what$x < threshold,]$Group.1)
  
  # Define threshold for countries to be included
  if (drop.other | rlang::is_empty(other)) {
    m <- m[which(m$C1 %in% keep & m$C2 %in% keep),]
  } else {
    m[which(m$C1 %in% other),]$C1 <- "other"
    m[which(m$C2 %in% other),]$C2 <- "other"
  } 
  
  m <- as.matrix(with(m, tapply(value, list(C1, C2), FUN = sum)))
  dimnames(m) <- list(production = colnames(m),
                      demand = colnames(m))
  
  # Add color specification
  mcol <- data.frame(country = c(unique(exio_d_all$C1), "other"),
                     color = rainbow(length(unique(exio_d_all$C1)) + 1),
                     stringsAsFactors = F)
  set.seed(100)
  mcol <- transform(mcol, color = sample(color))
  mcol[46, 2] <- "#A8A8A8"
  mcol <- mcol[match(colnames(m), mcol$country),]$color

  return(list(m = m, mcol = mcol))
}

# Exio BW
pdf("~/MA-Thesis/Thesis/figures/circle_world.pdf", width = 10, height = 14)
layout(matrix(c(0,0, rep(1, 4), 0,0, rep(2, 4), rep(3, 4), rep(4, 4),
                rep(5, 4), rep(6, 4), rep(7, 4)),  4, byrow = TRUE))
m <- prepare("bw", 500)
circlize::chordDiagramFromMatrix(m$m, directional = 1, m$mcol) # annotationTrack = c("name","grid")
title(main = "Blue Water Footprint")

# Exio BL
m <- prepare("bl", 0.00005)
circlize::chordDiagramFromMatrix(m$m, directional = 1, m$mcol)
title(main = "Biodiversity Loss")

# Exio CC
m <- prepare("cc", 50000000000)
circlize::chordDiagramFromMatrix(m$m, directional = 1, m$mcol)
title(main = "Climate Change Impact")

# Exio EN
m <- prepare("en", 3000000)
circlize::chordDiagramFromMatrix(m$m, directional = 1, m$mcol)
title(main = "Energy Footprint")

# Exio LU
m <- prepare("lu", 50000)
circlize::chordDiagramFromMatrix(m$m, directional = 1, m$mcol)
title(main = "Land Use Footprint")

# Exio MF
m <- prepare("mf", 70000)
circlize::chordDiagramFromMatrix(m$m, directional = 1, m$mcol)
title(main = "Material Footprint")

# Exio WS
m <- prepare("ws", 7000)
circlize::chordDiagramFromMatrix(m$m, directional = 1, m$mcol)
title(main = "Water Stress")
dev.off() # End




############################################################################
## FIGURE 2 - DEMOCRACY AND ENVIRONMENTAL FOOTPRINTS #######################
prep_eo <- function(cf, year = 2015, relative = FALSE){
  cf <- which(c(0, 0, 0, gsub(".*_", "", colnames(main$eora)[4:length(main$eora)])) == cf)
  eora_d <- main$eora[main$eora$year == year, c(1, 2, cf)]
  eora_d <- eora_d[eora_d$C1 != "E hh",]

  # replace countries with regime levels
  eora_d$C1 <- vdem$regime[match(eora_d$C1, vdem$country)]
  eora_d$C2 <- vdem$regime[match(eora_d$C2, vdem$country)]
  eora_d <- eora_d[!is.na(eora_d$C1) & !is.na(eora_d$C2),]
  
  eora_d[eora_d$C1 %in% 8:9,]$C1 <- "DEMOCRACY"
  eora_d[eora_d$C2 %in% 8:9,]$C2 <- "DEMOCRACY"
  eora_d[eora_d$C1 %in% 6:7,]$C1 <- "HYBRID DEMOCRACY"
  eora_d[eora_d$C2 %in% 6:7,]$C2 <- "HYBRID DEMOCRACY"
  eora_d[eora_d$C1 %in% 4:5,]$C1 <- "HYBRID"
  eora_d[eora_d$C2 %in% 4:5,]$C2 <- "HYBRID"
  eora_d[eora_d$C1 %in% 2:3,]$C1 <- "HYBRID AUTOCRACY"
  eora_d[eora_d$C2 %in% 2:3,]$C2 <- "HYBRID AUTOCRACY"
  eora_d[eora_d$C1 %in% 0:1,]$C1 <- "AUTOCRACY"
  eora_d[eora_d$C2 %in% 0:1,]$C2 <- "AUTOCRACY"

  m <- reshape2::melt(eora_d, id.var = c("C1", "C2"))
  m <- as.matrix(with(m, tapply(value, list(C1, C2), FUN = sum)))
  m <- m[c(2,5,3,4,1),c(2,5,3,4,1)]
  dimnames(m) <- list(production = colnames(m),
                      demand = colnames(m))
  
  if (relative == TRUE) {
    mcount <- table(paste0(eora_d$C1, "_", eora_d$C2))[c(7,10,8,9,6,17,20,18,19,16,22,25,23,24,21,12,15,13,14,11,2,5,3,4,1)]
    mcount <- matrix(mcount, nrow = 5)
    m <- m / mcount
  }
  
  return(m)
}

# Load VDEM before using pre_eo
vdem <- readRDS("C:/Data/VDEM/V-Dem-CY-Full+Others-v9.rds")
vdem <- subset(vdem, select = c(country_text_id, year, v2x_regime_amb,
                                v2x_polyarchy, v2x_libdem, v2xeg_eqdr,
                                e_polity2, e_miurbani, e_total_resources_income_pc,
                                e_ti_cpi, e_peaveduc))
vdem <- subset(vdem, year %in% 2015)
vdem <- vdem[c(1, 3)]
colnames(vdem) <- c("country", "regime")

col_eor <- c("#66A61E", "#E6AB02", "#E7298A", "#1E90FF", "#2B2B2B")


# Figure 2 - BW
pdf("~/MA-Thesis/Thesis/figures/circle_dem.pdf", width = 11, height = 5)
par(mfrow = c(1,2))
circlize::chordDiagramFromMatrix(prep_eo("bw"), directional = 1, col_eor)
title(main = "a) Absolute Shift of Blue Water Footprint")
circlize::chordDiagramFromMatrix(prep_eo("bw", relative = T), directional = 1, col_eor)
title(main = "b) Relative Shift of Blue Water Footprint by Group Size")
dev.off()


# Appendix
pdf("~/MA-Thesis/Thesis/figures/circle_dem_app.pdf", width = 8, height = 9)
par(mfrow = c(2,2))
circlize::chordDiagramFromMatrix(prep_eo("cc"), directional = 1, col_eor)
title(main = "Shift of Climate Change Impact")
circlize::chordDiagramFromMatrix(prep_eo("en"), directional = 1, col_eor)
title(main = "Shift of Energy Use Footprint")
circlize::chordDiagramFromMatrix(prep_eo("lu"), directional = 1, col_eor)
title(main = "Shift of Land Use Footprint")
circlize::chordDiagramFromMatrix(prep_eo("mf"), directional = 1, col_eor)
title(main = "Shift of Material Footprint")
dev.off()


############################################################################
## Figure 3 - Democracy and Footprints over time ###########################
load("o_main_world.RData")

stack_bar <- function(cf, title, axis) {
  
  # Remove NAs
  world_stack <- subset(main$eora, !is.na(eval(parse(text = cf))) & !is.na(dem_world))
  
  # Subset Established democracies
  world_stack <- subset(world_stack, dem_world_C2 == 9)
  world_stack <- aggregate(eval(parse(text=paste0("world_stack$", cf))), by = list(world_stack$dem_world_C1,
                                                                                   world_stack$year), FUN=sum)
  colnames(world_stack) <- c("dem_diff", "year", "value")
  
  # Make Precentages etc.
  library(data.table)
  library(scales)
  dt <- setDT(world_stack)[,list(value = value), by = .(year,-dem_diff)][,
                          list(dem_diff = dem_diff, value = value, percent_fmt = paste0(formatC(value*100/sum(value),
                          digits = 2), "%"), percent_num = value/sum(value)), by = year]
  dt$dem_diff <- as.factor(dt$dem_diff*-1)
  
  # Plot
  library(ggplot2)
  return(
    ggplot(data=dt, aes(x=year, y=percent_num, fill=dem_diff)) +   
      geom_bar(position=position_fill(reverse=TRUE), stat = "identity") +
      geom_text(aes(label = percent_fmt),position = position_stack(vjust = 0.5, reverse=TRUE), size = 2.5, colour = "white") +
      scale_y_continuous(labels =percent_format(), expand = c(0, 0)) +
      scale_x_continuous(breaks= 1990:2015, expand = c(0, 0.5)) +
      guides(fill = guide_legend(title="Democracy Level", reverse=TRUE)) +
      scale_fill_hue(c=65, l=60, labels = c("0 = AUT", as.character(1:8), "9 = DEM")) +
      theme_linedraw() +
      labs(x = "Year", y = axis) +
      ggtitle(title) +
      theme(panel.border = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

# Figure ...
pdf("~/MA-Thesis/Thesis/figures/dem_time_world.pdf", width = 13, height = 6)
stack_bar("cf_cc","Carbon Footprint Offloading of Established Democracies from 1990 to 2015 \n",
          "Share of Offloaded Carbon Footprint")
dev.off() # End



# Appendix ----------------------------------------------------------------
library(gridExtra)
pdf("~/MA-Thesis/Thesis/figures/dem_time_world_app.pdf", width = 13, height = 17)
p1 <- stack_bar("cf_mf", "a) Material Footprint Offloading of Established Democracies from 1990 to 2015 \n",
          "Share of Offloaded Material Footprint")
p2 <-stack_bar("cf_en", "b) Energy Footprint Offloading of Established Democracies from 1990 to 2015 \n",
          "Share of Offloaded Material Footprint")
p3 <-stack_bar("cf_bw", "c) Blue Water Footprint Offloading of Established Democracies from 1990 to 2015 \n",
          "Share of Offloaded Blue Water Footprint")
p4 <-stack_bar("cf_lu", "d) Land Use Footprint Offloading of Established Democracies from 1990 to 2015 \n",
          "Share of Offloaded Land Use Footprint")
grid.arrange(p1, p2, p3, p4, nrow=4, ncol=1)
dev.off() # End




############################################################################
## Figure 4 - Sectors and Products #########################################
load("o_main_world_long.RData")


# Sectors -----------------------------------------------------------------
tree <- main_long$exio_s[main_long$exio_s$S1 != "Total",] 
# tree <- subset(tree, year == 2015)

# tree <- tree[tree$C2 == "DEU",]
tree1 <- tree[tree$dem_world_C1 == 9,]
tree2 <- tree[tree$dem_world_C1 == 0,]
# tree1 <- tree[tree$dem_world_C2 == 9,]
# tree2 <- tree[tree$dem_world_C2 == 0,]

library(treemap)
library(grid)
pdf("~/MA-Thesis/Thesis/figures/sectors_bw.pdf", width = 13, height = 9)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
vp <- viewport(layout.pos.col=1, layout.pos.row=1)
pushViewport(vp)
treemap(tree1, index="S1", vSize="cf_cc", type="index",
        title = "a) EBO by sectors' blue water footprint for established democracies\n",vp=vp)
popViewport()
vp <- viewport(layout.pos.col=2, layout.pos.row=1)
pushViewport(vp)
treemap(tree2, index="S1", vSize="cf_cc", type="index",
        title = "b) EBO by sectors' blue water footprint for pure autocracies\n",vp=vp)
popViewport()
dev.off()




# Products ----------------------------------------------------------------
tree <- main_long$exio_pxp[main_long$exio_pxp$S1 != "Total",] 
# tree <- subset(tree, year == 2011)

# Offloaded by Regimes to Regimes
tree1 <- tree[tree$dem_world_C1 >= 8 & tree$dem_world_C2 >= 8,] # Dem to Dem
tree2 <- tree[tree$dem_world_C1 <= 3 & tree$dem_world_C2 >= 8,] # Dem to Auth

library(treemap)
library(grid)
pdf("~/MA-Thesis/Thesis/figures/products_byto.pdf", width = 11, height = 7)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
vp <- viewport(layout.pos.col=1, layout.pos.row=1)
pushViewport(vp)
treemap(tree1, index="S1", vSize="cf_bw", type="index",
        title = "a) EBO by democracies to democracies\n",vp=vp)
popViewport()
vp <- viewport(layout.pos.col=2, layout.pos.row=1)
pushViewport(vp)
treemap(tree2, index="S1", vSize="cf_bw", type="index",
        title = "b) EBO by democracies to autocracies\n",vp=vp)
popViewport()
dev.off()


library(treemap)
library(grid)
pdf("~/MA-Thesis/Thesis/figures/products_byto.pdf", width = 11, height = 7)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
vp <- viewport(layout.pos.col=1, layout.pos.row=1)
pushViewport(vp)
treemap(tree1, index="S1", vSize="cf_cc", type="index",
        title = "a) EBO by democracies to democracies\n",vp=vp)
popViewport()
vp <- viewport(layout.pos.col=2, layout.pos.row=1)
pushViewport(vp)
treemap(tree2, index="S1", vSize="cf_cc", type="index",
        title = "b) EBO by democracies to autocracies\n",vp=vp)
popViewport()
dev.off()


# Offloaded by Regimes
tree1 <- tree[tree$dem_world_C2 == 9,]
tree2 <- tree[tree$dem_world_C2 == 0,]

library(treemap)
library(grid)
pdf("~/MA-Thesis/Thesis/figures/products_by.pdf", width = 13, height = 9)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
vp <- viewport(layout.pos.col=1, layout.pos.row=1)
pushViewport(vp)
treemap(tree1, index="S1", vSize="cf_bw", type="index",
        title = "a) Products' blue water footprint outsourced by established democracies\n",vp=vp)
popViewport()
vp <- viewport(layout.pos.col=2, layout.pos.row=1)
pushViewport(vp)
treemap(tree2, index="S1", vSize="cf_bw", type="index",
        title = "b) Products' blue water footprint outsourced by pure autocracies\n",vp=vp)
popViewport()
dev.off()


# Offloaded to Regimes
tree1 <- tree[tree$dem_world_C1 == 9,]
tree2 <- tree[tree$dem_world_C1 == 0,]

library(treemap)
library(grid)
pdf("~/MA-Thesis/Thesis/figures/products_to.pdf", width = 13, height = 9)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
vp <- viewport(layout.pos.col=1, layout.pos.row=1)
pushViewport(vp)
treemap(tree1, index="S1", vSize="cf_bw", type="index",
        title = "a) Products' blue water footprint outsourced to established democracies\n",vp=vp)
popViewport()
vp <- viewport(layout.pos.col=2, layout.pos.row=1)
pushViewport(vp)
treemap(tree2, index="S1", vSize="cf_bw", type="index",
        title = "b) Products' blue water footprint outsourced to pure autocracies\n",vp=vp)
popViewport()
dev.off()



# Germany -----------------------------------------------------------------
tree <- tree[tree$C2 == "DEU",]
tree1 <- tree[tree$dem_world_C1 == 9,]
tree2 <- tree[tree$dem_world_C1 == 0,]
# tree1 <- tree[tree$dem_world_C2 == 9,]
# tree2 <- tree[tree$dem_world_C2 == 0,]


library(treemap)
library(grid)
pdf("~/MA-Thesis/Thesis/figures/products_ger.pdf", width = 13, height = 9)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
vp <- viewport(layout.pos.col=1, layout.pos.row=1)
pushViewport(vp)
treemap(tree1, index="S1", vSize="cf_cc", type="index",
        title = "a) EBO by products' CCI for established democracies\n",vp=vp)
popViewport()
vp <- viewport(layout.pos.col=2, layout.pos.row=1)
pushViewport(vp)
treemap(tree2, index="S1", vSize="cf_cc", type="index",
        title = "b) EBO by products' CCI for pure autocracies\n",vp=vp)
popViewport()
dev.off()


# Bar Chart
tree3 <- subset(tree2, select = c(S1, cf_bw))
tree3$S1 <- as.factor(tree3$S1)
tree3 <- as.data.frame(tree3)
tree3 <- aggregate(tree3[2], by= list(tree3$S1), FUN = "sum")
tree3 <- subset(tree3, cf_bw > 0)
barplot(tree3$cf_bw, names=tree3$Group.1, las=2)



############################################################################
## Figure 5 - Stacked Area Graphs  #########################################
load("~/MA-Thesis/Empirical Analysis/o_main_countries.RData")


# Germany's Offload of CCI
ger <- main_c$eora_d[main_c$eora_d$C2 == "DEU",]
ger <- ger[ger$C1 != "E hh",]
ger$dyad <- paste0(ger$C1, "_", ger$C2)


# ger <- ger[grep("_DEU", ger$dyad),]
agr <- aggregate(ger$dem_world, list(ger$dyad), function(x) {round(mean(x))})
colnames(agr) <- c("dyad", "var")
ger <- merge(ger, agr)
ger2 <- aggregate(ger$cf_cc, list(ger$year, ger$var), sum)


library(ggplot2)
ger2$Group.2 <- as.factor(ger2$Group.2)
ger2$Group.2 <- factor(ger2$Group.2, sort(unique(ger2$Group.2), decreasing = TRUE))
p1 <- ggplot(ger2, aes(x = Group.1, y = x, fill = Group.2)) + 
  geom_area(colour="#7A7A7A", size=.2, alpha=.4) +
  scale_x_continuous(breaks= seq(1990, 2015, 2), expand = c(0, 0.5)) +
  guides(fill = guide_legend(title="Democracy Level")) +
  scale_fill_discrete(labels = c("0 = AUT", as.character(1:8), "9 = DEM")) +
  theme_bw() +
  labs(x = "Year", y = "Offloaded CCI") +
  theme(panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Domestic Production-Consumption
load("~/MA-Thesis/Empirical Analysis/o_eora_d_cc.RData")
ger <- eora_d_cc[eora_d_cc$C1 == "DEU",]
ger <- ger[ger$C2 == "DEU",]
ger <- ger[ger$C1 != "E hh",]
ger$dyad <- paste0(ger$C1, "_", ger$C2)

ger_dups <- as.data.frame(ger)[c("dyad", "year")]
ger <- ger[!duplicated(ger_dups),]

p2 <- ggplot(ger, aes(x=year)) + 
  geom_area(aes(y=value, fill="value")) +
  scale_x_continuous(breaks= seq(1990, 2015, 2), expand = c(0, 0.5)) +
  guides(fill = guide_legend(title="Domestic P-D")) +
  scale_fill_discrete(labels = c("Germany")) +
  theme_bw() +
  labs(x = "Year", y = "Domestic CCI") +
  theme(panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.box.margin=margin(0,13,0,0))


library(gridExtra)
pdf("~/MA-Thesis/Thesis/figures/ger_cc.pdf", width = 13, height = 11)
grid.arrange(p1, p2, nrow=2, ncol=1, layout_matrix = matrix(c(rep(1,9), rep(2,3)), ncol = 3, byrow = T))
dev.off()


# Germany's offload of BW
ger2 <- aggregate(ger$cf_bw, list(ger$year, ger$var), sum)
ger2$Group.2 <- as.factor(ger2$Group.2)
ger2$Group.2 <- factor(ger2$Group.2, sort(unique(ger2$Group.2), decreasing = TRUE))
pdf("~/MA-Thesis/Thesis/figures/ger_bw.pdf", width = 13, height = 10)
ggplot(ger2, aes(x = Group.1, y = x, fill = Group.2)) + 
  geom_area(colour="#7A7A7A", size=.2, alpha=.4) +
  scale_x_continuous(breaks= 1990:2015, expand = c(0, 0.5)) +
  guides(fill = guide_legend(title="Democracy Level")) +
  scale_fill_discrete(labels = c("0 = AUT", as.character(1:8), "9 = DEM")) +
  theme_bw() +
  labs(x = "Year", y = "Offloaded blue water burden") +
  theme(panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()



