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
title(main = "Carbon Footprint")

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
prep_eo <- function(cf, year = 2015){
  cf <- which(c(0, 0, 0, gsub(".*_", "", colnames(eora_d_all)[4:length(eora_d_all)])) == cf)
  eora_d_all[eora_d_all$year == year, c(2, 3, cf)] -> eora_d
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
  # 
  m <- reshape2::melt(eora_d, id.var = c("C1", "C2"))
  m <- as.matrix(with(m, tapply(value, list(C1, C2), FUN = sum)))
  m <- m[c(2,5,3,4,1),c(2,5,3,4,1)]
  dimnames(m) <- list(production = colnames(m),
                      demand = colnames(m))
  
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
pdf("~/MA-Thesis/Thesis/figures/circle_dem.pdf", width = 7, height = 6)
circlize::chordDiagramFromMatrix(prep_eo("bw"), directional = 1, col_eor)
title(main = "Shift of Blue Water Footprint")
dev.off()

# Appendix
pdf("~/MA-Thesis/Thesis/figures/circle_dem_app.pdf", width = 8, height = 9)
par(mfrow = c(2,2))
circlize::chordDiagramFromMatrix(prep_eo("cc"), directional = 1, col_eor)
title(main = "Shift of Carbon Footprint")
circlize::chordDiagramFromMatrix(prep_eo("en"), directional = 1, col_eor)
title(main = "Shift of Energy Usage Footprint")
circlize::chordDiagramFromMatrix(prep_eo("lu"), directional = 1, col_eor)
title(main = "Shift of Land Use Footprint")
circlize::chordDiagramFromMatrix(prep_eo("mf"), directional = 1, col_eor)
title(main = "Shift of Material Footprint")
dev.off()

# Animation
par(mfrow = c(1,1))
setwd("~/MA-Thesis/Thesis/figures/")
library(animation)
saveGIF({
  for (i in 1990:2015) {
    circlize::chordDiagramFromMatrix(prep_eo("bw", i), directional = 1, col_eor)
    title(main = paste0("Shift of Blue Water Footprint - ", i))
  }
},
movie.name = "test.gif", 
interval = 0.5, 
ani.width = 1800,
ani.height = 1800,
ani.res = 400)
setwd("~/MA-Thesis/Empirical Analysis")


# Interactive

prep_eo("cc") -> m
p <- chorddiag::chorddiag(m, groupColors = rainbow(nrow(m)), groupnamePadding = 30,
                          showTicks = FALSE, chordedgeColor = "#DBD8D8",
                          groupedgeColor =  "#969491")
p
htmlwidgets::saveWidget(p, "world_cc_dem_int.html")



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
stack_bar("cf_mf", "Material Footprint Offloading of Established Democracies from 1990 to 2015 \n",
          "Share of Offloaded Material Footprint")
stack_bar("cf_en", "Energy Footprint Offloading of Established Democracies from 1990 to 2015 \n",
          "Share of Offloaded Material Footprint")
stack_bar("cf_bw", "Blue Water Footprint Offloading of Established Democracies from 1990 to 2015 \n",
          "Share of Offloaded Blue Water Footprint")
stack_bar("cf_lu", "Land Use Footprint Offloading of Established Democracies from 1990 to 2015 \n",
          "Share of Offloaded Land Use Footprint")

