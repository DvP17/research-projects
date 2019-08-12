###########################################################################|
###################   I N S T A L L   P A C K A G E S   ###################|
###########################################################################|

# - R-version used: 3.6.1
# - Packages installed on: 7/26/2019 


############################################################################
## packages ################################################################
install.packages("countrycode")
install.packages("WDI")
install.packages("readxl")
install.packages("dplyr")
install.packages("zoo")
install.packages("reshape2")
install.packages("stargazer")
install.packages("caret")
install.packages("gplots")
install.packages("car")
install.packages("texreg")
install.packages("ggplot2")
install.packages("rCharts")
install.packages("data.table")
install.packages("plotly")
install.packages("networkD3")
install.packages("plm")
install.packages("lfe")
install.packages("d3heatmap")


devtools::install_github("mattflor/chorddiag")



############################################################################
## order in which the files should be run ##################################

# 1. Set two directories: one for the project and one for the data
# 2. Run s_DataMang.R (Attention! This can take up to 20 hours.)
# 3. Run s_MainDtCOnstr.R (can take up to ...)
# 4. Run s_AnalyWorld.R (can take up to ...)€
# 5. Run s_AnalyDEU.R 
# 6. Run s_AnalyCHE.R 
# 7. Run s_AnalyAUT.R 
# 8. Run s_VisualizationsFinal.R


# Files for additional explanation: ExampleMRIOEora.R gives overview of the 
# calculation method employed