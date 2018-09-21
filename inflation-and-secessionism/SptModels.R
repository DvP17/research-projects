## Models ##

# Load Data


# Bivariate Models


# PLM-Models V-Dem #######################################################
library(plm)
pvdemdf <-  pdata.frame(dtFull, index = c("country","year"))

ptestmod1.1 <- plm(v2csantimv ~ inflatavg + debtm1 + popDensity + ctrySize
                 + e_Fiscal_Reliance + v2svinlaut + v2clrgunev + v2clsocgrp,
                data=pvdemdf, model = "within", effect = "twoways")
ptestmod1.2 <- plm(v2csantimv ~ inflatavg * v2clsocgrp + debtm1 +
                     popDensity + ctrySize + e_Fiscal_Reliance,
                 data=pvdemdf, model = "within", effect = "twoways")
ptestmod1.3 <- plm(v2svstterr ~ inflatavg * popDensity + debtm1 + ctrySize
                 + e_Fiscal_Reliance + v2clsocgrp,
                 data=pvdemdf, model = "within", effect = "twoways")
summary(ptestmod1.1)

#conflict
ptestmod2.1 <- plm(e_Civil_War ~ inflatavg * ctrySize + debtm1 + popDensity
                 + e_Fiscal_Reliance,
                 data=pvdemdf, model = "within", effect = "twoways")
ptestmod2.2 <- plm(v2csanmvch_9 ~ inflatavg * popDensity + debtm1 + ctrySize
                 + e_Fiscal_Reliance + v2svinlaut + v2clrgunev,
                 data=pvdemdf, model = "within", effect = "twoways")
ptestmod2.3 <- plm(v2csanmvch_9 ~ inflatavg + popDensity + debtm1 + ctrySize
                   + e_Fiscal_Reliance + v2svinlaut + v2clrgunev + v2clsocgrp,
                   data=pvdemdf, model = "within", effect = "twoways")
summary(ptestmod2.3)



# PLM-Models World Values
library(plm)
pvaluedf <-  pdata.frame(dtFullwvs, index = c("country","year"))

ptestmod3.1 <- plm(trust1 ~ inflatavg + popDensity + v2clsocgrp  + debtm1 
                 + ctrySize + v2svinlaut + v2clrgunev + v2pepwrsoc,
                 data=pvaluedf, model = "within", effect = "twoways")
ptestmod3.2 <- plm(trust1 ~ inflatavg * popDensity + v2clsocgrp  + debtm1 
                 + ctrySize + v2svinlaut + v2clrgunev + v2pepwrsoc,
                 data=pvaluedf, model = "within", effect = "twoways")
ptestmod3.3 <- plm(trust1 ~ inflatavg * v2clsocgrp + popDensity + debtm1 
                 + ctrySize + v2svinlaut + v2clrgunev + v2pepwrsoc,
                 data=pvaluedf, model = "within", effect = "twoways")
ptestmod3.4 <- plm(trust2 ~ inflatavg + debtm1 + popDensity + ctrySize
                   + v2svinlaut + v2clsocgrp + v2clrgunev + v2pepwrsoc,
                   data=pvaluedf, model = "within", effect = "twoways")
ptestmod3.5 <- plm(national ~ inflatavg + debtm1 + popDensity + ctrySize
                 + v2svinlaut + v2clrgunev + v2clsocgrp,
                 data=pvaluedf, model = "within", effect = "twoways")
summary(ptestmod3.5)

# Diagnostics: F-Test and Hausmann
pFtest()
phtest()


# Stargazer
library(stargazer)
stargazer(ptestmod1.1, ptestmod1.2, ptestmod1.3, ptestmod2.1, ptestmod2.2, 
          ptestmod2.3, type = "latex")

stargazer(ptestmod3.1, ptestmod3.2, ptestmod3.3, ptestmod3.5, ptestmod3.5, 
          type = "latex")


# Interaction
interaction_plot_continuous <- function(model, effect, moderator, interaction, varcov="default", minimum="min", maximum="max", incr="default", num_points = 50, conf=.95, mean=FALSE, median=FALSE, alph=80, rugplot=T, histogram=T, title="Marginal effects plot", xlabel="Value of moderator", ylabel="Estimated marginal coefficient"){
  
  # Define a function to make colors transparent
  makeTransparent<-function(someColor, alpha=alph){
    newColor<-col2rgb(someColor)
    apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                                blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
  }
  
  # Extract Variance Covariance matrix
  if (varcov == "default"){
    covMat = vcov(model)
  }else{
    covMat = varcov
  }
  
  # Extract the data frame of the model
  mod_frame = model.frame(model)
  
  # Get coefficients of variables
  beta_1 = model$coefficients[[effect]]
  beta_3 = model$coefficients[[interaction]]
  
  # Set range of the moderator variable
  # Minimum
  if (minimum == "min"){
    min_val = min(mod_frame[[moderator]])
  }else{
    min_val = minimum
  }
  # Maximum
  if (maximum == "max"){
    max_val = max(mod_frame[[moderator]])
  }else{
    max_val = maximum
  }
  
  # Check if minimum smaller than maximum
  if (min_val > max_val){
    stop("Error: Minimum moderator value greater than maximum value.")
  }
  
  # Determine intervals between values of the moderator
  if (incr == "default"){
    increment = (max_val - min_val)/(num_points - 1)
  }else{
    increment = incr
  }
  
  # Create list of moderator values at which marginal effect is evaluated
  x_2 <- seq(from=min_val, to=max_val, by=increment)
  
  # Compute marginal effects
  delta_1 = beta_1 + beta_3*x_2
  
  # Compute variances
  var_1 = covMat[effect,effect] + (x_2^2)*covMat[interaction, interaction] + 2*x_2*covMat[effect, interaction]
  
  # Standard errors
  se_1 = sqrt(var_1)
  
  # Upper and lower confidence bounds
  z_score = qnorm(1 - ((1 - conf)/2))
  upper_bound = delta_1 + z_score*se_1
  lower_bound = delta_1 - z_score*se_1
  
  # Determine the bounds of the graphing area
  max_y = max(upper_bound)
  min_y = min(lower_bound)
  
  # Make the histogram color
  hist_col = makeTransparent("grey")
  
  # Initialize plotting window
  plot(x=c(), y=c(), ylim=c(min_y, max_y), xlim=c(min_val, max_val), xlab=xlabel, ylab=ylabel, main=title)
  
  # Plot estimated effects
  lines(y=delta_1, x=x_2)
  lines(y=upper_bound, x=x_2, lty=2)
  lines(y=lower_bound, x=x_2, lty=2)
  
  # Add a dashed horizontal line for zero
  abline(h=0, lty=3, lwd=2, col="darkgreen")
  
  # Add a vertical line at the mean
  if (mean){
    abline(v = mean(mod_frame[[moderator]]), lty=2, col="red")
  }
  
  # Add a vertical line at the median
  if (median){
    abline(v = median(mod_frame[[moderator]]), lty=3, col="blue")
  }
  
  # Add Rug plot
  if (rugplot){
    rug(mod_frame[[moderator]])
  }
  # Add Histogram (Histogram only plots when minimum and maximum are the min/max of the moderator)
  if (histogram & minimum=="min" & maximum=="max"){
    par(new=T)
    hist(mod_frame[[moderator]], axes=F, xlab="", ylab="",main="",
         border=hist_col, col=hist_col, breaks = 20)
  }
}

interaction_plot_continuous(ptestmod1.2,"inflatavg","v2clsocgrp",
                            "inflatavg:v2clsocgrp",
                            title = "Relationship between INFLATION and dANTIMOVEMENT \n for different levels of Ethnic Equality",
                            xlabel = "Ethnic Equality",
                            ylabel = "Marginal Effect of INFLATION on dANTIMOVEMENT")
color <- rgb(0,255,0, alpha=60, maxColorValue=255)
rect(xleft=-2, xright=-1.83, ybottom=-2, ytop=100, density=100, col=color)

interaction_plot_continuous(ptestmod1.3,"inflatavg","popDensity",
                            "inflatavg:popDensity",
                            title = "Relationship between INFLATION and dAUTHTERR \n for different levels of Population Density",
                            xlabel = "Population Density",
                            ylabel = "Marginal Effect of INFLATION on dAUTHTERR")
interaction_plot_continuous(ptestmod2.1,"inflatavg","ctrySize",
                            "inflatavg:ctrySize",
                            title = "Relationship between INFLATION and dCIVILWAR \n for different levels of Country Size",
                            xlabel = "Country Size",
                            ylabel = "Marginal Effect of INFLATION on dCIVILWAR")
rect(xleft=1250, xright=1550, ybottom=-3, ytop=100, density=100, col=color)
interaction_plot_continuous(ptestmod2.2,"inflatavg","popDensity",
                            "inflatavg:popDensity",
                            title = "Relationship between INFLATION and dSEPARATWAR \n for different levels of Population Density",
                            xlabel = "Population Density",
                            ylabel = "Marginal Effect of INFLATION on dSEPARATWAR")

interaction_plot_continuous(ptestmod3.3,"inflatavg","v2clsocgrp",
                            "inflatavg:v2clsocgrp",
                            title = "Relationship between INFLATION and dANTI-TRUSTSOC \n for different levels of Ethnic Equality",
                            xlabel = "Ethnic Equality",
                            ylabel = "Marginal Effect of INFLATION on dANTI-TRUSTSOC")