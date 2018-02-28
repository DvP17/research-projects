## Tables and Graphics ##

# 1. Descriptive Statistics ####
library(xtable)
xtable(summary(dtfull), latex.environments = "center")

# 2. Regression Tables ####
library(stargazer)
stargazer(model1, #model2, model3,
          title = "DV Interest Rate"
          #, dep.var.labels = c("test1","test2")
          )

stargazer(model1, title = "DV Interest Rate", type = "text")

# 3. Graphics ####
# library(interplot)
# interplot(m = fixed2, var1 = "depthacc", var2 = "nettrade")
# 
# mod_fe <- plm(inv ~ value + capital + value:capital, data = Grunfeld, model = "within")
# Grunfeld[ , "firm"] <- factor(Grunfeld[ , "firm"]) # needs to be factor in the data NOT in the formula [required by package effects]
# mod_lsdv <- lm(inv ~ value + capital + value:capital + firm, data = Grunfeld)
# coefficients(fixed2)    # estimates are the same
# coefficients(lm(intrateplus1 ~ depthacc*nettrade + gdp + inflation + debtext + lvau_garriga,
#     data=ptestdf))
# 
# eff_obj <- effects::Effect(c("value", "capital"), mod_lsdv)
# plot(eff_obj)

#The one and only:
library(plm)
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

# UNVAL
par(mfrow=c(1,1))
interaction_plot_continuous(modunval3,"log1p(depthacc)","lvau_garriga",
                            "log1p(depthacc):lvau_garriga",
                            title = "Relationship between DEPTH and dUNVAL 
for different levels of CBI",
                            xlabel = "Central Bank Independence",
                            ylabel = "Marginal Effect of DEPTH on dUNVAL")

# PRAT
par(mfrow=c(1,2))
interaction_plot_continuous(modprat2,"log1p(depthacc)","lvau_garriga",
                            "log1p(depthacc):lvau_garriga",
                            title = "Relationship between CBI and PRAT+2 
for different levels of NETTRADE",
                            xlabel = "Central Bank Independence",
                            ylabel = "Marginal Effect of DEPTH on PRAT+2")
interaction_plot_continuous(modprat5,"log1p(depthacc)","negsqrt(nettrade)",
                            "log1p(depthacc):negsqrt(nettrade)",
                            title = "Relationship between DEPTH and PRAT+2 
for different levels of NETTRADE",
                            xlabel = "Cube Root of NETTRADE",
                            ylabel = "Marginal Effect of DEPTH on PRAT+2")


# 4. Data ####

tableall<-df.merge4[,c("year","usdx","usdreserves","foreigntroops","milexp",
                       "importshare","domcred","finservices")]

library(xtable)

#Absatz für Beschriftung auf nächster Seite
add.to.row <- list(pos = list(0), command = NULL)
command <- paste0("\\hline\n\\endhead\n","\\hline\n",
                  "\\multicolumn{",dim(tableall)[2] + 1, "}{l}",
                  "{\\footnotesize Continued on next page}\n",
                  "\\endfoot\n","\\endlastfoot\n")
add.to.row$command <- command

print(xtable(tableall,display = c("s","s","f","f","s","E","f","f","f"),
             caption = "Tabelle 2: Verwendete Daten"),
      caption.placement = "top",math.style.exponents = TRUE,
      tabular.environment = "longtable",add.to.row = add.to.row)


print(xtable(tableall,caption = "Tabelle 2: Verwendete Daten"),
      caption.placement = "top",tabular.environment = "longtable",
      add.to.row = add.to.row)

print(xtable(tableall))


# Notes ####