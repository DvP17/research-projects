## Script Work Sheet ##

# Text Mining mit tm
# https://www.youtube.com/watch?v=lRTerj8fdY0


# Text Mining Introduction by Data Science Dojo
# https://www.youtube.com/watch?v=Y7385dGRNLM
#
# Quanteda:
# https://www.youtube.com/watch?v=CQsyVDxK7_g


# Build a Text Mining, Machine Learning Document Classification System in R!
# https://www.youtube.com/watch?v=j1V2McKbkLo

#Plot Undervalue
plotline <- function(name,var){
  temp <- subset(dtfull, dtfull$cname == name)
  min <- min(temp[var], na.rm = T)
  max <- max(temp[var], na.rm = T)
  plot(c(1990,2016), c(min,max), type="n")
  abline(h=0, col="green")
  lines(temp$entryforceyear, temp[var])
}

plot(c(1990,2016), c(-20,40), type="n")
for (i in 1:26) {
  temp <- subset(dtfull, dtfull$cname == "Vietnam")
  lines(temp$entryforceyear, temp$nettrade, lwd=1.5,
        lty=1, col=1, pch=1)
}
temp <- subset(dtfull, dtfull$cname == "China")
lines(temp$entryforceyear, temp$bop/1000000000, col=3)
      
plot(c(1990,2016), c(0,3), type="n")
lines(temp$underval, type="b")
abline(h=0)

plot(subset(dtfull, dtfull$cname == "United States")$bop,type = "o")
abline(h=0)

#countpta
plot(c(1,nrow(countpta)), c(0,20), type="n")
lines(countpta$xrat, type="b")
abline(h=0)



  