###########################################################################|
#######################   M R I O   E X A M P L E   #######################|
###########################################################################|

library(readxl)
eora <- read_excel("~/Data/Ecological Footprint/Eora/Eora26Structure.xlsx", 
                   sheet = "Dummy MRIO")

T <- data.matrix(eora[4:15,3:14])
rownames(T) <- eora[4:15,]$..1
colnames(T) <- as.vector(t(eora[2,3:14]))

FD <- data.matrix(eora[4:15,16:21])
rownames(FD) <- eora[4:15,1:14]$..1
colnames(FD) <- as.vector(t(eora[1,16:21]))

VA <- data.matrix(eora[18:20,3:14])
rownames(VA) <- eora[18:20,]$..1
colnames(VA) <- as.vector(t(eora[2,3:14]))

Q <- data.matrix(eora[25,3:14])
row.names(Q) <- "Direct Emissions"
colnames(Q) <- as.vector(t(eora[2,3:14]))

###########################################################################|

xout <- rowSums(T) + rowSums(FD)

totalinput <- t(xout)

E <- Q / totalinput

A <- sweep(T, MARGIN = 2, totalinput, FUN = '/')

I <- diag(rep(1, ncol(T)))

L <- solve(I - A) 

m <- E %*% L

###########################################################################|
# mat <- matrix(1:9, 3, byrow = T)
# sweep(mat, MARGIN = 1, 1:3, `*`)

emissiondyads <- sweep(L, MARGIN = 1, m, FUN = `*`)

