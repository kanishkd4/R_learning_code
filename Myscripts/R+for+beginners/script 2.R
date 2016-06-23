#starting chapter 4
#tapply
Veg <- read.table(file = "Vegetation2.txt", header = T)
names (Veg)
str(Veg)
m <- mean(Veg$R)
m1 <- mean(Veg$R[Veg$Transect == 1])
m2 <- mean(Veg$R[Veg$Transect == 2])
m3 <- mean(Veg$R[Veg$Transect == 3])
m4 <- mean(Veg$R[Veg$Transect == 4])
m5 <- mean(Veg$R[Veg$Transect == 5])
m6 <- mean(Veg$R[Veg$Transect == 6])
m7 <- mean(Veg$R[Veg$Transect == 7])
m8 <- mean(Veg$R[Veg$Transect == 8])
c (m, m1, m2, m3, m4, m5, m6, m7, m8)
tapply (Veg$R, Veg$Transect, mean)
mean(Veg$R)
Me <- tapply(Veg$R, Veg$Transect, mean)
Sd <- tapply(Veg$R, Veg$Transect, sd)
Le <- tapply(Veg$R, Veg$Transect, length)
cbind (Me, Sd, Le)

#sapply and lapply (these don't have index)

sapply(Veg[,5:9], mean)
lapply(Veg[5:9], mean)

z <- cbind(Veg$R, Veg$ROCK, Veg$LITTER)
colnames(z) <- c("R", "ROCK", "LITTER")
summary(z)

summary(Veg[ , c("R", "ROCK", "LITTER")])

#the table function

Deer <- read.table(file = "Deer.txt", header = T)
names(Deer)
str(Deer)
unique(Deer$Tb)
table(Deer$Farm)

table(Deer$Sex, Deer$Year)
unique(Deer$Sex)
