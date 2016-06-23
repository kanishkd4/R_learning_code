# Plot function
Veg <- read.table(file = "Vegetation2.txt", header = T)
names(Veg)
plot(Veg$BARESOIL)
plot(Veg$BARESOIL, Veg$R)

plot(x = Veg$BARESOIL, y = Veg$R, 
     xlab = "Exposed soil", ylab = "Species Richness", main = "Scatter Plot", 
     xlim = c(0,45), ylim = c(4,19))

summary (cbind(Veg$R, Veg$BARESOIL))

#pch to change the symbol used in a plot
plot(x = Veg$BARESOIL, y = Veg$R, 
     xlab = "Exposed soil", ylab = "Species Richness", main = "Scatter Plot", 
     xlim = c(0,45), ylim = c(4,19), pch = 16)
str(Veg)
Veg$Transect

plot(x = Veg$BARESOIL, y = Veg$R, 
     xlab = "Exposed soil", ylab = "Species Richness", main = "Scatter Plot", 
     xlim = c(0,45), ylim = c(4,19), pch = Veg$Transect)

Veg$Time2 <- Veg$Time

Veg$Time2 [Veg$Time <= 1974] <- 1
Veg$Time2 [Veg$Time > 1974] <- 16
Veg$Time2

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed Soil", ylab = "Specied Richness", main = "Scatter Plot",
     xlim = c(0,45), ylim = c(4,19), pch = Veg$Time2)

#changing colour of plotting symbols

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed Soil", ylab = "Species Richness", main = "Scatter Plot",
     xlim = c(min(Veg$BARESOIL, na.rm = TRUE), max(Veg$BARESOIL, na.rm = TRUE)),
     ylim = c(min(Veg$R, na.rm = TRUE), max(Veg$R, na.rm = TRUE)),
     col = 9)

x <- 1:8
x
plot(x, col = x)
?par

Veg$Time2
Veg$Col2 <- Veg$Time
Veg$Col2[Veg$Time <= 1974] <- 1
Veg$Col2[Veg$Time > 1974] <- 2
Veg$Col2

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed Soil", ylab = "Species Richness", main = "Scatter plot",
     xlim = c(min(Veg$BARESOIL, na.rm = TRUE), max(Veg$BARESOIL, na.rm = TRUE)),
     ylim = c(min(Veg$R, na.rm = TRUE), max(Veg$R, na.rm = TRUE)),
     pch = Veg$Time2, col = Veg$Col2)

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed Soil", ylab = "Species Richness", main = "Scatter plot",
     xlim = c(min(Veg$BARESOIL, na.rm = TRUE), max(Veg$BARESOIL, na.rm = TRUE)),
     ylim = c(min(Veg$R, na.rm = TRUE), max(Veg$R, na.rm = TRUE)),
     pch = Veg$Time2, col = Veg$Col2)

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed Soil", ylab = "Species Richness", main = "Scatter plot",
     xlim = c(min(Veg$BARESOIL, na.rm = TRUE), max(Veg$BARESOIL, na.rm = TRUE)),
     ylim = c(min(Veg$R, na.rm = TRUE), max(Veg$R, na.rm = TRUE)),
     pch = Veg$Time2, col = Veg$Col2, cex = 0.5)

# Adding a smoothening Line

plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed Soil", ylab = "Specied Richness", main = "Scatter Plot",
     xlim = c(0,45), ylim = c(4,19))
M.Loess <- loess(Veg$R ~ Veg$BARESOIL)
Fit <- fitted(M.Loess)
lines(Veg$BARESOIL, Fit)

M.Loess
summary(M.Loess)

#the curve drawn above was not what we expected. The problems occur because BARESOIL is not sorted from..
#.. high to low. Below shows a properly drawn smoothening curve
plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed Soil", ylab = "Specied Richness", main = "Scatter Plot",
     xlim = c(0,45), ylim = c(4,19))
M.Loess <- loess(R~BARESOIL, data = Veg)
Fit <- fitted(M.Loess)
Ord1 <- order(Veg$BARESOIL)
lines(Veg$BARESOIL[Ord1], Fit[Ord1], lwd = 3, lty = 2)

