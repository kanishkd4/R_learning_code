names(squid)
str(squid)
squid2 <- read.table(file = "squidGSI.txt", dec=",", header = T)
str(squid2)
# Always combine read.table with name and str
GSI
m1 <- lm(GSI ~ factor(Location) + factor(Year), data = squid)
# not all functions support "data ="
mean (GSI, data = squid)
#if a function has the data argument, use it; it's really neat and easy top use!
squid$GSI
squid[,6]
mean(squid$GSI)
detach("sn")
squid$sex
str(squid)
squid$Sex
unique(squid$Sex)
sel <- squid$Sex == 1
squidm <- squid[sel, ]
squidm

squidm <- squid[squid$Sex == 1, ]

squid[ ,6]

mean(squid[,6])

mean(squid$GSI)

squidf <- squid[squid$Sex == 2,]
squidf

str(squid)
mean(squidf$GSI)
mean(squidm$GSI)
mean(squid$GSI)
unique(squid$Location)

squid123 <- squid[squid$Location == 1 | squid$Location == 2 | squid$Location == 3,]
squid123
unique(squid123$Location)

squidm1 <- squid[squid$Sex == 1 & squid$Location == 1, ]
squidm1

squid

mean(squidm1$GSI)

squid[,1]
str(squid)
squidm <- squid

ord1 <- order(squid$Month)
squid[ord1,]
ord1
options(max.print) = 5.5E5)

?options
options(max.print=5.5E5)

sq1 <- read.table("Squid1.txt", header = T)
sq2 <- read.table("Squid2.txt", header = T)
str(squid)
str(sq2)
sqm <- merge(sq1,sq2, by = "Sample")
sqm

squidm <- squid[squid$Sex == 1, ]
squidm

write.table (squidm, file = "MaleSquid1.txt", sep = " ",quote = TRUE, append = FALSE, na = "NA")
str(squid)
squid$flocation <- factor(squid$location)
squid$fsex <- factor(squid$Sex)
str(squid)
squidfsex

squid$fsex <- factor (squid$Sex, levels =c(1,2), labels = c("M","F"))
squid$fsex

boxplot(GSI ~ fsex, data = squid)
