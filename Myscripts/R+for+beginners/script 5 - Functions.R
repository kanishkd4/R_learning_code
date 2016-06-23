setwd("C:\\Users\\309292\\Desktop\\R\\RBook")

#Function to find the no. of missing values per variable
Veg <- read.table(file = "Vegetation2.txt", header = T)
names(Veg)
str(Veg)
NAPerVariable <- function(X1) {
  D1 <- is.na(X1)
  colSums(D1)
}
NAPerVariable(Veg[5:24, ])

Parasite <- read.table(file = "CodParasite.txt", header = T)
names(Parasite)

NAPerVariable(Parasite)
NAPerVariable(Parasite[1:1200, ])

ZerosPerVariable <- function (X1) {
  D1 = (X1 == 0)
  colSums(D1, na.rm = T)
}

ZerosPerVariable(Parasite)

VariableInfo <- function(X1, Choice1) {
  if (Choice1 == "Zeros") {D1 = (X1 == 0)}
  if (Choice1 == "NAs") {D1 <- is.na(X1)}
  colSums(D1, na.rm = T)
}
ZerosPerVariable(Parasite)
VariableInfo(Parasite, "Zeros")
VariableInfo(Parasite, "NAs")


