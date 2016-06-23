x <- 1:10
y <- 1:20
length(x)
x[5]
x[-5]
y - x
z <- c(x, "a", y)
View(z)
a <- as.numeric(z)
a
View(z)
round(digits = 3, 10.44556)
rm(R.data_1)
s <- sample(x, 10) #selecting a random sample

which(s == 1) #which can be used to subset a dataset

y <- c(rep(2, 3), 3:6)
which(y == 2)
View(y)
str(blood)
#for sas, dots are missing values. In R, we have to define it
#beacause of the dot, it takes the value of a character
# the factor cannot just be converted to number
# convert factor to chacarter and then to numeric
head(n=10,x = blood$WBC)

y1 <- as.character(blood$WBC)
y1 <- as.numeric(y1)

levels(blood$Age)
unique(blood$Age)

y2 <- as.character(blood$Chol)
y2 <- as.numeric(y2)
View(y2)
View(blood$Chol)
str(blood)

# R stores data as vectors, matrix(only stores one type of data),
# data frame should have the same lines for each variable
# list is the most powerful storage

blood[100:105, ] #selecting rows from 100 to 105

blood[c(100:105, 110:115), ]

#starting on data manipulation (hopefully)

blood$WBC <- as.character(blood$WBC)
blood$WBC <- as.numeric(blood$WBC)

blood$RBC <- as.character(blood$RBC)
blood$RBC <- as.numeric(blood$RBC)

blood$ratio <- blood$WBC/blood$RBC
str(blood)
blood$ratio <- with(blood, WBC/RBC) #with helps you to avoid $

# within is another option

blood1 <- within(blood, {ratio = WBC/RBC; sum = WBC + RBC})

# conditinal indexing

blood2 <- na.omit(blood1)
str(blood2)

is.na(blood) = blood == "." #dots will be converted to NA (works on
                            # numbers and characters)

summary(blood$WBC, na.rm = T)
#Loops..for and whle loop

for(i in 1:2) 
  df <- auto[, i]
df

# we can use df as a list
for(i in 1:2)
  df[[i]] <- auto[, i]
df

# apply function

str(insurance)
with(insurance, lapply(unique(claim_type), 
                       function(x)mean(claim_amount[claim_type == X])))



xy <- data.frame(df)
rm(xy)


# functions

fivenum_new <- funcion(x) {
  p <- fivenum(x)
  IQR <- p[4] - p[2]
  L.Bound <- p[2] - 1.5*IQR
  U.Bound <- p[4] + 1.5*IQR
  names(p) <- c("Min", "1st.Q", "Med", "3rd.Q", "Max")
  p1 <- c(p[1], L.Bound, p[2], p[3], p[4], U.Bound, p[5])
  names(p1)[2] <- "L.Bound"
  names(p1)[6] <- "U.Bound"
  return(p1)
}
install.packages("ggplot2")

summary
lm

str(auto)
f <- auto[auto$type == "Truck", ]

g <- filter(auto, type == "Truck")
View(auto)
g
View(g)

# http://www.milanor.net/blog/?p=594
# http://www.milanor.net/blog/?p=534
# http://www.milanor.net/blog/?p=534
