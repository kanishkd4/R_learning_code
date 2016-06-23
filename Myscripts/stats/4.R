# EXAMINING OUTLIERS

# categorical outliers
# categorical outliers may not sound intuitive as categories don't have distnce or position
# what we can have is unusual values like a category that contains less than the sample
# as a normal curve may not serve as a good approximation

OS <- read.csv(file = "E:\\Lynda courses\\Exercise2\\Ex_Files_RStats_EssT\\Exercise Files\\Ch04\\04_01\\OS.csv", header = T)
View(OS)
OS
# an outlier in this case is any that constitutes of less than 10%
# we can combine all of them in "Other"; this can be a really bad idea as it can do damage
# so, we might not want to use them at all

OS.hi <- subset(OS, Proportion > 0.1)
OS.hi


# Quantitative data
?rivers
hist(rivers)
boxplot(rivers, horizontal = T)
boxplot.stats(rivers)
rivers.low <- rivers[rivers < 1210]
boxplot.stats(rivers.low)
# we seee we still have outliers as the dataset gets trimmed
# for some data, this can be a long process of remoiving outliers multiple time







# Transforming Variables
# Sometimes, data cannot be deleted and it has to be trasformed to fit better with the assumptions of the analysis
?islands
islands # areas of world's major landmasses
hist(islands, breaks = 16)
boxplot(islands)

# turning into Z scores
islands.z <- scale(islands)
# scale transforms it into a mean of 0 and sd of 1
# the obkect created is a matrix
islands.z
hist(islands.z, breaks = 16)
mean(islands.z) # is almost 0
sd(islands.z) # is 1

attr(islands.z, "scaled:center") # this shows original mean
attr(islands.z, "scaled:scale") # this shows original sd
islands.z <- as.numeric(islands.z)
islands.z

# Logarithmic transformation
islands.ln <- log(islands) # natural log, base e
islands.log10 <- log10(islands) # base 10
islands.log2 <- log2(islands) # binary log, bsae 2

# cannot get a log of 0
# if you have 0, add 1 or 0.5 to the values

# Squaring
# for negetively skewed variables
# you may need to recenter the distribution so that all values are positive

# Ranking
# Another transformation that maintains only the order of the distribution
islands.rank1  <- rank(islands)
# check out boxplot or hist

# ties can be dealt with
islands.rank2 <- rank(islands, ties.method = "random")
# ties can be on average, first, random, max or min

# Dichotomizing!
# we lose a lot of info and can exaggerate differnces between things

continent <- ifelse(islands > 1000, 1, 0)
continent
# Dichotomization makes sense here but might not in other requirements








# COMPUTING COMPOSITE VARIABLES
rn1 <- rnorm(1000000)
hist(rn1)
summary(rn1)
# mean and median are very close to 0. range is from -5 to +5 (standard normal)

rn2 <- rnorm(1000000)
summary(rn2)
# Everything but the extremes are almost the same for rn1 and rn2

rn.mean <- (rn1 + rn2)/2 # easy as R is vectorized and rn1 and rn2 have the same length (1000000)
rn.mean

rn.prod <- rn1*rn2
summary(rn.prod)

# Kurtosis
install.packages("moments")
library(moments)
library(psych)
kurtosi(rn1)
kurtosi(rn2)
kurtosi(rn.mean)
kurtosi(rn.prod) # similar to a cauchy distribution





# CODING MISSING DATA
# summary works with NA, mean does not
x1 <- c(1, 2, 3, NA, 5)
which(is.na(x1)) # which gives the index number
mean(x1, na.rm = T)
x2 <- x1
x2[is.na(x2)] <- 0
x2
# missing values can be replaced with a number

x3 <- ifelse(is.na(x1), 0, x1) # another way to put in 0
x3
# there are many ways to impute data in values
# package mi is one that has very complex imputations
# mice: multivariate imputation by chained variables
# imputation package
# the eprocedure will be very similar to what we did here
rm(list = ls())
