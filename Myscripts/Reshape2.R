library(reshape2)
F <- flights
F
# cast
# acast for vector/matrix/array
# dcast for df output

airquality
names(airquality)
names(airquality) <- tolower(names(airquality))
aqm <- melt(airquality, id = c("month", "day"), na.rm = TRUE)
acast(aqm, day ~ month ~ variable) # x~y~z
acast(aqm, month ~ variable, mean)
acast(aqm, month ~ variable, mean, margins = T) # same as above but gives total
acast(aqm, month ~ variable, mean, margins = c("month", "variable"))
acast(aqm, month ~ variable, mean, margins = c("month")) # gives totals only for month
?length

library("plyr") # always need plyr for subset
acast(aqm, variable ~ month, mean, subset = .(variable == "ozone"))# filtering for ozone
?.

acast(aqm, variable ~ month, mean, subset = .(month == 5))

acast(aqm, variable ~ month, mean, subset = .(day == 5))

#chick weight example
names(ChickWeight) <- tolower(names(ChickWeight))
chick_m <- melt(ChickWeight, id = 2:4, na.rm = TRUE)
unique(chick_m$variable)

?melt

dcast(chick_m, time ~ variable, mean) # average effect of time
dcast(chick_m, diet ~ variable, mean) # average effect of diet
acast(chick_m, diet ~ time, mean) # average effect of diet and time

acast(chick_m, time ~ diet, length) # how many chicks at a particular time?
acast(chick_m, chick ~ time, mean)
acast(chick_m, chick ~ time, mean, subset = .(time < 10 & chick < 20)) # using subset to filter

dcast(chick_m, diet + chick ~ time)
# this does not add, but gives a combination of diet and chick on the rows! 
# Like the tabular form design option in excel with all row items repeated

acast(chick_m, diet + chick ~ time)
# acast concatenates diet and time with a _ in between

acast(chick_m, chick ~ time + diet)

acast(chick_m, diet + chick ~ time, length, margins = "diet")
# this takes the margins for all diet and chick combined

acast(chick_m, diet + chick ~ time, length, drop  = F)
# drop defines if missing combinations should be dropped or kept. dropped by default

# tips example
View(tips)
dcast(melt(tips), sex ~ smoker, mean, subset = .(variable == "total_bill"))
melt(tips) # melt simply groups by all factors or categorical variables

# moving to melt
a <- as.list(c(1:4), NA)
a
melt(a)
names(a) <- letters[1:4]
melt(a)
mtcars



# understanding on the PL file

PL <- read.csv(file = "D:\\Projects\\PL\\Interest rate optimization\\PL with tenure.csv", header = T)

PL$DISBURSAL_DATE <- as.Date(as.character(PL$DISBURSAL_DATE), "%d/%m/%Y")

PL1 <- mutate(PL , 
              day = as.numeric(format(as.Date(as.character(DISBURSAL_DATE)), "%d"))
)
