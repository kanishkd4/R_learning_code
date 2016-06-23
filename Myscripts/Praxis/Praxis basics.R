str(insurance)
install.packages("dplyr")
I1 <- insurance$incident_date
I2 <- as.numeric(insurance$incident_date)
I3 <- cbind(I1, I2)
View(I1)
?cbind
View(I2)
cbind(I1, I2)

View(bankloan)

for(i in c(6, 7, 8)) {
  blood[, i] = as.numeric(as.character(blood[, i]))
}

aggregate(cbind(WBC, RBC) ~ Age + BloodGroup, data = blood, FUN = mean)

aggr <- as.data.frame(aggregate(cbind(WBC, RBC) ~ Age + BloodGroup, data = blood, FUN = mean))

?cut
SW <- read.table(file = "C:\\Users\\309292\\Desktop\\New folder\\sample_weblog", header = F, sep = "")
install.packages("stringi")#, "stringr")1
install.packages("stringr")
readLines(SW)
SW <- 
readLines(file = "C:\\Users\\309292\\Desktop\\New folder\\sample_weblog")
SW1 <- readLines(file.choose())
?readLines

webdata <- stri_extract_all(SW1, regex = "[[:alnum:][:punct:]]+")
webdata

w <- t(as.data.frame(webdata))

# converting the character to date

rownames(w) <- NULL
w1 <- as.data.frame(w)
w1$V4 <- gsub(x = w1$V4, pattern = "\\[", replacement = "") # have to use \\ for [
w1$V4 <- sub(x = w1$V4, pattern = ":", replacement = ", ") # sub will substitute the first occurance
w1$V4 <- stri_datetime_parse(w1$V4, "dd-MMM-uuuu HH:mm:ss")
str(w1)

?stri_datetime_parse
stri_datetime_parse("01/Jul/1995, 00:00:09","dd-MMM-uuuu HH:mm:ss")

install.packages("lubridate")
library(lubridate)
hour(w1$V4)
seconds(w1$V4)
w1$V4
?as.POSIXct

chars <- c("abc", "bcd", "efgh", "abd", "ecg", "ahf")
stri_extract_all(chars, regex = "^[a]") # defines index of all where a is the beginning
stri_extract_all(chars, regex = "[d]$") # defines index of all where d is in the end
stri_extract_all(chars, regex = "^[a] | [d]$") # starting with a and ending with d
# problem figuring out how to use and. did it usng .*

?stri_extract_all
?grep
?agrep

# loops today, functions next week and simulations after that

library(lubridate)
# functions are similar to excel for date manipulation

# functions include being able to use today's date(now), 
now()
# We can extract the day of the month and year
mday(now())
yday(now())
week(now())
month(now())

# if we want to modify from today's date, we can
now() + years(1)
# we can also change month, day, hour, etc

round_date(x = now() + years(1), unit = "month")
# round_date calls the date rounded to the unit we want

dmy("16/04/2014")
round_date(dmy("16/04/2014") + years(1), unit = "month")

# if we need the last date of february, 2008

dmy("02/02/2008") + days(30)

# there is a package timedate that can do more calculations than lubridate

install.packages("timeDate")
library(timeDate)
timeLastDayInMonth(charvec = "02/02/2008", format = "%d/%m/%Y")

# the following is creating a function to define the last day of the month
myfunc <- function(x){
  require(lubridate)
  y = ceiling_date(x, unit = "month") - days(1)
  return(y)
}

myfunc(dmy("02/02/2008")) # this works

start <- 

start <- mdy("03.24.2015")
end <- ydm("2015/12/09")

end - start # nice

interval <- new_interval(start = start, end = end)
# interval is type formal class interval

interval/dweeks(1)
dweeks(1)
dyears(1)
names(insurance)
str(insurance)
insurance$incident_date <- as.Date(as.character(insurance$incident_date),
                                   format = "%d/%m/%Y")# how I've done it earlier

insurance$incident_date <- dmy(as.character(insurance$incident_date))

insurance$dob <- dmy(as.character(insurance$dob))

as.Date(as.character("01/01/1936"), format = "%d/%m/%Y")

str(insurance)

library(dplyr)

insurance$incident_date <- dmy(as.character(insurance$incident_date))

insurance$dob <- dmy(as.character(insurance$dob))

insurance1 <- mutate(insurance,
                     age.at.incidence = new_interval(start = dob, end = incident_date),
                     age = age.at.incidence %/% dyears(1)
                     )
View(insurance1)
df.date <- data.frame(c(dmy("24/03/2013"), dmy("10/06/2014")),
                      HH = c(10, 14),
                      MM = c(12, 28),
                      SS = c(10, 12))
View(df.date)

update.date <- with(df.date, update(Date, hour = HH, minute = MM, second = SS))

# loops. situations to use different types of loops
library(dplyr)
list = ls()
ls()
names(insurance)
insurance1 <- select(insurance, claim_amount, coverage, income, townsize)
View(insurance1)

table(insurance1$townsize)
# we can run a k means clustering on this data set
# we can use a for loop

listcenters = list()
for(i in 1:length(unique(insurance1$townsize))){  # unique are 5 as table shows
  df = subset(insurance1, townsize == i, select = c("claim_amount", "coverage",
                                                    "income"))
  fitkmeans = kmeans(x = df, centers = 3, nstart = 4) # imp to put nstart to get consistent result
  centers = fitkmeans$centers
  listcenters[[i]] = centers
}

listcenters
# it creates 3 clusters for all townsizes. 

x <- rep(NA, 100000)
t1 <- Sys.time()
for (i in 1:100000){
  x[i] = rnorm(1)
}
t2 <- Sys.time()
t2-t1

t3 <- Sys.time()
y <- sapply(1:100000, function(i)rnorm(1))
t4 <- Sys.time()
t4 - t3

# if we have to extract the mean values of characteristics in autompg
# we can use the aggregate function

aggregate(cbind(displacement, horsepower, weight, class) ~ 1, data = autompg,
          mean, na.rm = T)
# aggregate has na.action. this by default removes na's so we don't really need 
# na.rm = T
?aggregate
mean(autompg$horsepower)
mean(autompg$weight, na.rm = T)

# na.rm = T gives a smaller result than the aggregate function!!!!

aggregate(cbind(displacement, horsepower, weight, class) ~ 1, data = autompg,
          mean)

# note that na.omiot removes the entire row and na.rm only removes the NA values
# while calculating. 

aggregate(cbind(displacement, horsepower, weight, class) ~ 1, data = autompg,
          length) # this would confirm that!


