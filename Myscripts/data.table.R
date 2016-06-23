library(data.table)
library(plyr)
library(dplyr)
library(gcookbook)
library(datasets)
?datasets
set.seed(45L)
DT <- data.table(V1 = c(1L, 2L),
                 V2 = LETTERS[1:3],
                 V3 = round(rnorm(4), 4),
                 V4 = 1:12)
DT

# https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf

### Subsetting rows using i
# subset rows by numbers
DT[3:5, ]

# use column names to select
DT[V2 == "A"]

# column names to select multiple values
DT[V2 %in% c("A", "C")] # damn, works way better than filter

dplyr::filter(DT, V2 %in% c("A", "C"))
dplyr::filter(DT, V2 == "A" | V2 == "C")

### manipulating rows using j

# select 1 column
DT[, V2] # column returned as a vector

# Select multiple columns
DT[, .(V2, V3)] # returned as data.table

DT[, c(V2, V3)] # returned as a vector
# .() is an alias to list()

# call functions
DT[, sum(V1)]

# compute multiple columns
DT[, .(sum(V1), sd(V3))] # returned as a data.table

# Assigning columns names to computed columns
DT[, .(Aggregate = sum(V1), Sd.V3 = sd(V3))] # WHooaaaaaaaaa!!!!!!!!

# columns get recycled if different lengths
DT[, .(V1, sd.V3 = sd(V3))] # repeats calculation for all values of V1

# multiple expressions can be wrapped in curly braces
DT[, {print(V2) 
  plot(V3) 
  NULL}]

DT[, {sum(V1) # this gives
  sd(V3)}]    # only the second result and not the first; gives the last result; others are temporary

### Doing j by group

DT[, .(V4.sum = sum(V4)), by = V1]

# doing j by several groups
DT[, .(V4.sum = sum(V4)), by = .(V1, V2)]

# call functions in by
DT[, .(V4.sum = sum(V4)), by = sign(V1 - 1)] # calculates for every group in sign(V1 - 1)

# assigning new columns name in by
DT[, .(V4.sum = sum(V4)), by = .(V1.01 = sign(V1 - 1))]

# grouping only a subset by specifying i
DT[1:5, .(V4.sum = sum(V4)), by = V1]

# using .N to get the total number of observations of each group
DT[, .N, by = V1]

### Adding/updating columns by reference using :=

# adding updating column by reference using := in one line
DT[, V1 := round(exp(V1), 2)] # result is returned invisibly; no need for DT <- DT[]; WHoooooaaaaa!!!!!

# adding/updating several columns using :=
DT[, c("V1", "V2") := list(round(exp(V1), 2), LETTERS[4:6])] # again invisible; V1 and V2 are changed permanently

# using functional :=
DT[, ':=' (V1 =                  # another way of writing the code above this, but
             round(exp(V1), 2),  # writing comments side by side is easier
           V2 = LETTERS[4:6])][] # result is printed to the screen when [] is added at the end

# removing a column instantly using :=
DT[, V1 := NULL][]

# remove several columns using :=
DT[, c("V1", "V2") := NULL]

### Indexing and keys

# set a key on DT
setkey(DT, V2) # a key is set on column V2; data is sorted on the column we specified as reference

# use keys like supercharged row names to select rows
DT["A"] # returns all rows where key column has the value "A

DT[c("A", "C")] # returns all rows where key column is "A" or "C"

# mult argument is used to control which row that i matches to is returned; default is all
DT["A", mult = "first"]

DT["A", mult = "last"]

# nomatch argument when value specified in i does not match anything; default is NA
DT[c("A", "D")]

DT[c("A", "D"), nomatch = 0]

# by = .EACHI allows to group by each subset of known groups in i; A key must have been set to use this

DT[c("A", "C"), sum(V4)]
DT[c("A", "C"), sum(V4), by = .EACHI]

# any number of columns can be set as keys using setkey. this way, rows can be selected on 2 keys which is an equijoin

setkey(DT, V1, V2)

str(countries)
countries <- as.data.table(countries)
setkey(countries, Name, Year) # 2 different keys set

countries[.("Afghanistan", 1960)] # filter male for the first key and A for the 2nd

countries[.("Afghanistan", c(1960, 1961))] # filter for two things in 2nd key

### Advanced data table operations

# .N returns the number of rows or the last row
# returns the row if used in i and returns count if used in j

countries[.N] # last row

countries[.N - 1] # penultimate row

countries[, .N] # count

# .() is an alias for list and is not needed when there is only one itme in j or by

countries[, .(Name, Year)]

countries[, mean(GDP, na.rm = T), by = .(Name, Year)]
countries[, .(mean = mean(GDP, na.rm = T), count = .N), by = .(Name, Year)]

# .SD is a data.table and holds all the values of all columns, except the ones specified in by
# it reducedes programming time and imrpoves readability; only usable in j

countries[, print(.SD)]
countries[, print(.SD), by = Name]

countries[, .SD[c(1, .N)]]
countries[, .SD[c(1, .N)], by = Name]
countries[, lapply(.SD, sum), by = Year]

# .SDcols is used together with .SD to specify a subset of the columns of .SD to be used in j
names(countries)
countries[, lapply(.SD, sum), by = Name, .SDcols = c("infmortality", "GDP")]

# .SDcols can be the result of a function call

countries[, lapply(.SD, sum(na.rm = T)), by = Name, .SDcols = paste0("G", "DP")]


### Chaining

coun <- countries[, .(GDP_sum = sum(GDP)), by = Name]
coun[GDP_sum > 100]
# the above is without chaining

countries[, .(GDP_sum = sum(GDP)), by = Name][GDP_sum > 100]
countries[, .(GPD_sum = sum(GDP)), by = Name][Name == "Algeria"]

# ordering the result
countries[, .(GDP_sum = sum(GDP)), by = Name][order(-GDP_sum)]

# using the set family

# set() is used to repeatedly update rows and columns by reference. It is a loopable low overhead
# version of :=. Syntax of set:
# for(i in from:to) set(DT, row, column, new value)
# it sets the value of the column to whatever is specified
head(countries)

for(i in 1:2) set(countries, i, 6, 100)

# setnames()
# used to update columns by referrence setnames(DT, "old", "new")

setnames(countries, c("Name", "Code"), c("name", "code"))

# setcolorder() is used to reorder columns by referrence



# http://brooksandrew.github.io/simpleblog/articles/advanced-data-table/

head(mtcars)

dt <- data.table(mtcars)[, .(cyl, gear)]
dt[, unique(gear), by = cyl]

# add all categories of gears for each cyl to the column
dt[,gearsL:=.(list(unique(gear))), by=cyl]; 
head(dt)
dt

dt <- data.table(mtcars)[, .(cyl, gear)]
dt[, ':=' (gearsL = list(unique(gear))), by = cyl][] # another way of writing the above
# this makes ddply redundant!!!!
# accesing elements from a column of lists
dt[, gearsL1 := lapply(gearsL, function(x) x[2])]
dt[, gearsS1 := sapply(gearsL, function(x) x[2])]
head(dt)

str(dt)
# the columns are actually lists if created using lapply

?setdiff
# calculate all gears for all cars of each cyl (excluding the current row)
dt[, other_gear := mapply(function(x, y) setdiff(x, y), x = gearsL, y = gear)]
head(dt)
# can also use this one for the same result
dt[,other_gear:=mapply(setdiff, gearsL, gear)]

# supressing intermediate output with {}

dt <- data.table(mtcars)
dt[, {tmp1 = mean(mpg); tmp2 = mean(abs(mpg - tmp1)); tmp3 = round(tmp2, 2)}, by = cyl]
# the above function returns the last object defined inside {} without a name

# we can be more explicit by passing a names list of what we want to keep
dt[, {tmp1 = mean(mpg); tmp2 = mean(abs(mpg - tmp1)); tmp3 = round(tmp2, 2);
list(tmp2 = tmp2, tmp3 = tmp3)}, by = cyl]

# expressions inside {} can also be written inside {} without semicolons
# {} is tircky to use with := but it can be chained

?shift

# using shift for dealing with lead/lag vectors
dt <- data.table(mtcars)[, .(mpg, cyl)]
dt[, cum_mpg := mpg + shift(mpg, 1)][]

dt[, cum_mpg := mpg + ifelse(shift(mpg, nrow(dt)) == 0, 0, shift(mpg, nrow(dt)))][]
# this isn't working very well

# shift with by
n <- 30
dt <- data.table(
  date=rep(seq(as.Date('2010-01-01'), as.Date('2015-01-01'), by='year'), n/6), 
  ind=rpois(n, 5),
  entity=sort(rep(letters[1:5], n/5))
)
setkey(dt, entity, date)

dt[, indpct_fast := (ind/shift(ind, 1)) - 1, by = entity]

lagpad <- function(x, k) c(rep(NA, k), x)[1:length(x)]
dt[, indpct_slow := (ind/lagpad(ind, 1)) - 1, by = entity]
head(dt, 10)

