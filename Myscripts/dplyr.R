
?nycflights13
library(nycflights13)
library("dplyr")
?dim
dim(flights)
head(flights)
View(flights)

filter(flights, month == 1, day == 1)
filter(flights, month == 1 & day == 1)
filter(flights, month == 1 | day == 1)
filter(flights, month == 1 | month == 2) #gives the correct filter
filter(flights, month == c(1, 2)) # gives the incorrect filter (50% for 2 variables, 20% for 5)

arrange(flights, year, month, day)
arrange(flights, desc(year, month, day))
arrange(flights, desc(arr_delay))

select(flights, year, month, day)

# Select all columns between year and day (inclusive)
select(flights, year:day)

# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))

select(flights, tail_num = tailnum) #tail_num is the new name, tailnum the old

rename(flights, tail_num = tailnum) #tail_num is the new name, tailnum the old

distinct(select(flights, tailnum)) # extract distinct rows

mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)

mutate(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / (air_time / 60)
)

#You can use sample_n() and sample_frac() to take a random sample of rows,
# either a fixed number for sample_n() or a fixed fraction for sample_frac().

sample_n(flights, 10)
sample_frac(flights, 0.01)

library("ggplot2")

by_tailnum <- group_by(flights, tailnum) # groups dataframe by talenum for all operations
View(flights)
delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 2000)
# Interestingly, the average delay is only slightly related to the
# average distance flown by a plane.
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

#n(): number of observations in the current group
#n_distinct(x): count the number of unique values in x.
#first(x), last(x) and nth(x, n) - these work similarly to x[1], x[length(x)],
#and x[n] but give you more control of the result if the value isn't present.

# group by runs operations as if all operations are indexed by the grouped variable
# the grouped varaible is always retained


destinations <- group_by(flights, dest)
summarise(destinations,
          planes = n_distinct(tailnum),
          flights = n()
)

?summarise
summary(flights)
flights

# understanding on the PL file

PL <- read.csv(file = "D:\\Projects\\PL\\Interest rate optimization\\PL with tenure.csv", header = T)

PL$DISBURSAL_DATE <- as.Date(as.character(PL$DISBURSAL_DATE), "%d/%m/%Y")

PL1 <- mutate(PL , 
              day = as.numeric(format(as.Date(as.character(DISBURSAL_DATE)), "%d"))
              )

by_day <- group_by(PL1, day)

day_anal <- summarise(by_day, 
                      count = n(),
                      rate = mean(CUST_IRR, na.rm = T),
                      c = n_distinct(day)
                      )# count and mean indexed by day


###################### USING do
mtcars
by_cyl <- group_by(mtcars, cyl)
do(by_cyl, head(., 3))

do(group_by(mtcars, carb), x = sum(hp))

models <- by_cyl %>% do(mod = lm(mpg ~ disp, data = .))
models






ce <- ddply(cabbage_exp, "Date", transform,
            percent_weight = Weight / sum(Weight) * 100)
ggplot(ce, aes(x=Date, y=percent_weight, fill=Cultivar)) +
  geom_bar(stat="identity")

