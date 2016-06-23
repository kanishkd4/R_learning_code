load(url("http://maths-people.anu.edu.au/~johnm/r/dsets/usingR.RData"))
save.image("usingR.RData")

# CHAPTER 1
elasticband <- edit(elasticband) # to open a spreadsheet editor

names(elasticband)
ggplot(data = elasticband, aes(x = stretch, y = distance)) + geom_point()

SC <- data.frame(year = c(1970:1979))
SC$snow.cover <- c(6.5, 12, 14.9, 10, 10.7, 7.9, 21.9, 12.5, 14.5, 9.2)
SC

ggplot(data = SC, aes(x = snow.cover)) + geom_histogram(binwidth = 1)
ggplot(data = SC, aes(x = snow.cover)) + geom_histogram(aes(y = ..density..), binwidth = 1) + geom_density()

# CHAPTER 2
# R may be used as a calculator
2+2
pi

names(hills)
summary(hills)

pairs(hills) # this is a very helpful graphical summary

# R will handle a variety of specific analysis, eg regression and correlation

options(digits = 3) # allows us to decide how R computes and displays its results
cor(hills) # shows correlation between all elements
cor(log(hills)) # correlation between logs in one command
# it wil however not name elements as log in the previous statement
# why did the correlation between the logs of the element go down?

# straight line regression
# regression between distance travelled by the elastic band and the amount of stretch

ggplot(data = elasticband, aes(stretch, distance)) + geom_point(shape = 16)
elastic.lm <- lm(distance ~ stretch, data = elasticband)
lm(distance ~ stretch, data = elasticband) # gives the linear regression coefficients

summary(elastic.lm) # this gives more detailed information; can also be directly used with lm formula

# LOOPING - screw loops

# useful functions

print() # prints single R object
cat() # prints multiple objects
length() # no. of objects in a vector or a list
unique() # gives a vector of unique values
diff()
sort() # sort omits NA's
order() # order places NA's at the end
cumsum()
cumprod()
rev() # reverses the order of vector elements
table() # makes a table of counts
search()

miles.to.km <- function(x){
  x * 8/5
} 

miles.to.km(1)

# CHAPTER 3 AND 4 - PLOT AND LATTICE (use ggplot2 instead)

# CHAPTER 5 - regression models and ANOVA

qplot(distance, stretch, data = elasticband)

elastic.lm <- lm(distance ~ stretch, data = elasticband)
options(digits = 4)
summary(elastic.lm)
names(elastic.lm)
# there is a lot of information inside the elastic.lm
coef(elastic.lm)
resid(elastic.lm)
# summary is most often used to inspect the output of a regression as it extracts info we most likely want
# results of a regression can also be plotted

