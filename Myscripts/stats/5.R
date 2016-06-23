# SELECTING CASES

?mtcars
# 1974 car tests data
mtcars
# mean quater mile time
mean(mtcars$qsec) # quater mile mean
mean(mtcars$qsec[mtcars$cyl == 8]) # = is assignment and == is equal
library(dplyr)
mtcars %>% filter(cyl == 8) %>% summarise(mean(qsec)) 
# mean(qsec) doesn't work in dplyr; have to use summarize

median(mtcars$hp)

# mean MPG for cars above median HP
mean(mtcars$mpg[mtcars$hp > median(mtcars$hp)])
mtcars %>% filter(hp > median(hp)) %>% summarize(mean(mpg))

# we can also create a new dataset with just 8 cylinder cars





# ANALYSING BY SUBGROUPS
?iris
# measurements on different species of irises
iris
mean(iris$Petal.Width)
aggregate(iris$Petal.Width ~ iris$Species, FUN = mean)
# aggregate gives mean of petaol width as a function of species

# compare groups on several variables
aggregate(cbind(iris$Petal.Length, iris$Petal.Width) ~ iris$Species, FUN = mean)

# this gived v1 and v2 for the two variables in cbind