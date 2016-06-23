?plotmath
?ggplot
library(ggplot2)
library("scales")
p <- ggplot(diamonds, aes(carat, price, colour = cut))
p <- p + layer(geom = "point")
p
P <- p + layer(geom = "bar", geom_params = list(fill = "steelblue"), stat = "bin", 
               stat_params = list(binwidth = 2) # stat = "bin" didn't work for me
)
P

?ggplot
ggplot(diamonds, aes(carat, price, color = cut)) + layer(geom = "point")
?aes
ggplot(diamonds, aes(carat)) + layer(geom_histogram(binwidth = 2, fill = "steelblue"))


# All the shortcut functions have the same basic form, beginning with geom_ or stat_:
# geom_XXX(mapping, data, ..., geom, position)
# stat_XXX(mapping, data, ..., stat, position)
?geom_histogram
geom_histogram(data = aes(carat), binwidth = 2, fill = "steelblue")# does nothing

ggplot(data = msleep, aes(x = sleep_rem/sleep_total, y = awake)) + geom_point()
# equivalent to
qplot(x = sleep_rem/sleep_total, y = awake, data = msleep)

#layers can be added to qplot too

qplot(x = sleep_rem/sleep_total, y = awake, data = msleep) + geom_smooth()
#equivalent to
qplot(x = sleep_rem/sleep_total, y = awake, data = msleep, geom = c("point", "smooth"))
#equivalent to
ggplot(data = msleep, aes(x = sleep_rem/sleep_total, y = awake)) + geom_point() + geom_smooth()

p <- ggplot(data = msleep, aes(x = sleep_rem, y = awake))
summary(p)
p <- p + geom_point()
summary(P)

bestfit <- geom_smooth(method = "lm", se = F, colour = alpha("steelblue", 0.5), size = 2)

qplot(x = sleep_rem, y = sleep_total, data = msleep) + bestfit
qplot(x = awake, y = brainwt, data = msleep, log = "y") + bestfit
qplot(x = awake, y = brainwt, data = msleep, log = "xy") + bestfit
?qplot

p <- ggplot(mtcars)
summary(p)
p <- p + aes(wt, hp)
summary(p)
?geom_smooth
p + geom_point()
?pairs

ggplot(diamonds, aes(x=carat)) + geom_histogram(binwidth=.25, fill="steelblue")

install.packages("ggvis")
library(ggvis)
diamonds %>% ggvis(~carat) %>% layer_histograms(width=.25, fill:="steelblue")

p + geom_point(colour = "darkblue")# this sets the colour to darkblue

p + geom_point(aes(colour = "darkblue")) # this maps the colour to darkblue
# mapping the colour creates a new variable containing only the value "darkblue" and then 
# maps colour to that variable

# grouping
?interaction
library(nlme) #needed for Oxboys
# group is set to the interaction of discrete variables
# this often partitions data correctly but sometimes it fails for discrete variables
# cannot group when there isn't a discrete variable

Oxboys
View(Oxboys)
;names(Oxboys)

p <- ggplot(data = Oxboys, aes(age, height, group = Subject)) + geom_line()
p
ggplot(data = Oxboys, aes(age, height)) + geom_line()# when we leave out group

#the grouped plot shows one line per subject. His height as he ages

# DIFFERENT GROUPS ON DIFFERENT LAYERS

p + geom_smooth(aes(group = Subject), method = "lm", se = F)
# this adds a smoothed line for each boy/subject. a best fit for each boy

p + geom_smooth(aes(group = 1), method = "lm", se = F, size = 2)
# the above code uses group = 1 to create a smoothed line for all boys. One best fit for all

# OVERRIDING DEFAULT GROUPING
?aes
boysbox <- ggplot(data = Oxboys, aes(Occasion, height)) + geom_boxplot()
boysbox
# default grouping works because occasion is a discrete variable

# to overlay individual trajectories, we can override default grouping for that layer with'
# aes(group = Subject)

boysbox + geom_line(aes(group = Subject), colour = "#3366FF")
# Line colour is different to make it distinct from box plot




# MATCHING AESTHETICS TO GRAPHIC OBJECTS
# note how aesthetics of individual obs are mappe to the aesthetics of the complete entity
# page 62 of pdf - Geoms, stats and default stats with geoms


ggplot(diamonds, aes(carat)) + geom_histogram(binwidth = 0.1)
ggplot(diamonds, aes(carat)) + geom_histogram(aes(y = ..density..), binwidth = 0.1)
# names of generated variables must be surrounded with .. when used; useful if the original 
# data also has a variable of the same name
# can also be plotted using qplot
qplot(carat, ..density.., data = diamonds, geom = "histogram", binwidth = 0.1)

# position adjustments - page 65 in pdf used by position = 
# PUTTING IT TOGETHER
# combining geoms and stats

d <- ggplot(diamonds, aes(carat)) + xlim(0, 3)
d + stat_bin(aes(ymax = ..count..), binwidth = 0.1, geom = "area")
d + stat_bin(aes(size = ..density..), geom = "point", position = "identity")
d + stat_bin(aes(ymax = ..density..), geom = "point", position = "identity")
d + stat_bin(aes(y = 1, fill = ..count..), binwidth = 0.1, geom = "tile", position = "identity")
# xlim si used to determine max scale of x axis

# DISPLAYING PRECOMPUTED STATS
# data that has already been summarised can be used with stat_identity, which leaves the data unchanged

# VARYING AESTHETICS AND DATA
# different datasets can be plotted on different layers of the same plot
# a common example is supplementing the data with predictions from a model

Oxboys
head(Oxboys)
require(nlme, quiet = T, warn.conflicts = F)
model <- lme(height ~ age, data = Oxboys, # linear mixed effects model
             random = ~ 1 + age | Subject)
oplot <- ggplot(Oxboys, aes(age, height, group = Subject)) + geom_line()
# next, we'll compare the predicted trajectories to the actual trajectories
# this is done by building a grid that contains all combinations of ages and subjects
# we add the predictions back into the model as a variable called height

age_grid <- seq(-1, 1, length = 10)
subjects <- unique(Oxboys$Subject)
preds <- expand.grid(age = age_grid, Subject = subjects)
preds$height <- predict(model, preds)

# once we have the predictions, we can display them along with the original data
# because we have used the same name as the original Oxboys dataset, and we want the same group aesthetic
# we don't need to specify any aestheitc, but only override the default aesthetic
oplot + geom_line(data = preds, colour = "#3366FF", size = 0.4)

# it captures the high level structure of the data but it is hard to see the details
# plots of longitudanal data are often called spaghetti plots
# another way to compare the model to the data is to look at residuals
# we add predictions from the model to the original data(fitted), calculate residuals(resid)
# and add the residuals as well

Oxboys$fitted <- predict(model)
Oxboys$resid <- with(Oxboys, fitted - height)

oplot %+% Oxboys + aes(y = resid) + geom_smooth(aes(group = 1))
# %+% is to update the default data
# the smooth line shows that the residuals are not random, showing a deficiency in the model
# we add a quadratic term, refit the model, recalculate predictions and residuals, and replot

model2 <- update(model, height ~ age + I(age ^ 2))
Oxboys$fitted2 <- predict(model2)
Oxboys$resid2 <- with(Oxboys, fitted2 - height)
oplot %+% Oxboys + aes(y = resid2) + geom_smooth(aes(group = 1))

# modifying plot object is quite easy
# we updated the data and replotted twice without needing to reinitilise oplot. 