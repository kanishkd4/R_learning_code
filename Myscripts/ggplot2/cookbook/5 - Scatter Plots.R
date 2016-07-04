# SCATTER PLOTS
library(ggplot2)
library(gcookbook)
library(plyr)
library(dplyr)
library(MASS)
library(data.table)

# used to display relationships between two continuous variables
# they will often also have a line showing the predicted values based on a statistical model
# pretty damn easy to do
# sense can be made of the data when trends aren't immediately obvious

# Making a basic plot
heightweight[, c("ageYear", "heightIn")]
ggplot(heightweight, aes(ageYear, heightIn)) + geom_point()
# to change shape of points, add shape; to change size, use size

ggplot(heightweight, aes(ageYear, heightIn)) + geom_point(size = 1.5, shape = 21)
# shape 16 is default, 21 is hollow circle, 19 is same as 16 but comes out smooth while exporting in more cases

# Grouping data points by variable by using shape or colour
heightweight[, c("sex", "ageYear", "heightIn")]
ggplot(heightweight, aes(ageYear, heightIn, colour = sex)) + geom_point()
ggplot(heightweight, aes(ageYear, heightIn, shape = sex)) + geom_point()
# the grouping variable must be categorical or converted to factor before using for grouping
# we can map the same variable to shape and colour
ggplot(heightweight, aes(ageYear, heightIn, shape = sex, colour = sex)) +
  geom_point()

# other shapes can be used with scale_shape_manual and other
# colours with scale_colour_brewer() or scale_colour_manual()
ggplot(heightweight, aes(ageYear, heightIn, shape = sex, colour = sex)) +
  geom_point() + scale_shape_manual(values = c(1, 2)) +
  scale_colour_brewer(palette = "Set1")


# Using different point shapes
# specify shape in geom_point()
ggplot(heightweight, aes(ageYear, heightIn)) + geom_point(shape = 3)

ggplot(heightweight, aes(ageYear, heightIn, shape = sex)) + 
  geom_point(size = 3) + scale_shape_manual(values = c(1,4))

# page 79 shows all shapes available
# for shapes 21-25, outline is controlled by colour and fill is controlled by fill
# eg, in the heightweight data, we will add a column that indicates whether the child weighed more than 10 lbs

hw <- heightweight
# categorize into <100 and >=100
hw$weightGroup <- cut(hw$weightLb, breaks = c(-Inf, 100, Inf),
                      labels = c("< 100", ">= 100"))
ggplot(hw, aes(ageYear, heightIn, shape = sex, fill = weightGroup)) +
  geom_point(size = 2.5) +
  scale_shape_manual(values = c(21, 14)) +
  scale_fill_manual(values = c(NA, "black"),
                    guide = guide_legend(override.aes = list(shape = 21)))

# Mapping a continuos variable to colour or size
heightweight[, c("sex", "ageYear", "heightIn", "weightLb")]
ggplot(heightweight, aes(ageYear, heightIn, colour = weightLb)) + geom_point()
ggplot(heightweight, aes(ageYear, heightIn, size = weightLb)) + geom_point() 
# mapping weight to size can be very misleading!

# more on colour and fill in shape 21-25
ggplot(heightweight, aes(weightLb, heightIn, fill = ageYear)) +
  geom_point(shape = 21, size = 2.5) + scale_fill_gradient(low = "black", high = "white")
ggplot(heightweight, aes(weightLb, heightIn, fill = ageYear)) +
  geom_point(shape = 21, size = 2.5) + 
  scale_fill_gradient(low = "black", high = "white", breaks = 12:17, guide = guide_legend())
?scale_fill_gradient

# when we map a cobntinuous variable to an aesthetic, it doesn't stop us from mapping a categorical
# variable to other aesthetics
# to avoid problems due to overplotting, we can make the plot transparent
ggplot(heightweight, aes(ageYear, heightIn, size = weightLb, colour = sex)) +
  geom_point(alpha = .5) + scale_size_area() + # scale_size_area makes the area proportional to numeric value
  scale_colour_brewer(palette = "Set1")
# when a variable is mapped to size, it is a good idea to not map the variable to shape as it is 
# difficult to compare sizes of different shapes




# Dealing with overplotting
# if the amount of overplotting is low, it could be alleviated by using smaller points or
# usign a different shape through which points could be seen (hollow circle)
# for high degree of overplotting 
# make the points semi-transparent
# Bin the data into rectangles (for better quantitative analysis)
# Bin the data into hexagons
# use boxplots

sp <- ggplot(diamonds, aes(carat, price))
sp + geom_point()
sp + geom_point(alpha = .1)
sp + geom_point(alpha = .01)

# another solution is to bin the points to rectangles and map the density of points to the fill colour
# of the rectangles. Verticle bands are barely visible
# by default, stat_bin2d() divides the space into 30 groups in x and y directions
# for a total of 900 bins. we can change the number of bins

sp + stat_bin2d()
sp + stat_bin2d(bins = 50) +
  scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 6000))

# an alternative is to bin the data to hexagons
install.packages("hexbin")
library(hexbin)

sp + stat_binhex() +
  scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 8000))
sp + stat_binhex() +
  scale_fill_gradient(low = "lightblue", high = "red",
                      breaks = c(0, 250, 1000, 2000, 4000, 6000),
                      limits = c(0, 8000))

sp1 <- ggplot(ChickWeight, aes(Time, weight))
sp1 + geom_point()
sp1 + geom_point(position = "jitter")



### Adding fitted regression model lines
# to add lines from a fitted regression model to a scatterplot

sp <- ggplot(heightweight, aes(x = ageYear, y = heightIn))
sp + geom_point() + stat_smooth(method = lm)

# by default, stat_smooth adds a 95% confidence region for the regression fit. It can be changed by
# setting level or removed by se = F
sp + geom_point() + stat_smooth(method = lm, level = 0.95) # 99% confidence
sp + geom_point() + stat_smooth(method = lm, se = F) # no confidence region

# colour, linetype and size can be changed
# dots can be changed to emphasize on the line

sp + geom_point(colour = "grey60") + stat_smooth(method = lm, se = F, colour = "black")

# multiple models can be fitted to the data using stat_smooth(). lm is not the default. Loess is!
sp + geom_point() + stat_smooth(method = loess) # a loess fit

# additional parameters can be passed to the loess() function by passing them to stat_smooth()

# logistic regression can also be passed. Not for the heightweight set but good for biopsy data inside MASS
library(MASS)
b <- biopsy
b$classn[b$class == "benign"] <- 0
b$classn[b$class == "malignant"] <- 1
head(b)

# we will look at the relationship of V1 and the class of tumor. we will jitter and make points semi-transparent as
# there is overplotting

ggplot(b, aes(V1, classn)) + geom_point(position = position_jitter(width = 0.3, height = 0.06),
                                        alpha = 0.4, shape = 21, size = 1.5) +
  stat_smooth(method = glm, method.args = list(family = "binomial")) 
?stat_smooth

# if the scatterplot has grouping by a factor, one fit line will be drawn for each group

sps <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = sex)) + geom_point() +
  scale_colour_brewer(palette = "Set1")
sps + geom_smooth()

# the blue line does not go all the way to the right as there are no data points
# stat_smooth limits the prediction within the range of the predictor data
# and loess function only predicts within the range of x 
# the model lm allows extrapolation and we need to use fullrange = T

sps + geom_smooth(method = lm, se = F, fullrange = T)
# Loess makes more sense for the data as humans don't grow linearly and don't grow forever


### Adding fitted lines from an existing model

# we may already have created a model for a data set and just want to plot it

# A quadratic model using lm() 
model <- lm(heightIn ~ ageYear + I(ageYear^2), data = heightweight)
model

# create a dataframe with ageYear column, interpolating across range
xmin <- min(heightweight$ageYear)
xmax <- max(heightweight$ageYear)

predicted <- data.frame(ageYear = seq(xmin, xmax, length.out = 100))

# calculate predicted values of heightIn
predicted$heightIn <- predict(model, predicted)
predicted

# we can now plot the values predicted from the model
sp <- ggplot(heightweight, aes(ageYear, heightIn)) + geom_point(colour = "grey40")
sp + geom_line(data = predicted, size = 1)

# any model object can be used as long as it has a corresponding predict
# lm has predict.lm, loess has predict.loess

# adding lines from a model can be simplified by using predictvals() that is created below
# pass the model and it will do the work of 
# finding the variable names and range of the predictors and will return a data frame with predictor and predicted values

predictvals <- function(model, xvar, yvar, xrange=NULL, samples=100, ...) {
  # If xrange isn't passed in, determine xrange from the models.
  # Different ways of extracting the x range, depending on model type
  if (is.null(xrange)) {
    if (any(class(model) %in% c("lm", "glm")))
      xrange <- range(model$model[[xvar]])
    else if (any(class(model) %in% "loess"))
      xrange <- range(model$x)
  }
  newdata <- data.frame(x = seq(xrange[1], xrange[2], length.out = samples))
  names(newdata) <- xvar
  newdata[[yvar]] <- predict(model, newdata = newdata, ...)
  newdata
}


# two models with heightweight data
modlinear <- lm(heightIn ~ ageYear, heightweight)
modloess <- loess(heightIn ~ ageYear, heightweight)

lm_predicted <- predictvals(modlinear, "ageYear", "heightIn")
loess_predicted <- predictvals(modloess, "ageYear", "heightIn")

sp + geom_line(data = lm_predicted, colour = "red", size = 0.8) +
  geom_line(data = loess_predicted, colour = "blue", size = 0.8)

# for glm models that use a non linear link function, type = "response" needs to be specified
# to the predictvals function

b <- biopsy
b$classn[b$class == "benign"] <- 0
b$classn[b$class == "malignant"] <- 1

# performing logistic regression
fitlogistic <- glm(classn ~ V1, b, method.args = list(family = "binomial")) # not needed here; doesn't work

fitlogistic <- glm(classn ~ V1, b, family = binomial)

# we will make the graph with jittered points and the fitlogistic line
# by specifying colour in RGB format

glm_predicted <- predictvals(fitlogistic, "V1", "classn", type = "response")
ggplot(b, aes(V1, classn)) + geom_point(position = position_jitter(width = 0.3, height = 0.08),
                                        alpha = 0.4, shape = 21, size = 1) +
  geom_line(data = glm_predicted, colour = "#1177FF", size = 1)


### Adding fitted lines from multiple existing models

# use predictvals with ddply and ldply

# with the heightweight dataset, we make a linear model for each level of sex and put the objects in a list

make_model <- function(data) {
  lm(heightIn ~ ageYear, data)
}
# we can use dlply with this function to build a model for each subset of the data

models <- dlply(heightweight, "sex", .fun = make_model)
models
# models has models#m and models#f

# we can run predictvals to get the predicted values from each model, using the ldply function

predvals <- ldply(models, .fun =  predictvals, xvar = "ageYear", yvar = "heightIn")
predvals

ggplot(heightweight, aes(ageYear, heightIn, colour = sex)) + geom_point() +
  geom_line(data = predvals)

# the range of the predictions is lower for males
# to extend the range

predvals <- ldply(models, .fun = predictvals, xvar = "ageYear", yvar = "heightIn",
                  xrange = range(heightweight$ageYear))
ggplot(heightweight, aes(ageYear, heightIn, colour = sex)) + geom_point() +
  geom_line(data = predvals)


### Adding annotations to model coefficients
# add numeric information about a model

model <- lm(heightIn ~ ageYear, data = heightweight)
summary(model)

pred <- predictvals(model, "ageYear", "heightIn")
sp <- ggplot(heightweight, aes(ageYear, heightIn)) + geom_point() + geom_line(data = pred)

sp + annotate(geom = "text", label = "r^2=0.42", x = 16.5, y = 52)

# instead of using a plain text string, it is also possible to enter formulas using R's math expression syntax
# by parse = T

sp + annotate("text", label = "r^2 == 0.42", x = 16.5, y = 52, parse = T)

# text geoms don't take expression objects directly but take character strings turned into expressions with parse
?parse

# syntax is important

# it is possible to extract values from the model object and build an expression using those values

eqn <- as.character(as.expression(
  substitute(italic(y) == a + b * italic(x) * "," ~~ italic(r)^2 ~ "=" ~ r2,
             list(a = format(coef(model)[1], digits = 3),
                  b = format(coef(model)[2], digits = 3),
                  r2 = format(summary(model)$r.squared, digits = 2)))
))
eqn
parse(text = eqn)
sp + annotate(geom = "text", label = eqn, x = Inf, y = -Inf, hjust = 1.1, vjust = -.5, parse = T)
# WHOAAAAAAAAAAAA!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# math expression syntax can be bloody tricky


### Adding marginal rugs to a scatter plot

ggplot(faithful, aes(eruptions, waiting)) + geom_point() + geom_rug()

# a marginal rug is essentailly a one dimensional scatter plot that can be used to visualize the data 
# distribution on each axis
# there is a lot of overplotting on the x axis
?faithful
ggplot(faithful, aes(eruptions, waiting)) + geom_point() + geom_rug(position = "jitter", size = .2)


### labeling points in a scatter plot
filter(countries, Year == 2009 & healthexp > 2000)

sp <- ggplot(filter(countries, Year == 2009 & healthexp > 2000), aes(healthexp, infmortality)) + geom_point()

sp + annotate("text", x = 4350, y = 5.4, label = "Canada")
sp + annotate("text", x = 7400, y = 6.8, label = "USA")
sp + geom_text(aes(label = Name), size = 4)
sp + geom_text(aes(label = Name), size = 4, vjust = -1)

# add some extra things to y
sp + geom_text(aes(y = infmortality + .1, label = Name), size = 4, vjust = 0)
# hjust can also be used with 0 and 1 like vjust

# we can also only name some of the points if we want
cdat <- subset(countries, Year==2009 & healthexp>2000)
cdat$Name1 <- cdat$Name
idx <- cdat$Name1 %in% c("Canada", "Ireland", "United Kingdom", "United States",
                         "New Zealand", "Iceland", "Japan", "Luxembourg",
                         "Netherlands", "Switzerland")
idx
cdat$Name1[!idx] <- NA
ggplot(cdat, aes(x=healthexp, y=infmortality)) +
  geom_point() +
  geom_text(aes(x=healthexp+100, label=Name1), size=4, hjust=0) +
  xlim(2000, 10000)


### creating a baloon plot




















