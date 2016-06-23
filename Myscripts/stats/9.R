# STATS FOR 3 OR MORE VARIABLES

# COMPUTING A MULTIPLE REGRESSION
?USJudgeRatings
# lawyers evaluated 43 judges on 12 demeanors
data("USJudgeRatings")
head(USJudgeRatings)

# Basic multiple regression
reg1 <- lm(RTEN ~ CONT + INTG + DMNR + DILG + CFMG +
             DECI + PREP + FAMI + ORAL + WRIT + PHYS,
           data = USJudgeRatings)
reg1
# gives the call and coefficients
# we haven't computed bivariate correlations yet
# this is a joint function of within the context of all associations
# we can end up with a negetive coefficient for something that might have
# a positive correlation
summary(reg1)
# we have the same coefficients and also the standard errors for each coef
# and p value. we are looking for p vales less than 0.5(* next to those)
# Physucal ability is a big predictor. maybe a sign of weakness 
# a very high adjusted R square and multiple r squared
# a very good prediction from this model

# we can get more advanced summaries from this model like an anova table
# more detailed summaries
anova(reg1)
coef(reg1)
confint(reg1)
resid(reg1)
hist(residuals(reg1))

# posibility of a stepwise variables selection
# (backwards and forwards): exercise caution!!

# Backwards stepwise regression
# repeating the first regression model which contains all of the predictor
# variables and serves as a starting point

regb <- step(reg1,
             direction = "backward",
             trace = 0) # trace = 0  means "Don't print every step"
# if trace != 0, we get an enormous amount of info we don't need
summary(regb)
# we have only significant relationships and has interrelationships or multicolineatiry 
# of the variables
# still a huge r squared

# forwards stepwise regression 
# start with model that has nothing but a constant
reg0 <- lm(RTEN ~ 1, data = USJudgeRatings) # minimal model
reg0
regf <- step(reg0, # start with minimal model
             direction = "forward",
             scope = (~ CONT + INTG + DMNR + DILG + CFMG + DECI + PREP + 
                        FAMI + ORAL + WRIT + PHYS),
             data = USJudgeRatings,
             trace = 0) # Don't print the steps
summary(regf)
# this is consistent going either way  

install.packages("rms")
# regression modelling strategies





# COMPARING MEANS WITH TWO FACTOR ANOVA
# it allows us to use two categorical variables in a single quantitative outcome
?warpbreaks
data("warpbreaks")
boxplot(breaks ~ wool*tension, data = warpbreaks)

# specify a model of variance model with interaction
aov1 <- aov(breaks ~
              wool + tension + wool:tension,
            # or: wool+ tension,
            data = warpbreaks)
summary(aov1)
# we have 3 factors, wool, tension and the interaction of the two
# the F value is the actual test statistic and the p value is the probability values
# we are looking for p < 0.05
# wool is nearly significant and the dataset is small so might be something 
# to look into
# the interaction is also statisticlly significant

# additional info on model
model.tables(aov1)
# we have a table of effects
model.tables(aov1, typw = "means")
model.tables(aov1, typw = "effects") # effects is the default

# Post-hoc test; many choices but we use tukey here
TukeyHSD(aov1)
# we are looking at probabilities; we are looking for p < 0.05
# great way of looking at the effects of two categorical variables 
# both individually and on their interaaction on a single quantitative outcome variable





# CONDUCTING A CLUSTER ANALYSIS
?mtcars
# road test data from 1974
data(mtcars)
head(mtcars)
mtcars1 <- mtcars[, c(1:4, 6:7, 9:11)]
head(mtcars1)

# three major kinds of clustering
# 1. split into number of clusters (eg kmeans)
# 2. Hierarchical: start sepparate and combine
# 3. Dividing: start with a single group and split

# we are going to focus on hierarchichal
# need distance matrix (dissimilarity matrix)
d <- dist(mtcars1)
d # huge matrix
# it is like a correlation matrix and tells how similar each car is
# to all others and quantifies it as distance or dissimilarity

# using distance matrix for clustering
c <- hclust(d)
c
plot(c)
# we can get a picture of branches and knows how to plot clusters
library(ggplot2)
qplot(c) # not working
# this clusteing tells us what it takes for a car to be in a group
g3 <- cutree(c, k = 3) # "g3" = "groups 3" 3 clusters
# we can also use h = 230 instead of k = 3 (height in plot greater than 230)

gm <- cutree(c, k = 2:5) # or k = c(2, 4)
gm
# 2,3,4,5 are how many clusters there are
# maserati bora changes cluster every time

# draw boxes around clusters
rect.hclust(c, k = 2, border = "gray")
rect.hclust(c, k = 2, border = "blue")
# this is all based on similarity on the data
# different variables woul give different clusters
rect.hclust(c, k = 4, border = "blue")
rect.hclust(c, k = 5, border = "blue")

# k means clustering 
km <- kmeans(mtcars1, 3) # 3 clusters
km
# 3 clusters with the number of cases and then gives us the means 
# of the cases for each variables
# this enables us to form centroids; a centroid would be a centroid on different dimensions
# we also have sum of squares clusters

# graph based on k means
require(cluster)
clusplot(mtcars1, # data frame
         km$cluster, # cluster data
         color = T, # colour
         # shade = T, # lines in clusters
         lines = 3, # lines connecting centroids
         labels = 2) # labels clusters and cases

# this is a very different type of plotfor clusters
# there is a problem with labels overlapping; no easy solution right now
clusplot(mtcars1, # data frame
         km$cluster, # cluster data
         color = T, # colour
         shade = T, # lines in clusters
         lines = 3, # lines connecting centroids
         labels = 2) # labels clusters and cases





# CONDUCTING A PRINCIPAL COMPONENTS/factor analysis

# From "psych" package documentation (p. 213)
# "The primary empirical difference between a components 
# versus a factor model is the treatment of the variances
# for each item. Philosophically, components are weighted
# composites of observed variables while in the factor
# model, variables are weighted composites of the factors."

# Load data 
?mtcars
data(mtcars)
mtcars[1:5, ]
mtcars1 <- mtcars[, c(1:4, 6:7, 9:11)]  # Select variables
mtcars1[1:5, ]

# we are going to be grouping the columns
# Principle components model using default method
# If using entire data frame:
pc <- prcomp(mtcars1,
             center = TRUE,  # Centers means to 0 (optional)
             scale = TRUE)  # Sets unit variance (helpful)
# Or specify variables one at a time:
# pc <- prcomp(~ mpg + cyl + disp + hp + wt + qsec + am + 
#                gear + carb, data = mtcars, scale = TRUE)
?prcomp  # Generally preferred
?princomp  # Very slightly different method, similar to S

# Get summary stats
summary(pc)
# we have the importance of components, the sd and proportion of variance
# and commulative proportion
# we can decide how important each of these are


# Screeplot
plot(pc)
# the screeplot has a lot to do with how much of the variance each of these explain


# Get standard deviations and how variables load on PCs
pc
# rotation lets us know how the relationship is between variables and components


# See how cases load on PCs
predict(pc)
# can look at only the first twoor 3 components

# Biplot
biplot(pc)
# this gets very busy but we cna just see the two components that really matter
# PC1 and 2 were important; in red are the variables that go into components
 

# Factor Analysis
# Varimax rotation by default
# Gives chi square test that number of factors
# is sufficient to match data (want p > .05).
# Also gives uniqueness values for variables,
# variable loadings on factors, and variance
# statistics.
factanal(mtcars1, 1) # just one factor (sort of stupid)
factanal(mtcars1, 2)
factanal(mtcars1, 3)
factanal(mtcars1, 4)  # First w/p > .05
# with 4 factors, the observations do not significantly differ from the model
# here, we are looking for a p > 0.05

rm(list = ls())
