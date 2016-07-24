rm(list = ls())
library(ggplot2)
library(gcookbook)
library(MASS)
library(data.table)
library(plyr)
library(scales)
library(corrplot)
library(igraph)
library(rgl)

# There are many ways of visualizing data and some things don't fit into nice, tidy categories

### Making a corelation matrix
mtcars
mcor <- cor(mtcars)
# print mcor and round to 2 digits
round(mcor, digits = 2)
# note to self: how the fuck do you show this in a visual?

# if there are many columns that we do not want for corelatoins, we should exclude them
# NA's can be treated with use = "complete.obs" or use = "pairwise.complete.obs"
# to graph a corelation matrix, use corrplot

corrplot(mcor)
# the functin corrplot has many options, like shapes and colours
corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45)

# it may also be useful to display labels representing the corelation coefficient on each square
# of the matrix. We'll use a lighter palette so that the text is readable.
# we'll order using order = "AOE" (angular order of eigenvectors)
# so that the corelated items are closer together

# generate a lighter palette
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45, col = col(200),
         addCoef.col = "black", addcolorlabel = "no", order = "AOE")
# table 13.1 has some useful options for corrplot





### Plotting a function
# use stat_function
# it is also necessary to give ggplot a dummy data frame so that it gets the proper x range
# we'll use dnorm which gives the density of a normal distribution
# the date frame is only useful for setting the range
p <- ggplot(data.frame(x = c(-3, 3)), aes(x))
p + stat_function(fun = dnorm)

# some functions take additional arguments, like dt(), the function for the density of a
# t distribution takes parameters for degrees of freedom. These additional arguments can be 
# passed to the function by putting them in a list and giving the list to the args
p + stat_function(fun = dt, args = list(df = 2))

# It is also possible to define your own functiions
myfun <- function(xvar) {
  1/(1 + exp(-xvar + 10))
}

ggplot(data.frame(x = c(0, 20)), aes(x)) + stat_function(fun = myfun)
# by default, the function is plotted along 101 points along the x range
# Plotting predicted functions was shown in 5.7




### Shading a sub region under a function curve
# Define a new wrapper function around your curve function and replace out of range values with NA
# return dnorm for x between 0 and 2 and NA for all other x

dnorm_limit <- function(x){
  y <- dnorm(x)
  y[x < 0 | x > 2] <- NA
  return(y)
}

ggplot(data.frame(x = c(-3, 3)), aes(x)) + stat_function(fun = dnorm_limit, geom = "area",
                                                         fill = "blue", alpha = .2) +
  stat_function(fun = dnorm) # well, I'll be damned!

# We can program a function to program another function(sort of like nested functions)
limit_range <- function(fun, min, max) {
  function(x){
    y <- fun(x)
    y[x < min | x > max] <- NA
    return(y)
  }
}
# now, we can call this function to create another function; one that is effectively the same as
# dnorm_limit used earlier
dlimit <- limit_range(dnorm, 0, 2)
dlimit(-2:4)

# we can use limit range function to create a function passed to stat_function
p + stat_function(fun = dnorm) + 
  stat_function(fun = limit_range(dnorm, 0, 2), geom = "area", fill = "blue", alpha = .2)
# the limit range function can be used with any function, not just dnorm to create a range-limit 
# version of that function. the result is that instead of writing different hardcoded values
# for every situation, we can simply write a function and pass differnt arguments to it




### Creating a network graph
# use the igraph package
# to create a graph, pass a vector containing pairs of items to graph() and plot the resulting 
# object
# specify the edges for a directed graph

gd <- graph(c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6))
plot(gd)

# for an undirected graph
gu <- graph(c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6), directed = F)
# no labels
plot(gu, vertex.label = NA)
str(gd)

# In a network graph, the position of the nodes is unspecified by the data, and they're placed 
# randomly. To make the output repeatable, you can set a random seed before making a plot
# we can try different random numbers till w e get a result we like
set.seed(229)
plot(gu)
# It's also possible to create a graph from a data frame. The first two rows are used and
# each row specifies a connection between two nodes
# we'll use this on the madmen2 data 
# we'll also use the Fruchterman-Reingold layout algorithm. The idea is that all nodes have a 
# magnetic repulsion from one another, but the edges between nodes act as springs, pulling
# the nodes together. 
madmen2
g <- graph.data.frame(madmen2, directed = T)

# remove unnecessary margins
par(mar = c(0, 0, 0, 0))
plot(g, layout = layout.fruchterman.reingold, vertex.size = 8, edge.arrow.size = .5)#,
     vertex.label = NA)

# It is also possible to make a directed graph from a data frame
# The madmen data has only one row for each pairing and we can use the circle layout
# if direction doesn't matter
g <- graph.data.frame(madmen, directed = F)
par(mar = c(0, 0, 0, 0))
plot(g, layout = layout.circle, vertex.size = 8, vertex.label = NA)
# for more information about the available output options, see 
?plot.igraph
?igraph::layout

# An alternative to igraph is Rgraphviz, which is a frontend for Graphviz, an open source library
# for visualizing graphs



### Using text labels in a graph
# pass a vector of names to vertex.label
# copy madmen and drop every other row
m <- madmen[1:nrow(madmen) %% 2 == 1,]
g <- graph.data.frame(m, directed = F)

?V() # vertices of graphs
V(g)$name # print the names of each vertex

plot(g, layout = layout.fruchterman.reingold, 
     vertex.size = 4,             # smaller nodes
     vertex.label = V(g)$name,    # set the labels
     vertex.label.cex = 0.8,      # slightly smaller font
     vertex.label.dist = 0.4,     # offset the labels
     vertex.label.color = "black")

# Another way to achieve the same effect is to modify the plot object, instead of passing the
# values as arguments to plots. To do this, use V()$xxxx <- instead of passing a value to 
# a vector.xxxx argument

# This is equivalent to the preceding code
V(g)$size <- 4
V(g)$label <- V(g)$name
V(g)$label.cex <- 0.8
V(g)$label.dist <- 0.4
V(g)$label.color <- "black"

# Set a property of the entire graph
g$layout <- layout.fruchterman.reingold
plot(g)

# The properties of the edges can also be set, either with E() or passing the values to 
# edge.xxxx arguments
# View the edges
E(g)

# set some of the labels to "M"
E(g)[c(2, 11, 19)]$labels <- "M"

# set colour of all to grey and then set some to red
E(g)$color <- "grey70"
E(g)[c(2, 11, 19)]$color <- "red"
plot(g)

?igraph.plotting




### Creating a heat map
# use geom_tile or geom_raster and map a variable to fill
presidents # a time series object
str(presidents)

# convert it to a form usable by ggplot: a dataframe
pres_rating <- data.frame(
  rating = as.numeric(presidents),
  year = as.numeric(floor(time(presidents))),
  quater = as.numeric(cycle(presidents))
)
pres_rating

# now, we can make the heat map
p <- ggplot(pres_rating, aes(year, quater, fill = rating))

# using geom_tile 
p + geom_tile()

# using geom_raster
p + geom_raster()

# customization required for better heat maps
# we'll reverse y axis, place tick marks for every 4 years and change colour scale
p + geom_tile() + scale_x_continuous(breaks = seq(1940, 1976, by = 4)) +
  scale_y_reverse() + 
  scale_fill_gradient2(midpoint = 50, mid = "grey70", limits = c(0, 100))




### Creating a 3D scatter plot
# 