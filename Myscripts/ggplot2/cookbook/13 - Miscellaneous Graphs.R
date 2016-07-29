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

# install.packages("corrplot")
# install.packages("igraph")
# install.packages("rgl")
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
# rgl provides an interface to the opengl graphics library for 3d graphics.
# use plot3d() and pass a data frame where the first 3 columns are the x, y and z
# coordinates or pass 3 vectors

plot3d(mtcars$wt, mtcars$disp, mtcars$mpg, type = "s", size = .75, lit = F)
# type = "s" gives spherical points
# 3d plots are generally difficult to interpret but we can make it easier
# we can add vertical segments to give a sense of the spatial positions of the 
# points. 
# function to interleave the elements of two vectors 
interleave <- function(v1, v2) as.vector(rbind(v1, v2))

# Plot the points
plot3d(mtcars$wt, mtcars$disp, mtcars$mpg,
       xlab = "Weight", ylab = "Displacement", zlab = "MPG",
       size = .75, type = "s", lit = FALSE)
# Add the segments
segments3d(interleave(mtcars$wt, mtcars$wt),
           interleave(mtcars$disp, mtcars$disp),
           interleave(mtcars$mpg, min(mtcars$mpg)),
           alpha = 0.4, col = "blue")

# It's possible to tweak the appearance of the background and the axes

# make the plot without axis or labels
plot3d(mtcars$wt, mtcars$disp, mtcars$mpg,
       xlab = "", ylab = "", zlab = "", axes = F,
       size = .75, type = "s", lit = FALSE)

segments3d(interleave(mtcars$wt, mtcars$wt),
           interleave(mtcars$disp, mtcars$disp),
           interleave(mtcars$mpg, min(mtcars$mpg)),
           alpha = 0.4, col = "blue")

# Draw the box
rgl.bbox(colour = "grey50", emission = "grey50", xlen = 0, ylen = 0, zlen = 0)

# Set the default colour of future objects to black
rgl.material(color = "black")
# Add axes to specific sides. Possible values are "x--", "x-+", "x+-", and "x++".
axes3d(edges=c("x--", "y+-", "z--"),
       ntick=6, # Attempt 6 tick marks on each side
       cex=.75) # Smaller font
# Add axis labels. 'line' specifies how far to set the label from the axis.
mtext3d("Weight", edge="x--", line=2)
mtext3d("Displacement", edge="y+-", line=3)
mtext3d("MPG", edge="z--", line=3)




### Adding a prediction surface to a 3D plot
# First, we need to define some utility functions to generate predicted values from a model object
# Given a model, predict zvar from xvar and yvar
# Defaults to a range of x and y variables and a 16x16 grid

predictgrid <- function(model, xvar, yvar, zvar, res = 16, type = NULL) {
  xrange <- range(model$model[[xvar]]) # the range of the predictor variable; works for lm and glm
  yrange <- range(model$model[[yvar]]) # may need to customize for others
  
  newdata <- expand.grid(x = seq(xrange[1], xrange[2], length.out = res),
                         y = seq(yrange[1], yrange[2], length.out = res))
  names(newdata) <- c(xvar, yvar)
  newdata[[zvar]] <- predict(model, newdata = newdata, type = type)
  newdata
}

# Convert long style data frame to a list with x, y and z with x and y as row/column values and z as a matrix
df2mat <- function(p, xvar = NULL, yvar = NULL, zvar = NULL) {
  if(is.null(xvar)) xvar <- names(p)[1]
  if(is.null(yvar)) yvar <- names(p)[2]
  if(is.null(zvar)) zvar <- names(p)[3]
  x <- unique(p[[xvar]])
  y <- unique(p[[yvar]])
  z <- unique(p[[zvar]], nrow = length(y), ncol = length(x))
  m <- list(x, y, z)
  names(m) <- c(xvar, yvar, zvar)
  m
}

# Function to interleave the elements of the two vectors
interleave <- function(v1, v2) as.vector(rbind(v1, v2))

# With these utility functions defined, we can make a linear model from the data and plot it as a mesh along the data
# using the surface3d() function

m <- mtcars
mod <- lm(mpg ~ wt + disp + wt:disp, data = m)
m$pred_mpg <- predict(mod)

mgrid_df <- predictgrid(mod, "wt", "disp", "mpg")
mgrid_list <- df2mat(mgrid_df)

# make the plot with data points
plot3d(m$wt, m$disp, m$mpg, type = "s", size = .5, lit = F)

# Add the corresponding predicted points: smaller 
spheres3d(m$wt, m$disp, m$pred_mpg, alpha = .4, type = "s", size = .5, lit = F)

# Add a mesh of predicted values
surface3d(mgrid_list$wt, mgrid_list$disp, mgrid_list$mpg, alpha = .4, front = "lines", back = "lines")

# We can tweak the appearance of the graph. We'll add each compoments separately
plot3d(mtcars$wt, mtcars$disp, mtcars$mpg,
       xlab = "", ylab = "", zlab = "",
       axes = FALSE,
       size = .5, type = "s", lit=FALSE)
# Add the corresponding predicted points (smaller)
spheres3d(m$wt, m$disp, m$pred_mpg, alpha=0.4, type = "s", size = 0.5, lit=FALSE)
# Add line segments showing the error
segments3d(interleave(m$wt, m$wt),
           interleave(m$disp, m$disp),
           interleave(m$mpg, m$pred_mpg),
           alpha = 0.4, col = "red")
# Add the mesh of predicted values
surface3d(mpgrid_list$wt, mpgrid_list$disp, mpgrid_list$mpg,
          alpha = 0.4, front = "lines", back = "lines")
# Draw the box
rgl.bbox(color = "grey50", # grey60 surface and black text
         emission = "grey50", # emission color is grey50
         xlen = 0, ylen = 0, zlen = 0) # Don't add tick marks
# Set default color of future objects to black
rgl.material(color = "black")
# Add axes to specific sides. Possible values are "x--", "x-+", "x+-", and "x++".
axes3d(edges = c("x--", "y+-", "z--"),
       ntick = 6, # Attempt 6 tick marks on each side
       cex = .75) # Smaller font
# Add axis labels. 'line' specifies how far to set the label from the axis.
mtext3d("Weight", edge="x--", line = 2)
mtext3d("Displacement", edge="y+-", line = 3)
mtext3d("MPG", edge="z--", line = 3)





### Saving a 3D plot
# to save a bitmap image, use rgl.snapshot(). it will capture the exact image that is on the screen

plot3d(mtcars$wt, mtcars$disp, mtcars$mpg, type="s", size=0.75, lit=FALSE)
rgl.snapshot('3dplot.png', fmt='png')
# You can also use rgl.postscript() to save a Postscript or PDF file:
rgl.postscript('figs/miscgraph/3dplot.pdf', fmt='pdf')
rgl.postscript('figs/miscgraph/3dplot.ps', fmt='ps')

# postscript or pdf does not support many features of the opengl library on which rgl is bases
# to make the output more readable, you can save the current viewpoint and restore it later

# Save the current viewpoint
view <- par3d("userMatrix")

# Restore the saved viewpoint
par3d(userMatrix = view)

# use dput to save the view in a script
dput(view)

# once we have the text representation of the matrix, we can add the follwing to our script

view <- structure(c(0.907931625843048, 0.267511069774628, -0.322642296552658,
                    0, -0.410978674888611, 0.417272746562958, -0.810543060302734,
                    0, -0.0821993798017502, 0.868516683578491, 0.488796472549438,
                    0, 0, 0, 0, 1), .Dim = c(4L, 4L))
par3d(userMatrix = view)





### Animating a 3D plot
# to animate a 3d plot, use play3d or spin3d

plot3d(mtcars$wt, mtcars$disp, mtcars$mpg, type = "s", size = .75, lit = F)
play3d(spin3d())

# spin on the x axis, at 4 rpm, for 20 seconds
play3d(spin3d(axis = c(1, 0, 0), rpm = 4), duration = 20)
# to save the movie, use movie3d() function the same way as play3d. It will generate a series of png files and 
# combine them in a single animated gif
movie3d(spin3d(axis=c(0,0,1), rpm=4), duration=15, fps=50)





### Creating a dendogram
# use hclust and plot the output from it
c2 <- subset(countries, Year == 2009)
c2 <- c2[complete.cases(c2), ]

set.seed(201)
c2 <- c2[sample(1:nrow(c2), 25), ]
c2

rownames(c2) <- c2$Name
c2 <- c2[, 4:7]
# The values for GDP are several orders in magnitude larger than the values for infmortality
# because of this, effect of infmortality will be negetive when compared to effect of GDP
# So we scale the data
c3 <- scale(c2)
c3
# By default, scale uses standard deviation to scale data but other methods can be used
hc <- hclust(dist(c3))
plot(hc)
plot(hc, hang = -1) # to align text
?hclust





### Creating a vector field
# use geom_segment
isabel
# y and x are lat-long; z is the height in km; vx, vy, vz are the wind speed components respectively in each direction
# speed is the wind speed
summary(isabel)
# each segment has a starting point and an end point. We'll use the x and y as the starting points and end points 
# and add a fraction of the vx and vy values to get the end points for each segment. 
# If we didn't scale down these values, the lines would be too long
islice <- subset(isabel, z == min(z))

ggplot(islice, aes(x, y)) +
  geom_segment(aes(xend = x + vx/50, yend = y + vy/50), size = .25) # make the line segments .25 mm thick
# there are two problems with the data; it is too high resolution and the segments do not have 
# arrows indicating direction

# to reduce the resolution, we will define a function that will keep one out of every n values in the data and 
# drops the rest

every_n <- function(x, by = 2) {
  x <- sort(x)
  x[seq(1, length(x), by = by)]
}

# keep 1 out of every 4 values in x and y
keepx <- every_n(unique(islice$x), by = 4)
keepy <- every_n(unique(islice$y), by = 4)

islice_sub <- subset(islice, x %in% keepx & y %in% keepy)

# we need to load the arrow function from grid
library(grid)
ggplot(islice, aes(x, y)) +
  geom_segment(aes(xend = x + vx/50, yend = y + vy/50), arrow = arrow(length = unit(0.1, "cm")), size = .25)
# one effect of arrowheads is that short vectors appear with more ink than is proportional to their length
# To mitigate this, it may be useful to map the speed to other properties like size, alpha or colour
# We'll map the speed to alpha here
# the existing speed includes the z component; we'll calculate the speedxy, the horizontal speed
islice_sub$speedxy <- sqrt(islice_sub$vx^2 + islice_sub$vy^2)

# map speed to alpha
ggplot(islice, aes(x, y)) +
  geom_segment(aes(xend = x + vx/50, yend = y + vy/50, alpha = speed), 
               arrow = arrow(length = unit(0.1, "cm")), size = .25)

# next, we will map the speed to colour; we'll also add a map of the US and zoom in on the area of interest using
# coord_cartesian (without this, the entire US will be shown)
usa <- map_data("usa")

# map speed to colour and set go from grey80 to darkred
ggplot(islice_sub, aes(x, y)) +
  geom_segment(aes(xend = x + vx/50, yend = y + vy/50, colour = speed), 
               arrow = arrow(length = unit(0.1, "cm")), size = .6) +
  scale_colour_continuous(low = "grey80", high = "darkred") + 
  geom_path(aes(x = long, y = lat, group = group), data = usa) +
  coord_cartesian(xlim = range(islice_sub$x), ylim = range(islice_sub$y))

# the isabel dataset has 3d data so we can also make faceted graph of the data;
# because each facet is small, we'll use a sparser subset than before
keepx <- every_n(unique(isabel$x), by = 5)
keepy <- every_n(unique(isabel$y), by = 5)
keepz <- every_n(unique(isabel$z), by = 2)

isub <- subset(isabel, x %in% keepx & y %in% keepy & z %in% keepz)

ggplot(isub, aes(x, y)) + 
  geom_segment(aes(xend = x + vx/50, yend = y + vy/50, colour = speed),
               arrow = arrow(length = unit(.1, "cm")), size = .5) +
  scale_color_continuous(low = "grey80", high = "darkred") + facet_wrap( ~ z)





### Creating a QQ plot (quantile-quantile plot)
# to compare an empirical distribution to a theoretical one
# use qqnorm to compare a normal distribution. give qqnorm a vector of numerical values and add a theoretical 
# distribution line with qqline()

## QQ plot of height
qqnorm(heightweight$heightIn)
qqline(heightweight$heightIn)

# QQ plot of the age
qqnorm(heightweight$ageYear)
qqline(heightweight$ageYear)

# the points for the heightIn are close to the line, which means that the distribution is close to normal
# the points for ageYear are far away from the line, esp. on the left indicating that the distribution is skewed
# A histogram may also be useful to explore how the data is distributed




### Creating a graph of an empirical cumulative distribution function 
# use stat_ecdf(s)
ggplot(heightweight, aes(heightIn)) + stat_ecdf()

# ecdf of ageYear
ggplot(heightweight, aes(ageYear)) + stat_ecdf()

# ECDF shows what proportion of population are at or below a certain x value




### Creating a mosaic plot
# use mosaic function from the vcd package
install.packages("vcd")
library(vcd)
UCBAdmissions

# Print a flat contingency table
ftable(UCBAdmissions)
dimnames(UCBAdmissions)

# To visualize the relationships between variables, use mosaic() and pass it a formula with the variables
# that will be used to split up the data
# Split by admit, then by gender and then by dept
mosaic( ~ Admit + Gender + Dept, data = UCBAdmissions)

# Mosaic splits the data in the order in which the variables are provided
# It is difficult to make comparisons between different departments so a different split may be useful

mosaic( ~ Dept + Gender + Admit, data = UCBAdmissions, highlighting = "Admit", 
        highlighting_fill = c("lightblue", "pink"), direction = c("v", "h", "v"))
# It is easier to compare the male and female groups within each department
# we can use different splitting directions

# Another possible set of splitting directions
mosaic( ~ Dept + Gender + Admit, data = UCBAdmissions, highlighting = "Admit",
        highlighting_fill = c("lightblue", "pink"), direction = c("v", "v", "h"))

# This order makes it difficult to compare male and female
mosaic( ~ Dept + Gender + Admit, data = UCBAdmissions, highlighting = "Admit",
        highlighting_fill = c("lightblue", "pink"), direction = c("v", "h", "h"))

# The example here is a case of simpson's paradox in which a relationship between the groups can change 
# or even reverse when combined. Here, the difference in admition rates were because women were more likely to
# apply to departments with higher rejection rates




### Creating a pie chart
# use the pie function
survey
fold <- table(survey$Fold)
fold
pie(fold)
# We passed pie on an object of class table. We could have instead given it a vector

pie(c(99, 18, 120), labels = c("L on R", "Neither", "R on L"))






### Creating a map
# retrieve map data from the maps package and draw it with geom_polygon or geom_path
# By default, the latitude and longitude will be drawn on a cartesian coordinate plane but you can use coord_map
# to specify a projection. The default projection is mercator, which unlike a cartesian plane has a progressively
# changing spacing for latitude lines
library(maps)
# Get map data for USA
states_map <- map_data("state")

ggplot(states_map, aes(long, lat, group = group)) + geom_polygon(fill = "white", colour = "black")

# geom_path and no fill and mercator projection
ggplot(states_map, aes(long, lat, group = group)) + geom_path() + coord_map("mercator")

# To get the map data for the world
world_map <- map_data("world")
world_map

# To draw the map of a region, look for the region name
sort(unique(world_map$region))

# It's possible to get the data for specific regions from a particular map
east_asia <- map_data("world", region = c("Japan", "China", "North Korea", "South Korea"))

# map region to fill colour
ggplot(east_asia, aes(long, lat, group = group, fill = region)) + geom_polygon(colour = "black") +
  scale_fill_brewer(palette = "Set2")

# If there is a separate map available for a region, such as nz (New Zealand), that map data
# will be at a higher resolution than if you were to extract it from the world map, as shown

# Get New Zealand data from world map
nz1 <- map_data("world", region = "New Zealand")
nz1 <- subset(nz1, long > 0 & lat > -48) # Trim off islands
ggplot(nz1, aes(x = long, y = lat, group=group)) + geom_path()

# Get New Zealand data from the nz map
nz2 <- map_data("nz")
ggplot(nz2, aes(x = long, y=lat, group = group)) + geom_path()

?mapproject





### Creting a choropleth map
# you want to create a map with the regions that are coloured according to the variable values
# Merge the value data with the map data, then map a varaible to fill

# Transform the USArrests data to the correct format
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
crimes

library(maps)
states_map <- map_data("state")

# merge the datasets together
crime_map <- merge(states_map, crimes, by.x = "region", by.y = "state")

# Note that the order is very important as it leads to polygons drawn in the wrong order. So, we sort the data
head(crime_map)
crime_map <- arrange(crime_map, group, order)
head(crime_map)

ggplot(crime_map, aes(long, lat, group = group, fill = Assault)) + geom_polygon(colour = "black") +
  coord_map("polyconic")

# If you want to show how the values diverge from some middle value, use scale_fill_gradient2
ggplot(crimes, aes(map_id = state, fill=Assault)) + geom_map(map = states_map, colour="black") +
  scale_fill_gradient2(low = "#559999", mid = "grey90", high = "#BB650B", midpoint = median(crimes$Assault)) +
  expand_limits(x = states_map$long, y = states_map$lat) + coord_map("polyconic")

# We can also use discrete values as we used continuous
# we can convert the values into quantiles and show those quantiles
qa <- quantile(crimes$Assault, c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
qa

# Add a column of the quantile category
crimes$Assault_q <- cut(crimes$Assault, qa, labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                        include.lowest = T)
crimes

# generate a discrete colour palette with 5 values
pal <- colorRampPalette(c("#559999", "grey80", "#BB650B"))(5)

ggplot(crimes, aes(map_id = state, fill = Assault_q)) + geom_map(map = states_map, colour = "black") +
  scale_fill_manual(values = pal) + expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("polyconic") + labs(fill = "Assault Rate\nPercentile")

# Another way to make a choropleth without needing to merge the map data with the value data is to use geom_map
# For this, the map data must have columns names lat, long and region. In the value data frame, there must be 
# a column matched to the region column in the map data frame and this column is specified by mapping it to the
# map_id aesthetic

# The 'state' column in the crimes data is to be matched to the 'region' column
# in the states_map data
ggplot(crimes, aes(map_id = state, fill=Assault)) +
  geom_map(map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("polyconic")





### Making a map with a clean background
# use the following theme
theme_clean <- function(base_size = 12) {
  require(grid) # Needed for unit() function
  theme_grey(base_size) %+replace%
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.ticks.length = unit(0, "cm"),
      axis.ticks.margin = unit(0, "cm"),
      panel.margin = unit(0, "lines"),
      plot.margin = unit(c(0, 0, 0, 0), "lines"),
      complete = TRUE
    )
}
# then add it to the map
ggplot(crimes, aes(map_id = state, fill = Assault_q)) + geom_map(map = states_map, colour = "black") +
  scale_fill_manual(values = pal) + expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("polyconic") + labs(fill = "Assault Rate\nPercentile") + theme_clean()





### Creating a map from a shapefile
# load the shapefile using readShapePoly() from an ESRI shapefile from the maptools package`