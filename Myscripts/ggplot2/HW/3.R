head(mpg)
qplot(x = displ, y = hwy, data = mpg, colour = factor(cyl))
qplot(x = displ, y = hwy, data = mpg, colour = cyl) #gives shades of blue
?geom_smooth
qplot(x = displ, y = hwy, data = mpg, facets = . ~ year) + geom_smooth()
?qplot
?geom_smooth
?scale_