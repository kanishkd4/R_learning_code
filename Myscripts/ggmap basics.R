# taking location/distance into account
library(ggmap)
library(ggplot2)
options(timeout = 20000)

murder <- subset(crime, offense == "murder")
qmplot(lon, lat, data = murder,
       colour = I('red'), size = I(3), darken = .3)
baylor <- "baylor university"
qmap(baylor, zoom = 14, source = "google")

qmap(baylor, zoom = 14, source = "osm")
set.seed(500)
df <- round(data.frame(
  x = jitter(rep(-95.36, 50), amount = .3),
  y = jitter(rep( 29.76, 50), amount = .3)
), digits = 2)
map <- get_googlemap(center = 'houston', markers = df, path = df, scale = 2)
ggmap(map)