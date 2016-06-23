# taking location/distance into account
library(ggmap)
library(ggplot2)
install.packages("ggmap")
devtools::install_github("dkahle/ggmap")
library(leaderCluster)

distances <- base %>% select(as.character(From), as.character(To)) %>% distinct()
distances$kmdist <- mapdist(as.character(distances$From), as.character(distances$To), mode = "driving")$km
base$distance <- distances$kmdist[match(base$From.To.Place, distances$From.To)]

?geocode
x <- data.frame(geocode("ICICI Bank Ltd., Shilp Annexe, New C.G.Road, Chankheda, Ahmedabad 382424,Gujarat"))
geocode("Ahmedabad")

cpc <- read.table(file = "clipboard", sep = "\t", header = T)
cpc$Address <- gsub("-|-", "", cpc$Address)
cpc$lat <- geocode(as.character(cpc$Address))$lat
cpc$lon <- geocode(as.character(cpc$Address))$lon


SOL <- read.csv(file = "D:\\Projects\\HL\\CPC Exploration\\Final\\SOL_ID_clusters v4.csv", header = T)

SOL$cluster <- leaderCluster(SOL[, 10:11], 15, distance = "haversine")$cluster_id

?qmplot
names(SOL)
qmplot(data = subset(SOL, cluster == 1), x = SOLID_lon, y = SOLID_lat, source = "google")

murder <- subset(crime, offense == "murder")
qmplot(lon, lat, data = murder,
       colour = I('red'), size = I(3), darken = .3)

baylor <- "baylor university"
qmap(baylor, zoom = 14)
qmap(baylor, zoom = 14, source = "osm")
library(dplyr)
map <- get_googlemap(center = "India", markers = filter(SOL, cluster == 1)[, 10:11],
                     path = filter(SOL, cluster == 1)[, 10:11])
ggmap(map)
qmap(baylor, zoom = 14, maptype = 53428,
     api_key = api_key, source = "cloudmade")


ggmap(get_googlemap(center = "India"))


qmap("Mumbai", zoom = 14)
