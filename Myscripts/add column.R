
XY <- Non.APF
str(XY)
unique(XY$UNIT_TYPE)
rm(XY)

xy1 <- c("BUNGALOW", "LAND", "DUPLEX", "TRIPLEX", "ROW HOUSE")
xy2 <- c("GALA", "SHOP", "OFFICE", "FLAT", "PENT HOUSE", "FLOOR")
rm(xy1, xy2)
if(XY$UNIT_TYPE == xy1) {XY$Price = XY$LANDAREA_RATE}
if(XY$UNIT_TYPE == xy2) {XY$Price = XY$SELLABLEAREA_RATE}
if(XY$UNIT_TYPE == "GALA", "SHOP", "OFFICE", "FLAT", "PENT HOUSE", "FLOOR") {XY$Price = XY$SELLABLEAREA_RATE}

if(XY$UNIT_TYPE == "BUNGALOW") {XY$Price = XY$LANDAREA_RATE}

if(is.null(XY$LANDAREA_RATE) & is.null(XY$LANDAREA_RATE)) {XY$Price = XY$LAND_RATE}

price <- function(x) {
  if(XY$UNIT_TYPE == xy1) {XY$Price = XY$LANDAREA_RATE}
  return(XY$PRICE)
}
price(XY)
rm(price)

addp <- function(UT) {
  if (XY$UNIT_TYPE == xy1) {XY$Price = XY$LANDAREA_RATE}
  else if(XY$UNIT_TYPE == xy2) {XY$Price = XY$SELLABLEAREA_RATE}
  else if(is.null(XY$LANDAREA_RATE) & is.null(XY$LANDAREA_RATE)) {XY$Price = XY$LAND_RATE}
  else XY$Price = 0
}
addp(XY)

XY$DIff <- XY$Price - XY$LANDAREA_RATE
?ifelse

if(XY$UNIT_TYPE == xy1) {XY$Price <- XY$LANDAREA_RATE}


ifelse ((XY$UNIT_TYPE == xy1), mutate(XY, XY$Price <- XY$LANDAREA_RATE))
ifelse ((XY$UNIT_TYPE == xy2), mutate(XY, XY$Price <- XY$SELLABLEAREA_RATE))
ifelse (!is.null(XY$LAND_RATE) & is.null(XY$LANDAREA_RATE), mutate(XY, XY$Price <- XY$LAND_RATE))

unique(XY.1$UNIT_TYPE)
XY.1 <- filter(XY, UNIT_TYPE == c("BUNGALOW", "LAND", "DUPLEX", "TRIPLEX", "ROW HOUSE"))
XY.2 <- filter(XY, UNIT_TYPE == xy2)
XY.3 <- filter(XY, !is.null(XY$LAND_RATE) & is.null(XY$LANDAREA_RATE))

table(XY.1$UNIT_TYPE)
               
XY.1 <- XY[XY$UNIT_TYPE == xy1, ]
xy1
############



XY <- Non.APF
table(XY$UNIT_TYPE)
table(XY.B$UNIT_TYPE)
XY.1 <- filter(XY, UNIT_TYPE == c("BUNGALOW", "LAND", "DUPLEX", "TRIPLEX", "ROW HOUSE"))
XY.B <- filter(XY, UNIT_TYPE == "BUNGALOW")











