XY <- Non.APF
xy1 <- c("BUNGALOW", "LAND", "DUPLEX", "TRIPLEX", "ROW HOUSE")
xy2 <- c("GALA", "SHOP", "OFFICE", "FLAT", "PENT HOUSE", "FLOOR")
price <- rep(0, length = nrow(XY))
for(i in 1:400) { #nrow(XY))
  if(XY[i,9] %in% xy1 == TRUE ) {
    price[i] = XY[i,5]
    } else {
      if(XY[i,9] %in% xy2 == TRUE ) {
        price[i] = XY[i,7]
        } else 
          price[i] = XY[i,4]
        }

final_data=cbind(XY,price)

