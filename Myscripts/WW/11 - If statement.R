library(dplyr)

# TO CHECK NA's
# use is.na(x)


if1 <- read.table(file = "clipboard", header = T, sep = "\t")# ifstatement.xls - quantity discount
str(if1)
if2 <- mutate(if1, 
              cost = ifelse(order.quantity <= 500, order.quantity * 3, 
                            ifelse(order.quantity <= 1200, order.quantity * 2.7,
                                   ifelse(order.quantity <= 2000, order.quantity * 2.3,
                                          order.quantity * 2))),
              price = cost/order.quantity
              )


if1 <- read.table(file = "clipboard", header = T, sep = "\t")# ifstatement.xls - hedging
if1$final.stock.price <- sub(pattern = "\\$", x = if1$final.stock.price, replacement = "")
if1$final.stock.price <- as.numeric(if1$final.stock.price)
str(if1)

nputs <- 60
nshares <- 100
exprice <- 45
pricenow <- 55
startvalue <- 5800

if2 <- mutate(if1,
              fvp = ifelse(final.stock.price <= exprice, (exprice - final.stock.price)*nputs, 0),
              fvs = nshares * final.stock.price,
              prh = ((fvp + fvs)-startvalue)/startvalue,
              ph = (fvs - nshares * 55)/(nshares*pricenow)
              ) # never use <- in mutate; always =

# using match

a <- read.table(file = "clipboard", header = T, sep = "\t") # errortrap.xls
b <- read.table(file = "clipboard", header = T, sep = "\t") # errortrap.xls  
str(a)
str(b)
?match
b <- mutate(b,
            salary <- a$sal[match(x = b$Name, table = a$nam)]
            )

wm <- read.table(file = "clipboard", header = T, sep = "\t") # toyrev.xls
wm
str(wm)
wm$revenue <- gsub(pattern = ",", x = wm$revenue, replacement = "")
wm$revenue <- as.numeric(wm$revenue)
str(wm)

?nrow
?row
?Mod
