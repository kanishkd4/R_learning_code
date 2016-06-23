
f1 <- function(demand, price = 3, unit.cost = 0.45, fixed.cost = 45000){
  profit = (demand * price) - fixed.cost - (unit.cost*demand)
  return(profit)
}
f1(10000)

# find the break even point
# f3 works and does not take too long!!! ( need to find a better way)
f3 <- function(x){
  i = 1
  repeat{
    i = i + 0.01
  profit = (i * 3) - 45000 - (0.45*i)
  if(profit >= 0)
    break
  }
  print(c(demand = i, revenue = i*3, variable.cost = 0.45*i))
}
f3()

# another way would be to plot all values in a data frame and look for 0
x <- data.frame(1:100000)
x$p <- sapply(1:100000, FUN = f1)
# not sure how to search

# pay off mortgage in 15 years. rate is 6%. EMI = 2000. How much can we borrow?
library(tvm)
pmt(amt = 10000, maturity = 15, rate = 6/1200)

?pmt
f4 <- function(x){
  i = 10000
  repeat{
    i = i+1/10
    inst = pmt(amt = i, maturity = 15*12, rate = 6/1200)
    if(inst >= 2000)
      break
  }
  print(c(amt = i))
}
f4() # works. need something better than brute force


