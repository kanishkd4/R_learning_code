df <- data.frame(population=c(100, 300, 5000, 2000, 900, 2500), 
                 habitat=c(1,2,3,4,5,6))
mysize <- function(x){
  if(x<500)
    return(1)
  if(500 <= x & x < 1000)
    return(2)
  if(1000<=x & x<2000)
    return(3)
  if(2000<=x & x<3000)
    return(4)
  if(3000<=x & x <=5000)
    return(5)
  else
    return(NA)
}

table$population.bin <- sapply(table$population, mysize)
table