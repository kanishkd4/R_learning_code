#Loops and functions

#Importing Data
setwd("C:\\Users\\309292\\Desktop\\R\\RBook")
Owls <- read.table(file = "Owls.txt", header = TRUE)
names(Owls)
str(Owls)
unique(Owls$Nest)

#Scatterplot and labeling
Owls.ATV <- Owls[Owls$Nest == "AutavauxTV",]
plot(x = Owls.ATV$ArrivalTime, y = Owls.ATV$NegPerChick,
     xlab = "Arrival Time", ylab = "Negotiation Behaviour", main = "AutavauxTV")

#Designing General Code
#remove ATV
Owls.i <- Owls[Owls$Nest == "Bochet",]
plot(x = Owls.i$ArrivalTime, y = Owls.i$NegPerChick,
     xlab = "Arrival Time", ylab = "Negotiation Behaviour", main = "Bochet")

#Remove nest name
Nest.i <- "Bochet"
Owls.i <- Owls[Owls$Nest == Nest.i,]
plot(x = Owls.i$ArrivalTime, y = Owls.i$NegPerChick,
     xlab = "Arrival Time", ylab = "Negotiation Behaviour", main = Nest.i)

#Now, to make a different plot, we just need to change the nest name while naming nest.i

#saving file in a jpeg graph

setwd("C:\\Graphs")
Nest.i <- "Bochet"
Owls.i <- Owls[Owls$Nest == Nest.i,]
FileName <- paste(Nest.i, ".jpg", sep="")
jpeg(file = FileName)
plot(x = Owls.i$ArrivalTime, y = Owls.i$NegPerChick,
     xlab = "Arrival Time", ylab = "Negotiation Behaviour", main = Nest.i)
dev.off()

#Creating the Loop

AllNests <- unique(Owls$Nest)
for (i in 1:27){
  Nest.i <- AllNests[i]
  Owls.i <- Owls[Owls$Nest == Nest.i, ]
  FileName <- paste(Nest.i,".jpg", sep = "")
  jpeg(file = FileName)
  plot(x = Owls.i$ArrivalTime, y = Owls.i$NegPerChick,
       xlab = "Arrival Time", ylab = "Negotiation Per Chick", main = Nest.i)
  dev.off()
}

