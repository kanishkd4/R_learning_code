singer <- read.table(file = "clipboard", header = T, sep = "\t") # Rock.xls

head(singer)
table(singer$Singer) # how many songs were sung by each singer

# how many songs were sung not by Eminem
?table
table(singer$Singer, exclude = "Eminem")

f1 <- function(x){
  y <- data.frame(table(singer$Singer, exclude = x))
  sum(y$Freq)
}
f1("Eminem") # this works!!!.. might be a better way with the apply family of functions

sum(data.frame(table(filter(singer, Singer != "Eminem")$Singer))$Freq) # this is better!!!!!

# how many songs were at least 4 minutes long
sum(data.frame(table(filter(singer, Minutes >=4)$Singer))$Freq)

# how many songs were longer than the average lenght of all songs in the list
sum(data.frame(table(filter(singer, Minutes > mean(Minutes))$Singer))$Freq)

# how many songs were sung by singers whose last names begin with S
?grep

sum(data.frame(table(singer$Singer[grep('^S', singer$Singer)]))$Freq)

length(singer$Singer)

# singer has 6 letters in his name

sum(data.frame(table(singer$Singer[grep('^[a-z][a-z][a-z][a-z][a-z][a-z]',
                                        singer$Singer, ignore.case = T)]))$Freq)# works for at least 6 

sum(data.frame(table(singer$Singer[grep('^[a-z]{6}', singer$Singer, ignore.case = T)]))$Freq)
# the above works for exactly 6!!


