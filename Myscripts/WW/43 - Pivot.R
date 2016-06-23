groc <- read.table(file = "clipboard", header = T, sep = "\t", quote = "")
sum(groc$Revenue)
str(groc)
?read.table

groc <- read.csv(file = "C:\\Users\\309292\\Desktop\\Kanishk\\Practice\\Winston 2013 files\\Ch43\\Practice Files\\asasas.csv",
              header = T)

m <- melt(data = groc, id.vars = c("Year", "Group", "Product", "Store", "Month"))
# getting warnings that attributes are not identical across measure variables; they will be dropped
# melt deletes a part of the data if we get this error

 unique(m$variable)
 dcast(m, Year + Group + Product ~ variable, sum) # fig 43-5  on page 393
 