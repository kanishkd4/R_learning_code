library(plyr)
library(dplyr)

x <- read.table(file = "clipboard", header = T, sep = "\t")
names(x)
str(x)
x <- arrange(x, APP_ID_C, Reject.Reasons)

y <- x %>% group_by(APP_ID_C) %>% summarise(
  rej = paste(Reject.Reasons, collapse = "~")
    )
write.csv(y, "reject reason.txt", quote = F, row.names = F)
