data <- read.csv(file = "D:\\Projects\\Others\\Small\\develop cibil query\\data.csv", header = T, check.names = F)
str(data)

names(data) = gsub(pattern = "'", x = names(data), replacement = "")
data1 <- data.frame(lapply(1:6, FUN = function(x) gsub(pattern = "'", x = data[, x], replacement = "")))
names(data1) <- names(data)
rm(data1)
data1$score_final_num <- as.numeric(as.character(data1$score_final))
data1$sanction_date1 <- as.character(data1$sanction_date)
data1$sanction_date1 <- gsub(pattern = "00:00:00.000000", x = data1$sanction_date1, replacement = "")
data1$sanction_date1 <- as.Date(data1$sanction_date1)

library(dplyr)

data1 <- mutate(data1,
                CIBIL.Band = ifelse(score_final_num < 100, "-1 to 100",
                                    ifelse(score_final_num < 300, "100 to 300",
                                           ifelse(score_final_num < 600, "300 to 600",
                                                  ifelse(score_final_num < 800, "600 to 800",
                                                         ifelse(score_final_num >= 800, ">800", "NA"))))),
                Year_sanction = year(sanction_date1))


z <- acast(data = data1, CIBIL.Band ~ Year_sanction, fun.aggregate = length, margins = T)

z1 <- acast(data = data1, flag ~ Year_sanction, fun.aggregate = length, margins = T)
edit(z)
write.csv(x = z, file = "score to year.csv", quote = F)
write.csv(x = z1, file = "flag to year.csv", quote = F)
