
# entering data manually
# use x <- scan() 
UCBAdmissions
# this is an atomic vector
# a boxplot and $ won't work for atomic vectors

plot(UCBAdmissions)
margin.table(UCBAdmissions, 1) # Admit
margin.table(UCBAdmissions, 2) # gender
?margin.table
?prop.table
# prop.table has too many decimals. round up

?as.data.frame.table
admit1 <- as.data.frame.table(UCBAdmissions)
admit2 <- lapply(admit1, function(x) rep(x, admit1$Freq))
admit3 <- as.data.frame(admit2)
admit4 <- admit3[, -4] # remove the 4th column with freq

# to do that in one step
admit.rows <- as.data.frame(lapply(as.data.frame.table(UCBAdmissions), 
                                   function(x) rep(x, as.data.frame.table(UCBAdmissions)$Freq)))[, -4]
str(admit.rows)
