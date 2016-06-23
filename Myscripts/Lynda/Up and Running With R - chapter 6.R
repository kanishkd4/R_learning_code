# CORRELATION

google <- read.csv(file = "C:\\Users\\309292\\Desktop\\R\\Lynda files\\R\\Exercise1\\Ch06\\06_01\\google_correlate.csv", header = T)

# we can find correlation for individual sets or group of variables
str(google)
g.quant <- google[c(3, 7, 4, 5)]

# Correlation matrix for dataset
cor(g.quant)
# we get correlations for the whole data frame

# cor.test is another one that can do one correlation at a time and give a significance level 
# p value and confidence interval

cor.test(g.quant$data_viz, g.quant$degree)
# we get pearson product momment correlation (maybe we could get more)
# it gives us a t test for sigificance test and gives us degrees of freedom
# 95% confidence interval is from 0.69 to 0.84
# good way to get one correlation at a time

# if we want to do the entire matrix at once, we can get a probability matrix by using Hmisc

install.packages("Hmisc")
library(Hmisc)

# we will coerce the data into a matrix before we run the correlation as the matrix will have all the data
# as the same type

rcorr(as.matrix(g.quant))
# the correlation matrix is the same as before
# the second matrix also gives probability values. 
# for statistical significance, the value of P must be less than 0.05
# P value for data_viz and degree, the P value is significant but between degree and NBA is not significant





# REGRESSION

# using the same google correlate data
names(google)
str(google)
# we will predict interest in data visualization
# how common that term is relative to other searches on a state by state basis
# stats_ed is a yes/no variable entered as text
# region is a categorical variable 
# we will create a multiple regression model

reg1 <- lm(data = google, data_viz ~ degree + stats_ed + nba + has_nba + region)

# to see results 
summary(reg1)

# we have a decently big output
# residuals are a way of asesing how well the model fits the data 
# then the coefficients.. intercept and then the variables
# there are 3 region variables. It automatically puts stats_ed a sstats_edyes; same for has_nba
# it takes 3 out of 4 regions to avoid multicolinearity. have to leave one out to avoid it

# then we look at the columns estimate, std.error and t value and probability value 
# which is the significance test - Pr(>|t|)
# it puts * next to the ones that are statistically significant
# people with college degrees show a higher interest in serching for data visualization
# negetive correlation with facebook
# more degrees, more interest in data visualisation; more interest in  facebook, less interest in data visualization

# multiple R-squared is important. It is 65%. 65% of the variance can be predicted by the variables
# adjusted R-Squared is also given
# F-statistic is given and can be used as an inferrencial test for that R-Squared and confirms it is statistically significant




# CROSS TAB
# cross tabulation or a chi square test of significance for 2 categorical varibles

sn <- read.csv(file = "C:\\Users\\309292\\Desktop\\R\\Lynda files\\R\\Exercise1\\Ch06\\06_03\\social_network.csv", 
               header = T)

names(sn)
# we will see association between gender and site
# create a contingency table

sn.tab <- table(sn$Gender, sn$Site)
sn.tab # this is the count of all categorical variables

# we can also get marginal frequencies

margin.table(sn.tab, 1) # row marginal frequencies
margin.table(sn.tab, 2) # column marginal frequencies

# we can also get the proportion of the people using these
round(prop.table(sn.tab), 2) # cell %
round(prop.table(sn.tab, 1), 2) # row %
round(prop.table(sn.tab, 2), 2) # columns %

# chi square test
chisq.test(sn.tab)
# value of chi square is 13.2; p value is 0.02, less than 0.05 so the difference is statistically significant
# a warning message at the botton is cause the sample is very small
# this shows that there are gender differnces in social media preferrences



# T TEST
# one common inferential test is to compare two groups on a single quantitative outcome
# the simplest way to do this is a t test

google <- read.csv(file = "C:\\Users\\309292\\Desktop\\R\\Lynda files\\R\\Exercise1\\Ch06\\06_01\\google_correlate.csv", header = T)

names(google)
# Does interest in NBA as a search term differ between states that have a basketball teams and ones that don't

# independant two group t test
t.test(google$nba ~ google$has_nba)

# it is using a welch two sample t test
# value for t is -4.755; welch test gives fractional degrees of freedom
# p value is very small and it is statictically significant
# the 95% confidence interval shows the difference betweent the two groups is between -1.6 to -0.65
# since this is based on something that is an average of 0 for the nation, this is a reasonable size
# it gives us a mean for the groups; the mean in group no is -0.5; half a stdev above the mean
# the group with an Nba team has a mean of 0.6; half a stdev above the mean



# ANOVA
# can compare several groups on a single quantitiative outcome
names(google)

# One way anova
anova1 <- aov(data_viz ~ region, data = google)
summary(anova1)
# we have the dof of the model based on region (3 dof)
# we have the sum of squaresm mean squares, f value and the p value
# if the p value of less than 0.05, it is statistical significant
# the difference between the means here is considered a random fluctuation and not statistically reliable

# two way factorial design (or two way classification)

anova2a <- aov(data_viz ~ region + stats_ed + region:stats_ed, data = google)
# region:stats_ed is used as region and whether a state has a stats ed curriculum
# it is a way of specifying the interactionl; important when you do a factorial analysis of variance
summary(anova2a)

# we have several lines; one for region and one for stats_ed; is there a differnce by region by itself
# or stats_ed by itself? the third is region:stats_ed. Does the average score for region depend on whether they have a stats_ed or not?
# p values show it is statistically insignificant


# there is another way to do the same test

anova2b <- aov(data_viz ~
                 region*stats_ed,
               data = google)
summary(anova2b)
# this gives the exact same results; the 2nd version is easier to set up
# the effects here is not statistically significant

# ANOVA can be a really good way to look at grouped differences on a quantitative variable





