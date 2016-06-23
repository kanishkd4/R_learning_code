# http://www.r-bloggers.com/two-way-analysis-of-variance-anova/


delivery.df <- data.frame(
  Service = c(rep("Carrier 1", 15), rep("Carrier 2", 15),
              rep("Carrier 3", 15)),
  Destination = c(rep(c("Office 1", "Office 2", "Office 3",
                        "Office 4", "Office 5"), 9)),
  Time = c(15.23, 14.32, 14.77, 15.12, 14.05,
           15.48, 14.13, 14.46, 15.62, 14.23, 15.19, 14.67, 14.48, 15.34, 14.22,
           16.66, 16.27, 16.35, 16.93, 15.05, 16.98, 16.43, 15.95, 16.73, 15.62,
           16.53, 16.26, 15.69, 16.97, 15.37, 17.12, 16.65, 15.73, 17.77, 15.52,
           16.15, 16.86, 15.18, 17.96, 15.26, 16.36, 16.44, 14.82, 17.62, 15.04)
)

library(ggplot2)
ggplot(delivery.df, aes(Time, Destination, colour = Service)) + geom_point()


# To fit the two-way ANOVA model we use this code:
delivery.mod1 <- aov(Time ~ Destination*Service, data = delivery.df)

# * indicates that it should use the main effects as well as the interaction between the variables

summary(delivery.mod1)

# We have strong evidence here that there are differences between the three delivery services, 
# between the five sub-office destinations and that there is an interaction between destination and 
# service in line with what we saw in the original plot of the data.

# We can plot the model residuals against fitted values to look for obvious trends that are not 
# consistent with the model assumptions about independence and common variance. The first step 
# is to create a data frame with the fitted values and residuals from the above model:

delivery.res <- delivery.df
delivery.res$M1.Fit <- fitted(delivery.mod1)
delivery.res$M1.Resid <- resid(delivery.mod1)

ggplot(delivery.res, aes(M1.Fit, M1.Resid, colour = Service)) + geom_point() +
  xlab("Fitted Values") + ylab("Residuals")


# Alternate plot

ggplot(delivery.res, aes(M1.Fit, M1.Resid, colour = Service)) +
  geom_point() + xlab("Fitted Values") + ylab("Residuals") +
  facet_wrap( ~ Destination)


#We could also consider dividing the data by delivery service to get a different view of the 
# residuals:
  
ggplot(delivery.res, aes(M1.Fit, M1.Resid, colour = Destination)) +
geom_point() + xlab("Fitted Values") + ylab("Residuals") +
facet_wrap( ~ Service)

TukeyHSD(delivery.mod1, which = "Service")
TukeyHSD(delivery.mod1, which = "Destination")
TukeyHSD(delivery.mod1)



delivery.mod2 <- aov(Time ~ Service, data = delivery.df)
TukeyHSD(delivery.mod2)




