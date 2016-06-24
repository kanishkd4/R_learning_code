rm(list = ls())

# Suppose XX is a normally distributed random variable with a mean 0.050.05 and a variance (0.10)2(0.10)2. Start by computing some probabilities.
mu_x <- 0.05
sigma_x <- 0.1
  
# Pr(X > 0.10)
?pnorm  
pnorm(q = .10, mean = mu_x, sd = sigma_x, lower.tail = F)

# Pr(X < -0.10)
pnorm(q = -.10, mean = mu_x, sd = sigma_x, lower.tail = T)

  
# Pr(-0.05 < X < 0.15)
1 - pnorm(q = -.05, mean = mu_x, sd = sigma_x, lower.tail = T) - 
  pnorm(q = .15, mean = mu_x, sd = sigma_x, lower.tail = F)


# Compute quantiles
# Continue with computing quantiles of the normally distributed random variable XX with a mean 0.05 and a variance (0.10)^2(0.10)^2.

# quantiles can be computed with qnorm
# The mean (mu_x) and the standard deviation (sigma_x) are still in your workspace

# 1%, 5%, 95% and 99% quantile
qnorm(mean = mu_x, sd = sigma_x, p = c(0.01, 0.05, 0.95, 0.99))


# Compute densities
# we can use dnorm to calculate normal densities

# Normally distributed monthly returns
x_vals <- seq(-0.25, 0.35, length.out = 100)
MSFT <- dnorm(x = x_vals, mean = .05, sd = .1)
SBUX <- dnorm(x = x_vals, mean = .025, sd = .05)

# Plot normal curve
# MSFT and x_vals are still in your workspace

# Normal curve for MSFT
plot(x_vals, MSFT)
plot(x_vals, MSFT, type = "l", col = "blue", ylab = "Normal curves", ylim = c(0, 8))


# Add a normal curve for SBUX
lines(x_vals, SBUX, col = "red")

# Add a plot legend
legend("topleft", legend = c("Microsoft", "Starbucks"), 
       col = c("blue", "red"), lty = 1)

# Determine the value-at-risk of simple monthly returns

# value at risk intro:
# If R is the simple return from microsoft and R ~ N(0.05, (0.10)^2)
# Goal: Calculate how much we can lose with probability alpha
# we can use qnorm to get the quantile of value at risk

# VaR for continuously compounded returns
# r = ln(1 + R)

# Determine the value-at-risk of simple monthly returns
# 

# R ~ N(0.04, (0.09)^2) 
mu_R <- 0.04
sigma_R <- 0.09
  
# Initial wealth W0 equals $100,000
W0 <- 100000
  
# The 1% value-at-risk
W0 * qnorm(0.01, mu_R, sigma_R)  
  
# The 5% value-at-risk
W0 * qnorm(0.05, mu_R, sigma_R)  



# Determine the value-at-risk of continuously compounded monthly returns
# R ~ N(0.04, (0.09)^2) 
mu_r <- 0.04
sigma_r <- 0.09

# Initial wealth W0 equals $100,000
W0 <- 100000

# 1 * Value-at-risk
W0 * (exp(qnorm(0.01, mu_r, sigma_r)) - 1)

# The 5% value-at-risk
W0 * (exp(qnorm(0.05, mu_r, sigma_r)) - 1)

W0 * (exp(qnorm(0.05, mu_r, sigma_r)) - 1)



# Compute simple monthly returns

# consider a one month investment in two stocks, amazon and costco

# Vectors of prices
PA <- c(38.23, 41.29)
PC <- c(41.11, 41.74)

# Simple monthly returns
RA <- PA[2]/PA[1] - 1
RC <- PC[2]/PC[1] - 1


# Compute continuously compounded monthly returns
# Continuously compounded returns
rA <- log(PA[2]) - log(PA[1])
rC <- log(PC[2]) - log(PC[1])  

# Compute simple total returns and dividend yields
# Suppose Amazon paid a $0.10 per share cash dividend at the end of October. 
# Compute the monthly simple total return on Amazon and the monthly dividend yield.

# Cash dividend per share
DA <- 0.1

# Simple total return
RA_total <- (PA[2] + DA)/PA[1] - 1

# Dividend yield
DY <- DA/PA[1]


# Compute annual returns
# Suppose the monthly returns on Amazon are equal to RA every month for one year. 
# Compute the simple annual returns, as well as the continuously compounded annual returns.

# Simple annual return
RA_annual = (RA + 1) ^ 12 - 1

# Continuously compounded annual return
rA_annual = log(1 + RA_annual)


# At the end of September 2004, suppose you have $10,000 to invest in Amazon and Costco over the next month.
# Compute your portfolio shares if you invest $8,000 in Amazon and $2,000 in Costco. 
# Moreover, compute the simple monthly return on the portfolio and assume that no dividends were paid.
# Portfolio shares
xA <- 8000
xC <- 2000

# Simple monthly return
((xA*(1 + RA) - xA) + (xC*(1 + RC) - xC))/(xA + xC)


