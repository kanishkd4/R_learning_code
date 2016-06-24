rm(list = ls())

# Assign the URL to the CSV file
data_url <- "http://assets.datacamp.com/course/compfin/sbuxPrices.csv"

# Load the data frame using read.csv
sbux_df <- read.csv(data_url, header = T, stringsAsFactors = F)

# The sbux_df data frame is already loaded in your work space

# Check the structure of 'sbux_df'
str(sbux_df)

# Check the first and last part of 'sbux_df'
head(sbux_df)
tail(sbux_df)

# Get the class of the Date column of 'sbux_df'
class(sbux_df$Date)

# The sbux_df data frame is already loaded in your work space
closing_prices <- sbux_df[, "Adj.Close", drop = F]

# It will often be useful to select stock data between certain dates. Advanced users are advised to look at the xts package. However, base R also provides sufficient functionality to do this.

# The which() function returns the indices for which a condition is TRUE. For example: which(sbux_df$Date == "3/1/1994") returns the position of the date 3/1/1994, which indicates in this case the row number in the sbux_df data frame.
# Find indices associated with the dates 3/1/1994 and 3/1/1995
index_1 <- which(sbux_df$Date == "3/1/1994")
index_2 <- which(sbux_df$Date == "3/1/1995")
  
# Extract prices between 3/1/1994 and 3/1/1995
some_prices <- sbux_df[index_1:index_2, "Adj.Close"]

# The way you selected the data from a specific trading day in the previous exercise was not very convenient, right?
# 
# When you create a data frame that has the dates of the stock price as row names, you can select the price on a specific day much more easily. The sample code on the right creates a new data frame sbux_prices_df that has the trading days as row names. You can select the price on 3/1/1994 now simply with sbux_prices_df["3/1/1994", 1].
   
# Create a new data frame that contains the price data with the dates as the row names
sbux_prices_df <- sbux_df[, "Adj.Close", drop=FALSE]
rownames(sbux_prices_df) <- sbux_df$Date
head(sbux_prices_df)

# With Dates as rownames, you can subset directly on the dates.
# Find indices associated with the dates 3/1/1994 and 3/1/1995.
price_1 <- sbux_prices_df["3/1/1994", 1]
price_2 <- sbux_prices_df["3/1/1995", 1] 
  
# Now add all relevant arguments to the plot function below to get a nicer plot
# Let us make a better plot by adding the following arguments to the plot function: type="l" specifies a line plot, col="blue" indicates that the line should be blue, lwd=2 doubles the line thickness, ylab="Adjusted close" adds a y-axis label and main="Monthly closing price of SBUX" adds a title.

plot(sbux_df$Adj.Close, type = "l", col = "blue", lwd = 2, ylab="Adjusted close", main="Monthly closing price of SBUX")
legend(x='topleft',legend='SBUX', lty=1, lwd=2, col='blue')


# Calcualting returns
sbux_prices_df <- sbux_df[, "Adj.Close", drop=FALSE]

# Denote n the number of time periods
n <- nrow(sbux_prices_df)
sbux_ret <- (sbux_prices_df[2:n,1] - sbux_prices_df[1:(n-1),1])/sbux_prices_df[1:(n-1),1]
  
# Notice that sbux_ret is not a data frame object
class(sbux_ret)

# Now add dates as names to the vector and print the first elements of sbux_ret to the console to check
names(sbux_ret) <- sbux_df$Date[2:181]
head(sbux_ret)


# Compute continuously compounded 1-month returns
# As you might remember from class, the relation between single-period and multi-period returns is multiplicative for single returns. That is not very convenient. The yearly return is for example the geometric average of the monthly returns.
# Therefore, in practice you will often use continuously compounded returns. These returns have an additive relationship between single and multi-period returns and are defined as
# rt = ln(1 + Rt) where Rt is simple return and rt is the continuously compouded return at moment

# The sbux_df data frame is already loaded in your work space
sbux_prices_df <- sbux_df[, "Adj.Close", drop=FALSE]

# Denote n the number of time periods:
n <- nrow(sbux_prices_df)
sbux_ret <- ((sbux_prices_df[2:n, 1] - sbux_prices_df[1:(n-1), 1])/sbux_prices_df[1:(n-1), 1])

# Compute continuously compounded 1-month returns
sbux_ccret <- log(sbux_prices_df[2:n, 1]/sbux_prices_df[1:n-1, 1])
  
# Assign names to the continuously compounded 1-month returns
names(sbux_ccret) <- sbux_df$Date[2:181]

# Show sbux_ccret
head(sbux_ccret)



# Compare simple and continuously compounded returns
# The sbux_df data frame is already loaded in your work space
sbux_prices_df <- sbux_df[, "Adj.Close", drop=FALSE]

# Denote n the number of time periods:
n <- nrow(sbux_prices_df)
sbux_ret <- ((sbux_prices_df[2:n, 1] - sbux_prices_df[1:(n-1), 1])/sbux_prices_df[1:(n-1), 1])

# Compute continuously compounded 1-month returns
sbux_ccret <- log(sbux_prices_df[2:n,1]) - log(sbux_prices_df[1:(n-1),1])
names(sbux_ccret) <- sbux_df[2:n,1]
head(sbux_ccret)

# Compare the simple and cc returns
head(cbind(sbux_ccret, sbux_ret))

# Graphically compare the simple and continuously compounded returns
# The simple returns (sbux_ret) and the continuously compounded returns (sbux_ccret) have been preloaded in your workspace

# Plot the returns on the same graph
plot(sbux_ret, type="l", col="blue", lwd=2, ylab="Return",
     main="Monthly Returns on SBUX")

# Add horizontal line at zero
abline(h=0)

# Add a legend
legend(x="bottomright", legend=c("Simple", "CC"), 
       lty=1, lwd=2, col=c("blue","red"))

# Add the continuously compounded returns
lines(sbux_ccret, col = "red", lwd = 2)


# Calculate growth of $1 invested in SBUX
# The simple returns (sbux_ret) and the continuously compounded returns (sbux_ccret) have been preloaded in your workspace

# Compute gross returns
sbux_gret <- 1
# Compute future values
sbux_fv <- cumprod(sbux_ret + sbux_gret)

# Plot the evolution of the $1 invested in SBUX as a function of time
plot(sbux_fv, type="l", col="blue", lwd=2, ylab="Dollars", 
     main="FV of $1 invested in SBUX")


