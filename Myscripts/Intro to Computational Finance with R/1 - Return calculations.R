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

  
  
  











