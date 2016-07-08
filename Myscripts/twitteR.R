install.packages("syuzhet")
install.packages("syuzhet")


library(twitteR)
api_key <- "8QLRvrbAyQStthnUFt4IlJHDo"
api_secret <- "gDFdbZoo8zvL65DQ5ucvtciuKOoQBQgd1Lj73zN5lbzN6sGtw3"
token <- "139103301-8bAYdavsM6OUXQkWMS9N1QVn2KD83bnKsWE4d9h1"
token_secret <- "12Vwjw8tCm6RM7ZS7IKBYEE5MlHkJQtlUEonWvlG3Sdjl"

?getUser

setup_twitter_oauth(api_key, api_secret, token, token_secret)
degreed <- getUser("degreed")
