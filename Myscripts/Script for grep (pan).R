dog <- "The quick brown dog"
str_to_upper(dog)
str_to_lower(dog)
str_to_title(dog)

# Locale matters!
str_to_upper("i", "en") # English
str_to_upper("i", "tr") # Turkish


# invert_match
numbers <- "1 and 2 and 4 and 456"
num_loc <- str_locate_all(numbers, "[0-9]+")[[1]]
str_sub(numbers, num_loc[, "start"], num_loc[, "end"])
text_loc <- invert_match(num_loc)
str_sub(numbers, text_loc[, "start"], text_loc[, "end"])
?str_sub


# modifiers Control matching behaviour with modifier functions
pattern <- "a.b"
strings <- c("abb", "a.b")
str_detect(strings, pattern)
str_detect(strings, fixed(pattern))
str_detect(strings, coll(pattern))
# coll() is useful for locale-aware case-insensitive matching
i <- c("I", "\u0130", "i")
i
str_detect(i, fixed("i", TRUE))
str_detect(i, coll("i", TRUE))
str_detect(i, coll("i", TRUE, locale = "tr"))

# Word boundaries
words <- c("These are some words.")
str_count(words, boundary("word"))
str_split(words, " ")[[1]]
str_split(words, boundary("word"))[[1]]
?str_count

# Regular expression variations
str_extract_all("The Cat in the Hat", "[a-z]+")
str_extract_all("The Cat in the Hat", regex("[a-z]+", TRUE))
str_extract_all("a\nb\nc", "^.")
str_extract_all("a\nb\nc", regex("^.", multiline = TRUE))
str_extract_all("a\nb\nc", "a.")
str_extract_all("a\nb\nc", regex("a.", dotall = TRUE))

install.packages("grep")
?grep

pan <- read.csv(file = "Book1.csv", header = T)

txt <- c("arm","foot","lefroo", "bafoobar")
if(length(i <- grep("foo", txt)))
  cat("'foo' appears at least once in\n\t", txt, "\n")
i # 2 and 4
txt[i]

pan$pan = as.character(pan$pan_nbr.distinct.in.source.customer.details)

x <- c(aasd1234r, 123243454, aaaaa1111r, dsjksxcxcvl1234f)
str(pan)

grep("[a-z]/[a-z]/[a-z]/[a-z]/[a-z]", x )
grep("AAA", x = x, ignore.case = TRUE)
grep(x = x, pattern = "AAA")

grep(pattern = "[a-z][a-z][a-z][a-z][a-z][0-9][0-9][0-9][0-9][a-z]", x = pan$pan, ignore.case = TRUE)
grep(pattern = "[a-z]{5}[0-9]{4}[a-z]", x = pan$pan, ignore.case = TRUE)
pan$true <- gsub(pattern = "[a-z]{5}[0-9]{4}[a-z]", x = pan$pan, ignore.case = TRUE, replacement = "TRUE")


str(y)

y <- "(1)AADPN9905C(2)FORM60"


grep(pattern = "[a-z]{5}[0-9][0-9][0-9][0-9][a-z]", x = x, ignore.case = TRUE)

gsub(pattern = "[^0-9]", x = x, replacement = "")
x

# For pan data
pan <- read.csv(file = "Book1.csv", header = T)
pan$pan = as.character(pan$pan_nbr.distinct.in.source.customer.details)
pan$true <- gsub(pattern = "[^a-z][^0-9]", x = pan$pan, ignore.case = TRUE, replacement = "") #WIP
pan$true1 <- gsub(pattern = "\\.|/|\\-|\"|\\s|\\(|\\)|\\,", x = pan$pan,
                  ignore.case = TRUE, replacement = "") #works
# http://stackoverflow.com/questions/18660424/how-to-remove-special-characters-spaces-and-trim-in-one-string-a-character-vari



