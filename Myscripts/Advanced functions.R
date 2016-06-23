# one of the best ways to improve the efficiency of a function is to avoid using loops as they
# are very slow and inefficient. This can be done by vectorizing the functions

# APPLY FUNCTION.. applies to sections of an array and returns the same
# a 0 dimensional array is a scalar or point
# a 1 dimensional array is a vector
# a 2 dimensional array is a matrix

# apply(array, margin, function, ...)

# margin specifies which margin we wish to apply the function to and which margin we wish to keep.
# For a matrix, 1 would apply the function to rows and 2 would apply it to columns.
# the function can be built in or user defined. THe ... refers to any other argument that is passed to the 
# function being used
# The apply function internally uses a loop so others are better options

mat1 <- matrix(rep(seq(4), 4), ncol = 4)
mat1

# row sums of mat1
apply(mat1, 1, sum) # gives the sums of all elements in the 4 rows

# column sums of mat1
apply(mat1, 2, sum)

# using a user defined function

sum.plus.2 <- function(x){
  sum(x) + 2
}

# using sum.plus.2 on the rows of mat1
apply(mat1, 1, sum.plus.2)

# the function can be defined inside the apply function
# it will not use curly brackets

apply(mat1, 1, function(x) sum(x) + 2)

# generalization of a function to add any number to the sum
# add 3 to row sums

apply(mat1, 1, function(x, y) sum(x) + y, y = 3)

# LAPPLY.. applies to a list or a vector and returns the results in a list

# lapply(list, function, ...)

# lapply is very useful while working with data frames as all data frames are considered lists
# there is no margin argument since we apply the function to each component in the list

mat1.df <- data.frame(mat1)

mat1.df
is.list(mat1)
is.list(mat1.df)

# obtaining the sum of each variable in mat1.df
lapply(mat1.df, sum)

# we can store the results of lapply as a list

y <- lapply(mat1.df, sum)
is.list(y)
names(y)

# we can also define the function inside lapply

y1 <- lapply(mat1.df, function(x, y) sum(x) + y, y = 5)
y1[1:2]

# another useful application of lapply is the dummy sequence. the list argument is the dummy sequence 
# and is only used to specify how many iterations we would like to have the function executed
# lapply, used in this way can replace a for loop

# using lapply instead of for
unlist(lapply(1:5, function(i) print(i)))

# using for loop
for(i in 1:5){
  print(i)
}

# SAPPLY.. applies function to a list and returns the results in a vector, matrix or list
# sapply(list, function, ..., simplify)
# when simplify = F, the function results a list, if T (the default), it returns the simplest for possible

y2 <- sapply(mat1.df, function(x, y) sum(x) + y, y = 5)
is.vector(y2) # y2 is a vector

# TAPPLY.. applies the function to each cell in of a ragged array
# tapply(array, indices, function, ..., simplify)
# the function is applied to each of the cells which are defined by the categorical variables listed in the
# argument indices. 

# creating a data set with two categorical variables
x1 <- runif(16) 
x1

cat1 <- rep(1:4, 4)
cat1

cat2 <- c(rep(1, 8), rep(2, 8))
cat2
mat2.df <- data.frame(x1)
mat2.df$cat1 <- cat1
mat2.df$cat2 <- cat2
mat2.df

tapply(mat2.df$x1, mat2.df$cat1, mean)
tapply(mat2.df$x1, list(mat2.df$cat1, mat2.df$cat2), mean)

# SWEEP.. returns an array like input array and stats with sweep out
# sweep(array, margin, stats, function, ...)

# the input array can be any dimensional array, stats argument is a vector containing the summary statistics
# of the array that has to be swept out. margin specifies which dimension of the array corresponds to 
# summary statistics in stats. if matrix, 1 corresponds to rows and 2 to columns
# function is most often "/" or "-"

# creating data set
a <- matrix(runif(100, 1, 2), 20)
a.df <- data.frame(a)

# subtract column means from each column
# centering each column around mean
colMeans(a)
a1 <- sweep(a, 2, colMeans(a), "-")
a1[1:5, ]
colMeans(a1)

# deviding each column by sum
a2 <- sweep(a, 2, colSums(a), "/")
a2[1:5, ]

# centering each row around the mean of the row
rowMeans(a)[1:5]
a3 <- sweep(a, 1, rowMeans(a), "-")
a3[1:5, ]
rowMeans(a3)[1:5]

# THE COLUMN FUNCTIONS
# there are a suit of functions whose sole porpose is  to compute summary statistics over columns of vectors,
# matrices, arrays, and data frames. These include colSums and colMeans

a <- matrix(runif(100, 1, 2), 20)
a.df <- data.frame(a)
a.df[1:5, ]

# get column means using columns function; input is matrix a and results a vector
col.means1 <- colMeans(a)
col.means1
is.vector(col.means1)

# get column means using apply
col.means2 <- apply(a, 2, mean) # also a vector
col.means2

# get column means using lapply
col.means3 <- lapply(a.df, mean) # returns a list
col.means3

# all this can also be done for row means

# ALL FUNCTIONS SHOWN CAN ALSO BE USED INSIDE USER DEFINED FUNCTIONS
# in the following example, f1 multiplies the sequence 1 - x by y by using lapply instead of a for loop

f1 <- function(x, y){
  return(lapply(1:x, function(a, b) b*a, b = y))
}

# multiplying the sequence 1:3 by 2
f1(3, 2)

# multiplying 1:4 by 10
f1(4, 10)

# Cool use of lapply which can be used in many clever ways

list1 <- lapply(1:6, runif)
list1
list2 <- lapply(1:6, runif)
list2
lapply(1:6, function(i, x, y) x[[i]] + y[[i]],
       x = list1, y = list2)
