# creating a fucntion that adds 2 numbers

f1 <- function(x, y){
  x + y
}

f1(3, 4)

# if a function performs multiple tasks and has multiple results to report, then we have to 
# include a return statement(with c()) inside the function to see the result

f.bad <- function(x, y){
  z1 = 2*x + y
  z2 = x + 2*y
  z3 = 2*x + 2*y
  z4 = x/y
}

f.bad(1, 2)# does not return a result

f.good <- function(x, y){
  z1 = 2*x + y
  z2 = x + 2*y
  z3 = 2*x + 2*y
  z4 = x/y
  return(c(z1, z2, z3, z4))
}

f.good(1, 2)# returns a result

# functions can do multiple tasks. It's often useful to save the result as a list as we can
# access each result separately by using the list indices

f2 <- function(x, y){
  z1 = x + y
  z2 = x + 2*y
  list(z1, z2)
}

f2(1, 2)
f2(2, 5)[[1]] # we can call the output using the list indices but not by using $
              # we cannot refer to them outside the function

# we can access the result outside the function if we give the element a name

f3 <- function(x, y){
  z1 = x + y
  z2 = x + 2*y
  list(r1 = z1, r2 = z2)
}

f3(1, 2)
f3(2, 2)$r1

# we can often store the result of a function in an object

y <- f3(1, 4)
y$r2
y[[2]]

# TYPES OF ARGUMENTS
# we haven't yet put a restriction on the type of argument we can use. x and y can be single
# numbers as well as matrices and vectors. The precation has to be taken that x and y must 
# have the same dimensions or else the computation will not be performed

# using vectors

v1 <- seq(1:6)
V1
v2 <- seq(2, 12, 2)
V2
f3(v1, v2)

# using matrices

mat1 <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
mat1
mat2 <- matrix(c(2, 4, 6, 8, 10, 12), ncol = 2)
mat2

f3(mat1, mat2)

# DEFAULT ARGUMENTS
# it's quite easy and often useful to specify default arguments in a function
# f4 is the same as f3 except default values of x and y have been assigned
# by leaving arguments blank in the call, we use the defaults
# calls don't need to be in order just predefiend R functions.
# we can reorder by using x =  and y = in the function call

f4 <- function(x = 3, y = 2) {
  z1 = x + y
  z2 = x + 2*y
  list(r1 = z1, r2 = z2)
}

f4()
f4(1,)
f4(, 1)
f4(x = 1)
f4(y = 1)
f4(y = 3, x = 2)


#USING FOR LOOPS
# used when iterating through a list

# structure: for(index in list){commands}

for (i in 2:4){
  print(i)
}

# unlike a function, for does not have a return statement. when we just save the computation
# in an object, we will just be able to see the value after the loop has finished
# use print to see the value of each iteration

for (i in c(1, 3, 6, 9)){
  z = i + i
}
z

# using print to see the result of each iteration

for(i in 3:5){
  z = i+1
  print(z)
}

# the list does not have to containnumbers

cars <- c("Toyota", "Ford", "Chevy")
for(j in cars){
  print(j)
}

# including a for loop in a funcion we include a
# return statement at the end of the function as a return statement will exit the function

f5 <- function(x){
  for(i in 1:x){
    y = i*2
    print(y)
  }
  return(y*2)
}

f5(3)

# it can be useful to have a break in the loop. this is often combined with an if function  
# such that the loop will occur if the condition specified in the if function is satisfied

names1 <- c("Dave", "John", "Ann", "Roger", "Bill", "Kathy")
f.names <- function(x){
  for(name in x){
    if(name == "Roger")
      break
    print(name)
  }
}

f.names(names1)

# using while loop

# structure: while(condition){commands}

i <- 2
while(i <= 4){
  i = i + 1
  print(i)
}

# just like for, there is no return statement and we have to print to see every result
# once we have exited the loop, we can use the return statement

f6 <- function(x){
  i = 0
  while(i < x){
    i = i + 1
    y = i +2
    print(y)
  }
  return(y*2)
}

f6(3)
# not sure if we really need the return statement. Works without that

# it is rare to combine while with break
# the following will show how break is unnecesary

names1 <- c("Dave", "John", "Ann", "Roger", "Bill", "Kathy")
f.names.while <- function(x){
  i = 1
  while(x[i] != "Roger"){
    print(x[i])
    i = i + 1
  }
}

f.names.while(names1) # same result as done earlier


# USING REPEAT LOOPS

# Basic Structure

# repeat{
#   commands   
#   if(condition)
#       break 
# }

i <- 2
repeat{
  print(i)
  i <- i + 1
  if(i > 4)
    break
}


names1 <- c("Dave", "John", "Ann", "Roger", "Bill", "Kathy")

f.names.repeat <- function(x)  {
  i <- 1
  repeat {
    print(x[i])
    i <- i+1
    if(x[i] == "Roger")
      break
  }
}

f.names.repeat(names1)
# the same result obtained with for and while

# A major use of repeat is when we are not concerned with how manu iterations run but run an
# iteration till a specific condition is met

# in the following, we have a function that repeatedly draws samples with n = 100 from
# a standard normal deviation. We want to keep sampling till we have a sample with 
# a mean that is within epsilon of zero

random.sample1 <- function(epsilon){
  i <- 0
  repeat{
    i = i + 1
    mean.test <- abs(mean(rnorm(100)))
    if(mean.test < epsilon)
      break
  }
  list(mean = mean.test, number.iterations = i)
}

random.sample1(0.0001)


# IF ELSE statement
# ifelse(test, action.if.true, action.if.false)

x <- seq(1:5)
ifelse(x < 3, "T", "F")

# in anther example, we take the log of a sample with n = 10 drawn from a standard normal 
# distribution

norm2 <- rnorm(10, mean = 2)
norm2
log.normal <- ifelse(norm2 < 0, 0, log(norm2))
log.normal


# PASSING AN UNSPECIFIED NUMBER OF PARAMETERS TO A FUNCTION

# we can pass an unspecified number of parameters to a function by using the ... notation in
# the argument list. Need to be careful about the order of arguments when using ...
# consider f1 and f2 below

f1 <- function(x, ...){
  y = x + 2
  return(y)
  #other commands
}

f2 <- function(..., x){
  y = x + 2
  return(y)
  # other commands
}

# in f1, we can pass the value of x by specifying f1(3) or f1(x = 3), but in f2, we cannot 
# specify f2(3) as it would take 3 to be a part of ...

f1(3)
f1(x = 3)
f2(3)

# MODIFYING AN ALREADY EXISTING FUNCTION

x <- rnorm(100)
y <- x + rnorm(100)

plot(x, y)

my.plot <- function(..., pch.new = 15){
  plot(..., pch = pch.new)
}

my.plot(x, y)
