# Use solver to optimize

?optimize
?optim
install.packages("optimx")
install.packages("ROI")
install.packages("lpsolve")
library(lpSolve)
install.packages("nloptr")
# mainly planning to use lpsolve and nloptr

# LPSOLVE
# for linear, integer, transportation and assignment problems
?lp
?lp.assign
?lp.object
?lp.transport

# lp
# Maximize
# x1 + 9x2 + x3 subject to
# x1 + 2x2 + 3x3 <= 9
# 3x1 + 2x2 + 2x3 <= 15

f.obj <- c(1, 9, 1)
f.con <- matrix(c(1, 2, 3, 3, 2, 2), nrow = 2, byrow = T)
f.dir <- c("<=", "<=")
f.rhs <- c(9, 15)

lp(direction = "max", objective.in = f.obj, const.mat = f.con, const.dir = f.dir, const.rhs = f.rhs)
# Success: the objective function is 40.5

x <- lp("max", f.obj, f.con, f.dir, f.rhs)
str(x)
x$solution # gives values of x1, x2, x3

# using dense constraint approach

f.con.d <- matrix(c(rep(1:2, each = 3), rep(1:3, 2), t(f.con)), ncol = 3)
lp("max", f.obj, , f.dir, f.rhs, dense.const = f.con.d)


# get sensitivities

lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens = T)$sens.coef.from
lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens = T)$sens.coef.to

# right now, the dual values for the constraints and the variables are combined, constraints coming first
# so, in this example

lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens = T)$duals
# duals can be duals.from and duals.to

# run again, reuqiring that all 3 variables are int
lp("max", f.obj, f.con, f.dir, f.rhs, int.vec = 1:3)
lp("max", f.obj, f.con, f.dir, f.rhs, int.vec = 1:3)$solution

# sensitivities in the integer can are harder to interpret

# Here's an example in which we want more than one solution to a problem
# in which all variables are binary: the 8-queens problem,
# with dense constraints.

chess.obj <- rep(1:64)
q8 <- make.q8() # make.q8 is a general sparse matrix for 8 queens problem
chess.dir <- rep(c("=", "<"), c(16, 26))
c.rhs <- rep(1, 42)
lp("max", chess.obj, , chess.dir, c.rhs, dense.const = q8, all.bin = T, num.bin.solns = 3)
# Damn, I have no clue what the hell happened here


# lp.assign.... Interger Programming for the assignment problem
# cost.mat is the matrix of costs that we need for the assignment problem
# ijth element is the cost of assigning source i to destination j

assign.costs <- matrix(c(2, 7, 7, 2, 7, 7, 3, 2, 7, 2, 8, 10, 1, 9, 8, 2), 4, 4)
assign.costs

lp.assign(assign.costs)
lp.assign(assign.costs)$solution

# lp.transport
# cost.mat... ijth element is the cost of transporting from i to j
# formula needs direction, row.signs, row.rhs, col.signs and col.rhs

costs <- matrix (10000, 8, 5); costs[4,1] <- costs[-4,5] <- 0
costs[1,2] <- costs[2,3] <- costs[3,4] <- 7; costs[1,3] <- costs[2,4] <- 7.7
costs[5,1] <- costs[7,3] <- 8; costs[1,4] <- 8.4; costs[6,2] <- 9
costs[8,4] <- 10; costs[4,2:4] <- c(.7, 1.4, 2.1)
costs

row.signs <- rep("<", 8)
row.rhs <- c(200, 300, 350, 200, 100, 50, 100, 150)
col.signs <- rep (">", 5)
col.rhs <- c(250, 100, 400, 500, 200)

tr <- lp.transport(costs, "min", row.signs, row.rhs, col.signs, col.rhs)
tr$solution

# print.lp.... General Sparse matrix for the 8-queens problem
