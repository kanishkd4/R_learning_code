library(lpsolve)

f.obj <- c(6, 5.3, 5.4, 4.2, 3.8, 1.8)
f.con <- matrix(c(6, 5, 4, 3, 2.5, 1.5, 3.2, 2.6, 1.5, 0.8, 0.7, 0.3), nrow = 2, byrow = T)
f.dir <- c("<=", "<=", "<=", "<=", "<=", "<=", "<=", "<=")
f.rhs <- c(4500, 1600, 960, 928, 1041, 977, 1084, 1055)

lp("max", f.obj, f.con, f.dir, f.rhs)$solution # incorrect

# this is not working as constraint does not include demand constraint for every product
# using density constraint

f.con.d <- matrix()
?t
?lp
f.con1 <- matrix(c(rep(1:2, each = 6), rep(1:6, 2), 
                   c(6, 5, 4, 3, 2.5, 1.5, 3.2, 2.6, 1.5, 0.8, 0.7, 0.3)), ncol = 3)

f.con2 <- matrix(c(rep(3:8), rep(1:6), rep(1, 6)), ncol = 3)

f.cond <- rbind(f.con1, f.con2)


x <- lp(direction = "max", objective.in = f.obj, const.dir = f.dir, const.rhs = f.rhs, dense.const = f.cond)
x
x$solution
str(x)

