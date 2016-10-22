library(lattice)

a1 = 2
a2 = 2
a3 = 2
sqt3 = sqrt(3)
grid = data.frame(c(),c())
xrows = 200
yrows = 200
yseq = seq(0.001,sqt3/2-0.001,length.out = yrows)


for (i in yseq) {
  xseq = seq(0.001,0.999,length.out=xrows)
  for (j in xseq) {
    if ((j > i/sqt3) & (j < 1 - i/sqt3)) {
      grid = rbind(grid,c(j,i))
    }
  }
}

colnames(grid) <- c("x","y")

grid$z <- ( grid$x-grid$y/sqt3 )^(a1-1) * ( grid$y * 2 / sqt3 )^(a2-1) * ( 1-grid$x-grid$y/sqt3 )^( a3-1 )
#grid$z <- grid$x+grid$y


levelplot(z~x*y, data=grid, cuts = 50, xlab="",
          ylab="", main="Dirichlet Distribution",
          region = TRUE)
