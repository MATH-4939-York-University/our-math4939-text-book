#'
#' # Selection of problems from Warwick R Exercises
#'  
#'
#' ## Exercises 1.8
#' 
#' By using the function cumprod or otherwise, calculate:
#' $$ 1 + \frac{2}{3} + \left( \frac{2}{3} \frac{4}{5} \right) + \cdots +  \left( \frac{2}{3} \frac{4}{5} \cdots  \frac{38}{39}\right)$$
#' 
#' Solution:
#' 
cp <- function(n) {
  if( (n %% 2) != 0) stop('n must be an even integer')
  terms <- sapply(seq(2,n,2), function( i) prod(seq(2,i,2))/prod(seq(3,i+1,2)) ) 
  1 + sum(terms)
}
cp(38)
cp(100)
cp(500) # why? how could we improve this
#' 
#' ## Exercise 2.5 a
#' 
mat1 <- function(n) {
  ret <- matrix(0, nrow = n + 1, ncol = n + 1)
  ret <- (row(ret) + col(ret) - 2) %% (n+1)
  ret
}
mat1(3)


mat2 <- function(n) {
  circ <- function(x) c(x[-1],x[1])
  ret <- numeric(0)
  vec <- 0:n
  for ( i in 0:n) {
    ret <- cbind(ret,vec)
    vec <- circ(vec)
  }
  ret
}
mat2(3)
#'
#' ## Exercise 2.6
#'
sol <- function(x) {
  n <- length(x)
  mat <- matrix(0,n,n)
  mat <- abs(row(mat) - col(mat)) + 1
  solve(mat,x)
}
sol(c(7,-1,-3,5,17))

#'
#' Exercise 2.8 c
#'
dsum <- function(n) {
  isum <- function(i) {
    v <- 1:i
    sum(i^4/(3 + i*v))
  }
  sum(sapply(1:n, isum))
}
dsum(10)

