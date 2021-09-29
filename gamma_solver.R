gamma_solver <- function(.values, .probs, .guess = c(1, 1)) {
  qu <- .values
  p <- .probs
  gammaoptim <- function(param) { 
  q1 <- qu[1]
  q2 <- qu[2]
  p1 <- p[1]
  p2 <- p[2]
  (pgamma(q1 ,param[1], param[2]) - p1)^2 + 
    (pgamma(q2, param[1], param[2]) - p2)^2
}

r <- optim(.guess, gammaoptim)
if(r$convergence != 0) warning("Parameter solver did not converge", call. = FALSE)
params <- r[[1]]
names(params) <- letters[1:2]
params
}

# gamma_solver(c(3.0,4.0),c(.5,.75))
