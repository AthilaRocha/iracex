##rosenbrock function
f_rosenbrock <- function (x) {
  d <- length(x)
  z <- x + 1
  hz <- z[1:(d - 1)]
  tz <- z[2:d]
  s <- sum(100 * (hz^2 - tz)^2 + (hz - 1)^2)
  return(s)
}

##rastrigin function
f_rastrigin <- function(x){
  sum(x*x - 10 * cos(2*pi*x)+10)
}

##weights of f_(x)
weights <- rnorm(200,mean=0.9,sd=0.02)

## f_(x) to be optimized
f_x <- function(x){
  weights * f_rastrigin(x)+(1-weights)*f_rosenbrock(x)
}

##function for testing
fteste <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  ##100*(x2-x1*x1)^2+(1-x1)^2

  (x1-1)^2+(x2-1)^2

  ##weight <- rnorm(1,mean=0.9,sd=0.02)
  ##return(weight * f_rastrigin(x)+(1-weight)*f_rosenbrock(x))
}



##parameters of algorithm
parameters.table <- 'tmax "" i (1,5000)
temp "" r (0,100)
'

##Reading the parameters to irace method
parameters <- readParameters(text= parameters.table)


##race function
hook.run <- function(instance, candidate, extra.params = NULL, config = list())
{
  D <- 3
  par <-0.275957
  #par <- runif(D, min=-1, max=1)
  fn <- function(x) {
    weight <- instance
    return(weight * f_rastrigin(x) + (1 - weight) * f_rosenbrock(x))
  }
  res <- optim(par,fn, method="SANN",
               control=list(maxit=5000, tmax = as.numeric(candidate$values[["tmax"]]), temp = as.numeric(candidate$values[["temp"]])
               ))
  return(res$value)
}
