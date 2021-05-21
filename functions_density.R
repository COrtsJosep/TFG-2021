### Density Estimation ###
dens_point <- function(x0, x, width){
  # Returns the estimated density at 'x0' with respect to a sample 'x'.
  constant <- 1/sqrt(2*pi)
  weights <- constant*exp(-0.5*(((x0-x)/width)^2))
  return((1/(length(x)*width))*sum(weights))
}
dens_point <- Vectorize(dens_point, vectorize.args = "x0")

dens_line <- function(precision, data, width, use.default.grid = TRUE, min = NA, max = NA){
  # Returns a vector of densities for a grid of values.
  if (use.default.grid){
    grid <- seq(min(data), max(data), length = precision)
  } else{
    grid <- seq(min, max, length = precision)
  }
  densities <- dens_point(grid, data, width)
  return(data.frame(grid = grid, densities = densities))
}



### Density Bandwidth Selection, LS CV ###
h_rot <- function(data){
  # Returns the rule-of-thumb bandwidth for density estimation.
  return(1.06*sd(data)/length(data)^(1/5))
}

convolution_point <- function(x0, x, width){
  # Returns the estimated value of the convolution of the gaussian kernel at 'x0' with respect to a sample 'x'.
  constant <- 1/sqrt(4*pi)
  weights <- constant*exp(-0.25*(((x0-x)/width)^2))
  return(sum(weights))
}
convolution_point <- Vectorize(convolution_point, vectorize.args = "point")

dens_point_out_ls <- function(i, x, width){
  # Returns the LOO density for the 'i'-th observation of 'x'.
  constant <- 1/sqrt(2*pi)
  weights <- constant*exp(-0.5*(((x[i]-x[-i])/width)^2))
  return(sum(weights))
}
dens_point_out_ls <- Vectorize(dens_point_out_ls, vectorize.args = "i")

ls_funct <- function(width, x){
  # Returns the LS-CV error for a bandwidth 'width' and a vector 'x'.
  n <- length(x)
  first_term <- sum(convolution_point(x, x, width))/(width*n^2)
  second_term <- 2*sum(dens_point_out_ls(1:n, x, width))/(width*n*(n-1))
  return(first_term - second_term)
}

CV_LS <- function(x, precision, range = NA, use_default_range = TRUE, randomise = TRUE){
  # Optimally chooses the bandwidth for a sample 'x' using the CV-LS method. Returns the grid of proposed
  # bandwidths, the associated error and the optimal bandwidth, as a list.
  if (use_default_range){
    h_ref <- h_rot(x)
    range <- seq(h_ref*0.15, h_ref*2, length.out = precision)
  }
  if (randomise){
    x <- x + rnorm(length(x), sd = sd(x)/50)
  }
  errors <- unlist(lapply(range, ls_funct, x))
  index <- which(errors == min(errors))
  if(index == 1 | index == precision){
    print("Attention! The result might be suboptimal!")
  }
  return(list(widths = range, errors = errors, hst = range[index]))
}



### Density Bandwidth Selection, ML CV ###
dens_point_out_ml <- function(i, x, width){
  # Returns the LOO density for the 'i'-th observation of 'x', but taking logarithms..
  constant <- 1/sqrt(2*pi)
  weights <- constant*exp(-(1/2)*(((x[i]-x[-i])/width)^2))
  return(log(sum(weights)/(width*(length(x-1)))))
}
dens_point_out_ml <- Vectorize(dens_point_out_ml, vectorize.args = "i")

ml_funct <- function(width, x){
  # Returns the value of the ML function for a bandwidth 'width' and a sample 'x'.
  return(sum(dens_point_out_ml(1:length(x), x, width)))
}

CV_ML <- function(x, precision, range = NA, use.default.range = TRUE){
  # Optimally chooses the bandwidth for a sample 'x' using the CV-ML method. Returns the grid of proposed
  # bandwidths, the associated value of the likelihood function and the optimal bandwidth, as a list.
  if (use.default.range){
    h_ref <- h_rot(x)
    range <- seq(h_ref*0.1, h_ref*2, length.out = precision)
  }
  values <- unlist(lapply(range, ml_funct, x))
  index <- which(values == max(values))
  if(index == 1 | index == precision){
    print("Attention! The result might be suboptimal!")
  }
  return(list(widths = range, values = values, hst = range[index]))
}



### Density Bandwidth Selection, Plug-In Method ###
dens_point_4dev <- function(i, x, width){
  # Auxiliary function. Similar to the estimation of the density at 'x[i]' with respect to a sample
  # 'x', but using the 4th derivative of the gaussian kernel.
  u <- (x[i] - x)/width
  weights <- (3-6*u^2+u^4)*exp(-u^2/2)/sqrt(2*pi)
  return(sum(weights))
}
dens_point_4dev <- Vectorize(dens_point_4dev, vectorize.args = "i")

T_hat <- function(x, sd){
  # Auxiliary function. Returns an approximation for int(f'''(x)^2)dx.
  library(pracma)
  return(
    (1/(pi*sd^14)) * 
      ((-4*sd^2*x^5+14*sd^4*x^3-15*sd^6*x)*(1/8)*exp(-2*(sd^2*log(2)+x^2)/(2*sd^2)) - 
         15/32*sd^7*sqrt(pi)*erf(-x/sd))
  )
}

h_pi <- function(x){
  # Returns the optimal bandwidth for a sample 'x' using the plug-in method.
  n <- length(x)
  ck <- 0.2820947917739
  dk <- 0.5
  k_iv0 <- 1.1968268412043
  sd <- sd(x)
  t <- T_hat(9999999999, sd) - T_hat(-9999999999, sd)
  g <- (2*k_iv0/(dk*t*n))^(1/7)
  S <- sum(dens_point_4dev(1:n, x, g))/(n^2*g^5)
  return(
    ck^(1/5)*dk^(-2/5)*S^(-1/5)*n^(-1/5)
  )  
}
