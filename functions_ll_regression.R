### Regression in R^2 ###
reg_point_ll <- function(x0, x, y, width){
  # Returns the kernel regression estimate at 'x0' with respect to samples 'x' and 'y'.
  constant <- 1/sqrt(2*pi)
  W <- diag(constant * exp( -0.5*( (x0-x)/width )^2 ))
  B <- matrix( c(rep(1, length(x)), x), length(x), 2)
  M <- crossprod(B, W)
  MB <- M %*% B
  if(det(MB)<1.5*10^(-16)){
    return(NA)
  } else {return(t(c(1, x0)) %*% solve(MB) %*% M %*% y)}
}
reg_point_ll <- Vectorize(reg_point_ll, vectorize.args = "x0")

reg_line_ll <- function(precision, x, y, width, use.default.grid = TRUE, min = NA, max = NA){
  # Returns the kernel regression estimates for a grid of values.
  if (use.default.grid){
    grid <- seq(min(x), max(x), length = precision)
  } else{
    grid <- seq(min, max, length = precision)
  }
  output <- reg_point_ll(grid, x, y, width)
  return(data.frame(x = grid, y = output))
}



### Regression Bandwidth Selection in R^2, LOOCV ###
reg_point_out_ll <- function(i, x, y, width){
  # LOO regression estimate for the 'i'-th observation.
  constant <- 1/sqrt(2*pi)
  W <-  diag(constant * exp( -0.5*( (x[i]-x[-i])/width )^2 ))
  B <- matrix( c(rep(1, length(x[-i])), x[-i]), length(x[-i]), 2)
  M <- crossprod(B, W)
  MB <- M %*% B
  if(det(MB) < 1.5*10^(-16)){
    return(NA)
  } else {return(crossprod(c(1, x[i]), solve(MB)) %*% M %*% y[-i])}
}

reg_point_out_ll <- Vectorize(reg_point_out_ll, vectorize.args = "i")

loo_funct_ll <- function(width, x, y){
  # LOOCV error for a given bandwidth 'width' and two samples 'x' and 'y'. The computational
  # cost is gigantic, so it is just calculated for '80' random values.
  set.seed(seed_number)
  indices <- sample(1:length(x), 200)
  return(sum((y[indices] - reg_point_out_ll(indices, x, y, width))^2)/200)
}
loo_funct_ll <- Vectorize(loo_funct_ll, vectorize.args = "width")

LOOCV_ll <- function(x, y, precision, use.default.range = TRUE, min = NA, max = NA, randomise = TRUE){
  # Chooses the optimal bandwidth for regression using the LOOCV method. Returns a vector of
  # proposed bandwidths, their associated error and the optimal bandwidth, as a list.
  if (use.default.range){
    h_ref <- sd(x)/length(x)^(1/5)
    range <- seq(0.1*h_ref, 3.5*h_ref, length.out = precision)
  } else{
    range <- seq(min, max, length.out = precision)
  }
  if (randomise){
    y <- y + rnorm(length(y), sd = sd(y)/15)
  } 
  errors <- loo_funct_ll(range, x, y)
  nas <- which(is.na(errors))
  errors[nas] <- max(na.omit(errors))
  index <- which(errors == min(errors))
  if(index == 1 | index == precision){
    print("Attention! The result might be suboptimal!")
  }
  return(list(widths = range, errors = errors, hst = range[index]))
}



### Regression in R^3 ###
reg_point3d_ll <- function(x0, y0, x, y, z, width){
  # Returns the kernel regression estimate at 'x0' with respect to samples 'x', 'y' and 'z'.
  constant <- 1/sqrt(2*pi)
  W <- diag(constant * exp( -0.5*( sqrt((x0-x)^2+(y0-y)^2)/width )^2 ))
  B <- matrix( c(rep(1, length(x)), x, y), length(x), 3)
  M <- crossprod(B, W)
  MB <- M %*% B
  if(det(MB)<1.5*10^(-16)){
    return(NA)
  } else {return(crossprod(c(1, x0, y0), solve(MB)) %*% M %*% z)}
}
reg_point3d_ll <- Vectorize(reg_point3d_ll, vectorize.args = c("x0", "y0"))

reg_space_ll <- function(precision, x, y, z, width, use.default.grid = TRUE, minx = NA, maxx = NA, miny = NA, maxy = NA){
  # Returns the kernel regression estimates for a grid of values. The grid is a vector, not a matrix.
  if (use.default.grid){
    gridx <- seq(min(x), max(x), length = precision)
    gridy <- seq(min(y), max(y), length = precision)
  } else{
    gridx <- seq(minx, maxx, length = precision)
    gridy <- seq(miny, maxy, length = precision)
  }
  gridxy <- expand.grid(gridx, gridy)
  output <- reg_point3d_ll(gridxy[,1], gridxy[,2], x, y, z, width)
  return(data.frame(x = gridxy[,1], y = gridxy[,2], z = output))
}



### Regression Bandwidth Selection in R^3, LOOCV ###
reg_point_out3d_ll <- function(i, x, y, z, width){
  # LOO regression estimate for the 'i'-th observation.
  constant <- 1/sqrt(2*pi)
  W <- diag(constant * exp( -0.5*( sqrt((x[i]-x[-i])^2+(y[i]-y[-i])^2)/width )^2 ))
  B <- matrix( c(rep(1, length(x[-i])), x[-i], y[-i]), length(x[-i]), 3)
  M <- crossprod(B, W)
  MB <- M %*% B
  if(det(MB)<1.5*10^(-16)){
    return(NA)
  } else {return(t(c(1, x[i], y[i])) %*% solve(MB) %*% M %*% z[-i])}
}
reg_point_out3d_ll <- Vectorize(reg_point_out3d_ll, vectorize.args = "i")

loo_funct3d_ll <- function(width, x, y, z){
  # LOOCV error for a given bandwidth 'width' and samples 'x', 'y' and 'z'. Due to the gigantic
  # computational cost, the errors are only calculated for '400' random observations.
  set.seed(seed)
  indices <- sample(1:length(x), 400)
  return(sum((z[indices] - reg_point_out3d_ll(indices, x, y, z, width))^2)/400)
}
loo_funct3d_ll <- Vectorize(loo_funct3d_ll, vectorize.args = "width")

LOOCV3d <- function(x, y, z, precision, use.default.range = TRUE, min = NA, max = NA, randomise = TRUE){
  # Chooses the optimal bandwidth for regression using the LOOCV method. Returns a vector of
  # proposed bandwidths, their associated error and the optimal bandwidth, as a list.
  if (use.default.range){
    h_ref <- 0.5*(sd(x) + sd(y))/length(x)^(1/6)
    minimum <- min(c(max_separation(x),max_separation(y)))
    range <- seq(minimum*0.1, 2*h_ref, length.out = precision)
  } else{
    range <- seq(min, max, length.out = precision)
  }
  if (randomise){
    z <- z + rnorm(length(z), sd = sd(z)/15)
  }
  errors <- loo_funct3d_ll(range, x, y, z)
  nas <- which(is.na(errors))
  errors[nas] <- max(na.omit(errors))
  index <- which(errors == min(errors))
  if(index == 1 | index == precision){
    print("Attention! The result might be suboptimal!")
  }
  return(list(widths = range, errors = errors, hst = range[index]))
}
