### Regression in R^2 ###
reg_point_lc <- function(x0, x, y, width){
  # Returns the kernel regression estimate at 'x0' with respect to samples 'x' and 'y'.
  constant <- 1/sqrt(2*pi)
  weights <- constant * exp( -0.5*( (x0-x)/width )^2 )
  return(sum(weights*y)/sum(weights))
}
reg_point_lc <- Vectorize(reg_point_lc, vectorize.args = "x0")

reg_line_lc <- function(precision, x, y, width, use.default.grid = TRUE, min = NA, max = NA){
  # Returns the kernel regression estimates for a grid of values.
  if (use.default.grid){
    grid <- seq(min(x), max(x), length = precision)
  } else{
    grid <- seq(min, max, length = precision)
  }
  output <- reg_point_lc(grid, x, y, width)
  return(data.frame(x = grid, y = output))
}



### Regression Bandwidth Selection in R^2, LOOCV ###
reg_point_out_lc <- function(i, x, y, width){
  # LOO regression estimate for the 'i'-th observation.
  constant <- 1/sqrt(2*pi)
  weights <-  constant * exp( -0.5*( (x[i]-x[-i])/width )^2 )
  return(sum(weights*y[-i])/sum(weights))
}
reg_point_out_lc <- Vectorize(reg_point_out_lc, vectorize.args = "i")

loo_funct_lc <- function(width, x, y){
  # LOOCV error for a given bandwidth 'width' and two samples 'x' and 'y'.
  return(sum((y - reg_point_out_lc(1:length(x), x, y, width))^2)/length(x))
}
loo_funct_lc <- Vectorize(loo_funct_lc, vectorize.args = "width")
  
LOOCV_lc <- function(x, y, precision, use.default.range = TRUE, min = NA, max = NA, randomise = TRUE){
  # Chooses the optimal bandwidth for regression using the LOOCV method. Returns a vector of
  # proposed bandwidths, their associated error and the optimal bandwidth, as a list.
  if (use.default.range){
    h_ref <- sd(x)/length(x)^(1/5)
    range <- seq(0.05*h_ref, 1.5*h_ref, length.out = precision)
  } else{
    range <- seq(min, max, length.out = precision)
  }
  if (randomise){
    y <- y + rnorm(length(y), sd = sd(y)/15)
  } 
  errors <- loo_funct_lc(range, x, y)
  nas <- which(is.na(errors))
  errors[nas] <- max(na.omit(errors))
  index <- which(errors == min(errors))
  if(index == 1 | index == precision){
    print("Attention! The result might be suboptimal!")
  }
  return(list(widths = range, errors = errors, hst = range[index]))
}



### Regression in R^3 ###
reg_point3d_lc <- function(x0, y0, x, y, z, width){
  # Returns the kernel regression estimate at 'x0' with respect to samples 'x', 'y' and 'z'.
  constant <- 1/sqrt(2*pi)
  weights <- constant * exp( -0.5*( sqrt((x0-x)^2+(y0-y)^2)/width )^2 )
  return(sum(weights*z)/sum(weights))
}
reg_point3d_lc <- Vectorize(reg_point3d_lc, vectorize.args = c("x0", "y0"))

reg_space_lc <- function(precision, x, y, z, width, use.default.grid = TRUE, minx = NA, maxx = NA, miny = NA, maxy = NA){
  # Returns the kernel regression estimates for a grid of values. The grid is a vector, not a matrix.
  if (use.default.grid){
    gridx <- seq(min(x), max(x), length = precision)
    gridy <- seq(min(y), max(y), length = precision)
  } else{
    gridx <- seq(minx, maxx, length = precision)
    gridy <- seq(miny, maxy, length = precision)
  }
  gridxy <- expand.grid(gridx, gridy)
  output <- reg_point3d_lc(gridxy[,1], gridxy[,2], x, y, z, width)
  return(data.frame(x = gridxy[,1], y = gridxy[,2], z = output))
}



### Regression Bandwidth Selection in R^3, LOOCV ###
reg_point_out3d_lc <- function(i, x, y, z, width){
  # LOO regression estimate for the 'i'-th observation.
  constant <- 1/sqrt(2*pi)
  weights <- constant * exp( -0.5*( sqrt((x[i]-x[-i])^2 + (y[i]-y[-i])^2) /width )^2 )
  return(sum(weights*z[-i])/sum(weights))
}
reg_point_out3d_lc <- Vectorize(reg_point_out3d_lc, vectorize.args = "i")

loo_funct3d_lc <- function(width, x, y, z){
  # LOOCV error for a given bandwidth 'width' and samples 'x', 'y' and 'z'.
  return(sum((z - reg_point_out3d_lc(1:length(x), x, y, z, width))^2)/length(x))
}
loo_funct3d_lc <- Vectorize(loo_funct3d_lc, vectorize.args = "width")

LOOCV3d_lc <- function(x, y, z, precision, use.default.range = TRUE, min = NA, max = NA, randomise = TRUE){
  # Chooses the optimal bandwidth for regression using the LOOCV method. Returns a vector of
  # proposed bandwidths, their associated error and the optimal bandwidth, as a list.
  if (use.default.range){
    h_ref <- 0.5*(sd(x) + sd(y))/length(x)^(1/6)
    minimum <- min(c(max_separation(x),max_separation(y)))
    range <- seq(minimum*0.05, 1.5*h_ref, length.out = precision)
  } else{
    range <- seq(min, max, length.out = precision)
  }
  if (randomise){
    z <- z + rnorm(length(z), sd = sd(z)/15)
  }
  errors <- loo_funct3d_lc(range, x, y, z)
  nas <- which(is.na(errors))
  errors[nas] <- max(na.omit(errors))
  index <- which(errors == min(errors))
  if(index == 1 | index == precision){
    print("Attention! The result might be suboptimal!")
  }
  return(list(widths = range, errors = errors, hst = range[index]))
}
