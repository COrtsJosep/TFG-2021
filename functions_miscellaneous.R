### Useful Functions ###
max_separation <- function(x){
  # Sorts a vector and returns the max. separation between two consecutive values.
  distances <- c()
  sorted <- sort(x)
  for (i in 1:(length(x)-1)){
    distances <- append(distances, abs(sorted[i]-sorted[i+1]))
  }
  return(max(distances))
}

populated_neighborhood <- function(x0, y0, x, y, radius, minimum){
  # Returns TRUE if there are at least 'minimum' observations in a neighborhood of radius 'radius', FALSE otherwise.
  distances <- sqrt((x0-x)^2+(y0-y)^2)
  logical <- distances <= radius
  if (sum(logical) >= minimum){
    return(TRUE)
  } else{return(FALSE)}
}
populated_neighborhood <- Vectorize(populated_neighborhood, vectorize.args = c("x0", "y0"))

nullify <- function(colx, coly, colz, x, y, radius, minimum){
  # Changes an observation's value to NA if it is isolated enough or has a value of > 55.
  logical <- populated_neighborhood(colx, coly, x, y, radius, minimum)
  output <- colz
  for (i in 1:length(colx)){
    if(logical[i] == FALSE){
      output[i] <- NA
    }
    if (!is.na(output[i])){
      if (output[i] > 55 | output[i] < 5){
        output[i] <- NA
      }
    }
  }
  return(output)
}

na_to_text <- function(some_text){
  # If the input is a NA value, then '' is returned (empty string). Else the input is returned.
  if (is.na(some_text)){
    return('')
  } else {return(some_text)}
}
