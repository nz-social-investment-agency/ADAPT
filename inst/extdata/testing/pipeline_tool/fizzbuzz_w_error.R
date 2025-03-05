# Documentation header
# 2025-02-26

new_function = function(x, y, z){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(y))
  stopifnot(is.numeric(z))

  if(x %% y == 0 & x %% z == 0){
    return("fuzzbuzz")
  }
  if(x %% y == 0){
    return("fuzz")
  }
  if(x %% z == 0){
    return("buzz")
  }
  return(as.character(x))
}
  
stop("test R error")
result = sapply(1:100, new_function, y = 5, z = 7, USE.NAMES = FALSE)
