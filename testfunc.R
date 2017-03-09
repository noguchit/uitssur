myfct2 <- function(x1=5, opt_arg) {
  if(missing(opt_arg)) { # 'missing()' is used to test whether a value was specified as an argument
    z1 <- 1:10 
  } else {
    z1 <- opt_arg 
  }   
  cat("my function returns:", "\n")
  return(z1/x1)
}  
myfct2(x1=5