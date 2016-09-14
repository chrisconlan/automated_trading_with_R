# Declare global variables a and b
a <- 2
b <- 3

# Declare functions
f <- function(){
  a
}

g <-function(){
  f() + b
}

h <- function(b){
  f() + b
}


# a = 2 throughout.
# b = 3 when not supplied as a parameter.
f() # f() = 2
g() # g() = 5
h(5) # h(5) = 7
