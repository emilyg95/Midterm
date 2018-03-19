setClass(Class = "Rasch", ## creates a new class "Rasch"
         representation = representation(
           name = "character",
           a = "numeric",
           y = "numeric"
         ) ## indicates 3 inputs for the class; a and y, which must both be numeric, and name, which must be a character
)

newRasch = function(x, y, z){ ## construction function takes in 3 arguments; a, y, and name
  object = new("Rasch", name = x, a = y, y = z) ## creates a new object setting the 3 inputs as defined for class Rasch
  return(object) ## returns the object
}

########################################
newRasch("Jacob", c(2,5,2,5,7), c(3,4,6,7)) ## test for working

newRasch("Jacob", "c(2,5,2,5,7)", c(3,4,6,7)) ## test for error
########################################

checkLength = function(object){ ## creates a function to check if a and y are the same length and returns error message if not
  test1 = (length(object@a) == length(object@y))
  if (!test1){
    return("a and y must be of the same length")}
  else{
    return(TRUE)}
}

checkY = function(object){ ## creates a function to check if y values are 0 or 1 and returns error message if not
  y = as.vector(object@y) ## turns the y input into a separate vector
  ones = which(y == 1) ## creates a vector of the numbers of the slots with ones
  zeroes = which(y == 0) ## creates a vector of the numbers of the slots with zeroes
  appended = append(ones, zeroes) ## creates a new vector that appends the zeroes vector to the ones vector
  test1 = (length(appended) == length(y)) ## if there are only zeroes and ones in the original y vector, the length of the appended vector should be equal to the length of y
  if (!test1){
    return("y values must be 0 or 1")}
  else{
    return(TRUE)}
}

########################################
y = c(1,1,1,0,0) ## test y vector
testOnes = which(y == 1) ## test ones vector
length(testOnes) ## check length
testZeroes = which(y == 0) ## test zeroes vector
append(testOnes, testZeroes) ## test append vector

checkLength(newRasch("Jacob", c(2,5,2,5,7), c(3,4,6,7))) ## test for error

checkLength(newRasch("Jacob", c(2,5,2,5,7), c(3,4,6,7,8))) ## test for working

checkY(newRasch("Jacob", c(2,5,2,5,7), c(3,4,6,7,8))) ## test for error

checkY(newRasch("Jacob", c(2,5,2,5,7), c(0,0,0,1,1))) ## test for working
########################################





