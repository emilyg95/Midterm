#' Create new Rasch Object
#'
#' Creates a new object of S4 class Rasch
#'
#' @slot name A character object indicating the name of the test taker
#' @slot a A numeric object indicating a vector of question difficulty parameters
#' @slot y a A numeric object with the same dimensionality as \code{a} comprised of zeroes and ones indicating a vector of answers for the respondent
#'
#' @return A list with the elements
#'  \item{x}{The first object input} 
#'  \item{y}{The second object input}
#'  \item{z}{The third object input}
#'  
#' @author Emily Garner<\email{emily.garner@@wustl.edu}>
#' @examples
#' 
#' myA <- seq(3,12,3) 
#' myY <- c(1, 1, 0) 
#' newRasch <- ("Emily", myA, myY)
#' @seealso
#' @rdname newRasch
#' @exportClass Rasch
setClass(Class = "Rasch", ## creates a new class "Rasch"
         representation = representation(
           name = "character",
           a = "numeric",
           y = "numeric"
         ) ## indicates 3 inputs for the class; a and y, which must both be numeric, and name, which must be a character
)

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

checkValidityRasch = function(object){ ## feeds checkLength and checkY into validity function for Rasch
  if (checkLength(object) != TRUE | checkY(object) != TRUE){ ## returns an error message if the test does not pass
    return("object is not a valid value")}
  else{
    return(TRUE)}
}

setValidity("Rasch", checkValidityRasch) ## sets checkValidityRasch as the constraints for an object to be of class Rasch

#' @export
newRasch = function(x, y, z){ ## construction function takes in 3 arguments; a, y, and name
  object = new("Rasch", name = x, a = y, y = z) ## creates a new object setting the 3 inputs as defined for class Rasch
  return(object) ## returns the object
}