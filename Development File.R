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



