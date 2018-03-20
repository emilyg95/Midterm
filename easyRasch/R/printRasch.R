#' Printing an EAP and Student Name
#'
#' Print method for Rasch objects which calculates the EAP and prints the result as well as the test taker's name
#'
#' @slot x An object of class Rasch
#'
#' @return A list containing the output of the EAP calculation and the student's name
#'  \item{list(x@name, "EAP" = EAP)}{A list containing the output of the EAP calculation and the student's name}
#'  
#' @author Emily Garner<\email{emily.garner@@wustl.edu}>
#' @examples
#' 
#' myRasch <- newRasch("Emily", c(3, 4, 12), c(1, 1, 0))
#' printRasch(myRasch)
#' @seealso
#' @rdname printRasch
#' @include newRasch.R
#' @include EAP.R
#' @include Prior.R
#' @include Like.R
#' @include Prob.R
#' @exportMethod Integral
setMethod("print", signature(x = "Rasch"), ## sets method for print function which already has a generic with input x where x must be a Rasch object
                   function(x){ ## defines this function as taking the EAP of the input x and returning a list of the name slot of the Rasch object and the EAP value
                     EAP = EAP(x, lower = -6, upper = 6)
                     return(list(x@name, "EAP" = EAP))
                   })
