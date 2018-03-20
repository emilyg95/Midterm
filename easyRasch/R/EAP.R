#' Calculating Expected a Posteriori Value for Theta 
#'
#' Calculating the Likelihood That a Proposed Value of Theta is Correct
#'
#' @slot raschObj An object of class Rasch
#' @slot lower A numerical input the lower bound defaulted to -6
#' @slot upper A numerical input the upper bound defaulted to 6
#'
#' @return A numeric representing the likelihood theta is correct
#'  \item{Top/Bottom}{expected a posteriori value for theta} 
#'  
#' @author Emily Garner<\email{emily.garner@@wustl.edu}>
#' @examples
#' 
#' myRasch <- newRasch("Emily", c(3, 4, 12), c(1, 1, 0))
#' EAP(myRasch, lower = -6, upper = 6)
#' @seealso newRasch
#' @rdname EAP
#' @include newRasch.R
#' @include Prob.R
#' @include Like.R
#' @include Prior.R
#' @export
setGeneric("EAP", function(raschObj, lower = -6, upper = 6){ ## creates the interior function for set generic
  standardGeneric("EAP")
}) ## sets generic for EAP function

Num = function(raschObj, theta){ ## function to calculate the numerator of the EAP formula - takes inputs Rasch object and theta as specified by the question
  Like = Like(raschObj, theta) ## calls like function from earlier
  Prior = Prior(theta) ## calls prior function from earlier 
  Num = theta*Like*Prior ## output multiplies like, prior, and theta
  return(Num) ## returns output
}

Den = function(raschObj, theta){ ## function to calculate the denominator of the EAP formula - takes inputs Rasch object and theta as specified by the question
  Like = Like(raschObj, theta) ## calls like function from earlier
  Prior = Prior(theta) ## calls prior function from earlier 
  Den = Like*Prior ## output multiplies like and prior
  return(Den) ## returns output
}

#' @export
setMethod("EAP", signature(raschObj = "Rasch", lower = "numeric", upper = "numeric"), ## sets method of Like function for inputs of class Rasch and theta
          function(raschObj, lower = -6, upper = 6){
            Top = integrate(Num, raschObj = raschObj, lower = -6, upper = 6) ## integration function for the numerator over theta using Num 
            Top = Top$value ## separates out just the value of the integration
            Bottom = integrate(Den, raschObj = raschObj, lower = -6, upper = 6) ## integration function for the denominator over theta using Den
            Bottom = Bottom$value ## separates out just the value of the integration
            return(Top/Bottom) ## returns the numerator value divided by the denominator value
          })