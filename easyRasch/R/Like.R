#' Calculating Theta Likelihood 
#'
#' Calculating the Likelihood That a Proposed Value of Theta is Correct
#'
#' @slot raschObj An object of class Rasch
#' @slot theta A numerical input for theta
#'
#' @return A numeric representing the likelihood theta is correct
#'  \item{prod(PQ)}{The product of vector output PQ} 
#'  
#' @author Emily Garner<\email{emily.garner@@wustl.edu}>
#' @examples
#' 
#' myRasch <- newRasch("Emily", c(3, 4, 12), c(1, 1, 0))
#' Like(myRasch, theta = 4)
#' @seealso newRasch
#' @rdname Like
#' @include newRasch.R
#' @include Prob.R
#' @export
setGeneric("Like", function(raschObj, theta){ ## creates the interior function for set generic
  standardGeneric("Like")
}

#' @exportMethod 
setMethod("Like", signature(raschObj = "Rasch", theta = "numeric"), ## sets method of Prob function for inputs of class Rasch and theta
          function(raschObj, theta){
            Prob = Prob(raschObj, theta)
            PQ = as.vector(Prob$PQ) ## separates column PQ from Prob output and makes it its own vector
            return(prod(PQ)) ## returns the product of the vector
          })