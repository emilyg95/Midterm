#' Calculating Prior 
#'
#' Calculating Normal Distribution of Theta 
#'
#' @slot theta A numerical input for theta
#'
#' @return A numeric representing the height of the normal curve
#'  \item{dnorm(theta, mean = 0, sd = 3)}{The normal distribution of theta with mean of 0 and standard deviation of 3} 
#'  
#' @author Emily Garner<\email{emily.garner@@wustl.edu}>
#' @examples
#' 
#' Prior(4)
#' @seealso
#' @rdname Prior
#' @export
setGeneric("Prior", function(theta){ ## creates the interior function for set generic
  standardGeneric("Prior")
}) ## sets generic for Prob function

#' @exportMethod 
setMethod("Prior", signature(theta = "numeric"), ## sets method of Prob function for input of class theta
          function(theta){
            return(dnorm(theta, mean = 0, sd = 3))}
)