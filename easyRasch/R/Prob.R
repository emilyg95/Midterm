#' Calculating Rasch Probability 
#'
#' Calculating the Probability That the Respondent Answered Correctly or Incorrectly
#'
#' @slot raschObj An object of class Rasch
#' @slot theta A numerical input for theta
#'
#' @return A list with the input elements and the answer
#'  \item{final}{A dataframe comprised of columns P and PQ} 
#'  
#' @author Emily Garner<\email{emily.garner@@wustl.edu}>
#' @examples
#' 
#' myRasch <- newRasch("Emily", c(3, 4, 12), c(1, 1, 0))
#' Prob(myRasch, theta = 4)
#' @seealso newRasch
#' @rdname Prob
#' @include newRasch.R
#' @export
setGeneric("Prob", function(raschObj, theta){
  standardGeneric("Prob")
}) ## sets generic for Prob function

#' @exportMethod 
setMethod("Prob", signature(raschObj = "Rasch", theta = "numeric"), ## sets method of Prob function for inputs of class Rasch and theta
          function(raschObj, theta){
            a = raschObj@a ## separates a and defines it as its own value for use in P
            functionP = function(theta, a){ ## function to calculate the probability of a student getting a question right - takes inputs theta and a as specified in the instructions
              top = exp(theta-a) ## creates the numerator of the P equation
              bottom = 1 + exp(theta-a) ## creates the denominator of the P equation
              P = top/bottom ## calculates P
              return(P) ## returns output
            }
            P = functionP(theta, a)
            Q = 1-P ## defines Q as 1-P
            y = raschObj@y ## defines y input as a separate vector y
            matrix = cbind(P, Q, y) ## creates a matrix with 3 columns with values P, Q, and Y
            dataframe = as.data.frame(matrix) ## turns the matrix into a dataframe
            dataframe$PQ = ifelse(dataframe$y == 1, dataframe$P, dataframe$Q) ## creates new column PQ which contains P if the corresponding y value is 1 (the student got the question correct) and Q if y is zero (the student got the question wrong)
            final = dataframe ## names the dataframe final
            final$y = NULL ## removes column y for clean output purposes
            final$Q = NULL ## removes column Q for clean output purposes
            return(final) ## returns the edited dataframe with only P and PQ labeled accordingly
          })