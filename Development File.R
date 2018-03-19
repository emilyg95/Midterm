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

checkValidityRasch = function(object){ ## feeds checkLength and checkY into validity function for Rasch
  if (checkLength(object) != TRUE | checkY(object) != TRUE){ ## returns an error message if the test does not pass
    return("object is not a valid value")}
  else{
    return(TRUE)}
}

setValidity("Rasch", checkValidityRasch) ## sets checkValidityRasch as the constraints for an object to be of class Rasch

########################################
checkValidityRasch(newRasch("Jacob", c(2,5,2,5,7), c(1,0,1,1,1))) ## test for error

checkValidityRasch(newRasch("Jacob", c(2,5,2,5,7), c(3,4,6,7,2))) ## test for error

newRasch("Jacob", c(2,5,2,5,7), c(3,4,6,7)) ## test for error

newRasch("Jacob", c(2,5,2,5,7), c(0,0,0,1,0)) ## test for working
########################################

P = function(theta, a){ ## function to calculate the probability of a student getting a question right - takes inputs theta and a as specified in the instructions
  top = exp(theta-a) ## creates the numerator of the P equation
  bottom = 1 + exp(theta-a) ## creates the denominator of the P equation
  P = top/bottom ## calculates P
  return(P) ## returns output
}

########################################
P(7, 8) ## test for P with a as single value
P(7, c(3,4,6,7,2)) ## test for P with a as vector

testP = P(7, c(3,4,6,7,2)) ## sample output of P
testQ = 1-testP ## sample output of Q
y = c(1,0,0,1,1) ## sample y vector

## development for Probability function
matrix = cbind(testP, testQ, y) ## playing around with organization - combines P Q and Y into a sample matrix
matrix
matrix[1,]
matrix[,3]
head.matrix(matrix, 4)
dataframe = as.data.frame(matrix) ## turns matrix into sample dataframe
dataframe[,3]
dataframe$y

dataframe$PQ = ifelse(dataframe$y == 1, dataframe$testP, dataframe$testQ) ## if else statement which creates a new column PQ in the sample dataframe that contains the input from column P if y = 1 and the input from column Q if y = 0
dataframe
########################################

Prob = function(raschObj, theta){ ## function to calculate probability of student getting the question right P and also Q of them getting it wrong - takes inputs of a Rasch object and theta as specified in the question
  P = P(theta, raschObj@a) ## calls P function from earlier
  Q = 1-P ## defines Q as 1-P
  y = raschObj@y ## defines y input as a separate vector y
  matrix = cbind(P, Q, y) ## creates a matrix with 3 columns with values P, Q, and Y
  dataframe = as.data.frame(matrix) ## turns the matrix into a dataframe
  dataframe$PQ = ifelse(dataframe$y == 1, dataframe$P, dataframe$Q) ## creates new column PQ which contains P if the corresponding y value is 1 (the student got the question correct) and Q if y is zero (the student got the question wrong)
  final = dataframe ## names the dataframe final
  final$y = NULL ## removes column y for clean output purposes
  final$Q = NULL ## removes column Q for clean output purposes
  return(final) ## returns the edited dataframe with only P and PQ labeled accordingly
}

########################################
testRasch = newRasch("Jacob", c(2,5,2,5,7), c(1,1,0,1,0)) ## sample Rasch
testP = P(5, testRasch@a) ## sample P using Rasch
testP
testQ = 1-testP ## sample Q
testQ
testMatrix = cbind(testP, testQ, testRasch@y) ## sample matrix using Rasch
testMatrix ## test - determined that matrix needed to be called with a separate value vector y created from the Rasch object or the column will not be labeled
testDataframe = as.data.frame(testMatrix) ## sample data frame with Rasch
testDataframe
colnames(testDataframe) = c("testP", "testQ", "testy") ## experimenting with naming
colnames(testDataframe)
print(testDataframe)
testDataframe$testPQ = ifelse(testDataframe$testy == 1, testDataframe$testP, testDataframe$testQ) ## new sample PQ
testDataframe$testPQ
testvectPQ = as.vector(testDataframe$testPQ) ## turns column PQ into its own vector
prod(testvectPQ) ## takes product of vector inputs

Prob(testRasch, 5)
########################################

Like = function(raschObj, theta){ ## Likelihood function that calculates likelihood of proposed theta - takes inputs of a Rasch object and theta as specified in the question 
  Prob = Prob(raschObj, theta) ## calls probability function from earlier
  PQ = as.vector(Prob$PQ) ## separates column PQ from Prob output and makes it its own vector
  return(prod(PQ)) ## returns the product of the vector
}

########################################
Like(testRasch, 5) ## test

testRasch2 = newRasch("Jacob", c(2,5,2,5,7), c(1,1,1,1,1)) ## new sample student who got every answer right

Like(testRasch2, 100) ## test

testRasch3 = newRasch("Jacob", c(12,2,3,23,2), c(1,1,1,1,0))

Like(testRasch3, 18)
########################################

Prior = function(theta){ ## creates function to calculate height of normal curve for proposed theta - takes input of theta as specified in the question
  return(dnorm(theta, mean = 0, sd = 3)) ## returns a normal curve with x = theta a mean of 0 and standard deviation of 3
}

########################################
Prior(5) ## test
########################################

########################################

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

########################################
Num(testRasch, 5) ## test
Den(testRasch, 5) ## test

IntNum = integrate(Num, raschObj = testRasch, lower = -6, upper = 6) ## test integral over theta using the Num function from earlier holding testRasch constant from -6 to 6
as.numeric(IntNum) ## shows that it's stored as a list
str(IntNum) ## shows list items
IntNum = IntNum$value ## isolates the value

IntDen = integrate(Den, raschObj = testRasch, lower = -6, upper = 6) ## test integral over theta using the Den function from earlier holding testRasch constant from -6 to 6
IntDen = IntDen$value ## isolates the value

IntNum/IntDen ## divides the test numerator integral by the test denominator integral
########################################

EAP = function(raschObj, lower = -6, upper = 6){ ## creates function for EAP value which takes a Rasch object as the input and holds -6 to 6 as the boundaries
  Top = integrate(Num, raschObj = raschObj, lower = -6, upper = 6) ## integration function for the numerator over theta using Num 
  Top = Top$value ## separates out just the value of the integration
  Bottom = integrate(Den, raschObj = raschObj, lower = -6, upper = 6) ## integration function for the denominator over theta using Den
  Bottom = Bottom$value ## separates out just the value of the integration
  return(Top/Bottom) ## returns the numerator value divided by the denominator value
}

########################################
EAP(testRasch) ## test
########################################

setMethod("print", signature(x = "raschObj"), ## sets method for print function which already has a generic with input x where x must be a Rasch object
          function(x){ ## defines this function as taking the EAP of the input x and returning a list of the name slot of the Rasch object and the EAP value
            EAP = EAP(x, lower = -6, upper = 6)
            return(list(x@name, "EAP" = EAP))
          })

########################################
print(testRasch) ## test

