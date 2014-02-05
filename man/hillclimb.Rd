\name{hillclimb}
\alias{hillclimb}
\title{
  Local Hillclimbing on Economic Landscape (Kane 1996)
}
\description{
The function hillcimb simulates a profit landscape and illustrate outcomes of three firm strategies in replication of the ideas of David Kane's 1996 paper "Local Hillclimbing on an Economic Landscape." 
http://www.santafe.edu/media/workingpapers/96-08-065.pdf
Profit maxima are normalized and multiplied by 100 in output for readability.
}
\usage{
hillclimb = function(budget, inputs, connections=1, 
  iterations=1, coeffs.upper=1, coeffs.lower=-1, 
    coeffs.upper.square = 1, coeffs.lower.square = -1, 
      coeffs.upper.cross=1, coeffs.lower.cross=-1)
}
\arguments{
  \item{budget}{
The total budget a simulated firm has to allocate to its range of inputs: must be an integer in [0, inf)
}
  \item{inputs}{
The total number of inputs to which a simulated firm can choose allocate to its budget: must be an integer in [1, inf)
}
  \item{connections}{
The number of other inputs to which a single input can move one unit of its budget in a single move (or how many connections define the profit neighborhood): must be an integer  in [1, inputs]; default, connections = 1; note, profit maxima continue to increase to the point where connections = 1/2 of inputs
}
  \item{iterations}{
The number times the simulation is run: must be an integer in [1,inf); default, iterations = 1
}
  \item{coeffs.upper}{
The upper bound of randomly assigned coefficients for terms in the profit function: must be greater than coeffs.lower; default, coeffs.upper = 1
}
  \item{coeffs.lower}{
The lower bound of randomly assigned coefficients for terms in the profit function: must be less than coeffs.upper; default, coeffs.lower = -1
}
  \item{coeffs.upper.square}{
The upper bound of randomly assigned coefficients for square terms in the profit function: must be greater than coeffs.lower; default, coeffs.upper.square = 1
}
  \item{coeffs.lower.square}{
The lower bound of randomly assigned coefficients for square terms in the profit function: must be less than coeffs.upper; default, coeffs.lower.square = -1
}
  \item{coeffs.upper.cross}{
The upper bound of randomly assigned coefficients for cross product terms in the profit function: must be greater than coeffs.lower; default, coeffs.upper.cross = 1
}
  \item{coeffs.lower.cross}{
The lower bound of randomly assigned coefficients for cross product terms in the profit function: must be less than coeffs.upper; default, coeffs.lower.cross = -1
}
}

\references{
Kane, David. (1996) "Local Hillclimbing on an Economic Landscape." Santa Fe Institute. Working Paper.
}
\author{
Bryan Jones, Williams College '16 
bryan.c.jones@williams.edu
}

\seealso{
http://www.santafe.edu/media/workingpapers/96-08-065.pdf
}
\examples{

## Simple Example
hillclimb(budget = 50, inputs = 20, connections = 3, iterations = 2, coeffs.upper = 1, coeffs.lower = -1)

}

\keyword{function}
\keyword{hclimb}
\keyword{hillclimb}
