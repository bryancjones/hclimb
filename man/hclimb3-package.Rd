\name{hclimb3-package}
\alias{hclimb3-package}
\alias{hclimb3}
\docType{package}
\title{
hclimb: Local Hillclimbing on Economic Landscape
}
\description{
Replicating the ideas of David Kane's 1996 paper, hclimb simulates a profit landscape through the hillclimb function.
hillclimb() then surveys potential moves for a firm given a random budget allocation and moves in the direction of profit that aligns with one of three firm strategies:
steepest ascent, median ascent, or least ascent (in terms of profit movement).
See References for more information
}
\details{
\tabular{ll}{
Package: \tab hclimb3\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-02-03\cr
License: GPL-2
}

}
\author{
Bryan Jones bryan.c.jones@williams.edu

}
\references{
http://www.santafe.edu/media/workingpapers/96-08-065.pdf
}
\keyword{ package }

\examples{
hillclimg(50, 20)
}
