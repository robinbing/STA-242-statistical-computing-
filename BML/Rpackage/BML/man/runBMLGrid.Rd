\name{runBMLGrid}
\alias{runBMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Run a simulation for BML
}
\description{
    A funtion to simulate the cars moving in a BMLGrid for specific times.
}
\usage{
runBMLGrid(grid, numSteps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{
  A matrix which has a BMLGrid class
}
  \item{numSteps}{
  Number of steps that the simulation will run
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{grid }{A matrix which has a BMLGrid class}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
grid = creatBMLGrid(10,10,p=0.3)
runBMLGrid(grid,100)

function (grid, numSteps) 
{
    for (i in 1:numSteps) grid = moveCar.c(grid, i)
    grid
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
