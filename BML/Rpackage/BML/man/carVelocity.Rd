\name{carVelocity}
\alias{carVelocity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
      Calculate Car's Velocity
}
\description{
  This function calculate the cars velocity.
  We define velocity by fomula: 
  the number of red(blue) cars which can move / the number of whole red(blue) cars
}
\usage{
carVelocity(grid, carType)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{
    A Matrix of class "BMLGrid", typically result from creatBMLGrid and runBMLGrid.
}
  \item{carType}{
    Represent the types of car. Blue car denoted by 1 and red car denoted by 2. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{velocity}{The velocity of the car.}
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
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
vel = carVelocity(grid, 2)

## The function is currently defined as
function (grid, carType) 
{
    if (carType == 1) {
        grid.new = rbind(grid[nrow(grid), ], grid[1:(nrow(grid) - 
            1), ])
        blueMov = length(which(grid.new - grid == -1))
        velocity = blueMov/length(which(grid == 1))
    }
    else {
        grid.new = cbind(grid[, 2:ncol(grid)], grid[, 1])
        redMov = length(which(grid.new - grid == -3))
        velocity = redMov/length(which(grid == 3))
    }
    velocity
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
