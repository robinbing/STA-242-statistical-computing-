\name{creatBMLGrid}
\alias{creatBMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Create a BML Grid
}
\description{
  This function creat a grid and stored in a matrix. The red and blue cars which have specific
  number are randomly located. And define the class of grid as bot matrix and BMLGrid.
}
\usage{
creatBMLGrid(r = 100, c = 99, ncars = c(blue = 100, red = 100), p = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{
    The number of row of the BMLGrid
}
  \item{c}{
    number of column of the BMLGrid
}
  \item{ncars}{
    a vector including the number of red and blue cars separately
}
  \item{p}{
    the density of the cars. And if we have p, we will ignore the value of given ncars;
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{grid}{A BMLGrid.}
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
grid = creatBMLGrid(10,10,p = 0.3)
grid = creatBMLGrid(10,10,c(3,3))

## The function is currently defined as
function (r = 100, c = 99, ncars = c(blue = 100, red = 100), 
    p = 0) 
{
    if (p != 0) {
        blue = red = ceiling(r * c * p/2)
        ncars = c(blue, red)
    }
    grid = matrix(0, r, c)
    pos = sample(r * c, sum(ncars))
    grid[pos] = sample(rep(c(1, 3), times = ncars))
    class(grid) = c("BMLGrid", class(grid))
    grid
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
