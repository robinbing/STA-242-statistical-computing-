\name{summary.BMLGrid}
\alias{summary.BMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
summary method for class 'BMLGrid'
}
\description{
## S3 method for class 'BMLGrid'
summary(CarMatrix)
}

\usage{
summary.BMLGrid(grid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{
  a matrix which has a BMLGrid class
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{car.num}{the total number of cars}
\item{blue.num}{the total number of blue cars}
\item{red.num}{the total number of red cars}
\item{notmovBlue}{the blue cars which are blocked}
\item{notmovRed}{the red cars which are blockes}
\item{velocityBlue}{the velocity of Blue cars}
\item{velocityRed}{the velocity of Blue cars}
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
grid = creatBMLGrid(100,100,p=0.3)
sum = summary(grid)

## The function is currently defined as
function (grid) 
{
    r = nrow(grid)
    c = ncol(grid)
    car.num = length(which(grid != 0))
    blue.num = length(which(grid == 1))
    red.num = length(which(grid == 3))
    velocityBlue = round(carVelocity(grid, 1), 3)
    velocityRed = round(carVelocity(grid, 3), 3)
    notmovBlue = blue.num - velocityBlue * blue.num
    notmovRed = red.num - velocityBlue * red.num
    cat(" This is a ", r, "*", c, "GRID:", "\n", "There are total", 
        car.num, "CARS:", "\n", "                  BLUE               RED             ", 
        "\n", "NUM           ", blue.num, "               ", 
        red.num, "\n", "BLOCKED NUM   ", notmovBlue, "                ", 
        notmovRed, "\n", "VELOCITY      ", velocityBlue, "              ", 
        velocityRed, "\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
