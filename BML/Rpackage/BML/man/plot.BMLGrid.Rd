\name{plot.BMLGrid}
\alias{plot.BMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot an image for an BMLGird object
}
\description{
  It will return a plot for a BMLGrid grid.
}
\usage{
plot.BMLGrid(grid, main)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{
    A mtrixc which has a BMLGrid class.
}
  \item{main}{
    The title of the plot.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
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
grid = creatBMLGrid(100,100,p = 0.3)
myplot = plot(grid, main ="shengjingbing")

## The function is currently defined as
function (grid, main) 
{
    image(t(grid[nrow(grid):1, ]), col = c("white", "blue", "red"), 
        main = main)
    box()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
