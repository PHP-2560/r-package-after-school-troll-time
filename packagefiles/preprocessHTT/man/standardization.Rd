\name{standardization}
\alias{standardization}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standardization
}
\description{
Standardize selected columns in a given dataset, return a standardizer that can be used to standardize datasets using "predict" function (see examples)
}
\usage{
standardization(data, col)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
input dataset
}
  \item{col}{
columns to perform standardization
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
##
> standardizer <- standardization(mtcars, c("mpg","wt"))
> predict(standardizer, mtcars)
                            mpg cyl  disp  hp drat           wt  qsec vs am gear carb
Mazda RX4            0.15088482   6 160.0 110 3.90 -0.610399567 16.46  0  1    4    4
Mazda RX4 Wag        0.15088482   6 160.0 110 3.90 -0.349785269 17.02  0  1    4    4
Datsun 710           0.44954345   4 108.0  93 3.85 -0.917004624 18.61  1  1    4    1
Hornet 4 Drive       0.21725341   6 258.0 110 3.08 -0.002299538 19.44  1  0    3    1
Hornet Sportabout   -0.23073453   8 360.0 175 3.15  0.227654255 17.02  0  0    3    2
...

## The function is currently defined as
function (data, col)
{
    tp <- data
    std_trained <- preProcess(data[col], method = c("center",
        "scale"), na.remove = TRUE)
    std_trained
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
