\name{uniqueVar}
\alias{uniqueVar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Display Number of Unique Values
}
\description{
Displays number of unique values in of each columns, along with their data types.
}
\usage{
uniqueVar(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
the input dataset
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
> uniqueVar(mtcars)
              mpg       cyl       disp      hp        drat      wt        qsec
unique values "25"      "3"       "27"      "22"      "22"      "29"      "30"
class         "numeric" "numeric" "numeric" "numeric" "numeric" "numeric" "numeric"
              vs        am        gear      carb
unique values "2"       "2"       "3"       "6"
class         "numeric" "numeric" "numeric" "numeric"

## The function is currently defined as

uniqueVar <- function(data) {
  tp = rbind(rapply(data,function(x)length(unique(x))), rapply(mtcars,function(x)class(x)))
  rownames(tp) = c("unique values", "class")
  print(tp)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
