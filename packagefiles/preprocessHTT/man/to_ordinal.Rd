\name{to_ordinal}
\alias{to_ordinal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert to Ordinal Data
}
\description{
convert selected columns in a given dataset to ordinal data (input has to be numerical columns),
return an editted dataset
}
\usage{
to_ordinal(data, col)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
input data
}
  \item{col}{
columns to convert to ordinal type
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
function (data, col)
{
    tp <- data
    for (name in col) {
        tp[, name] = factor(tp[, name])
    }
    return(tp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
