\name{one_hot}
\alias{one_hot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
One Hot Encoding
}
\description{
This function performs one hot encoding to the selected columns in a given dataset
}
\usage{
one_hot(data, col)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
input dataset
}
  \item{col}{
columns to perform one hot encoding
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
    if (length(col) == 0) {
        return(data)
    }
    tp <- data
    for (name in col) {
        tp[, name] = as.factor(tp[, name])
    }
    dmy <- dummyVars(paste("~", paste(col, collapse = "+"), sep = ""),
        data = tp)
    trsf <- data.frame(predict(dmy, newdata = tp, na.action = na.omit))
    return(cbind(tp[, !(colnames(tp) \%in\% col)], trsf))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
