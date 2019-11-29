\name{train_test_split}
\alias{train_test_split}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Train Test Split
}
\description{
This function can split the input data into two dataset. One for training and one for testing.
}
\usage{
train_test_split(data, test_perc = 0.2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
the input data
}
  \item{test_perc}{
percentage of testing data
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
function (data, test_perc = 0.2)
{
    spec = c(train = 1 - test_perc, test = test_perc)
    g = sample(cut(seq(nrow(data)), nrow(data) * cumsum(c(0,
        spec)), labels = names(spec)))
    return(split(data, g))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
