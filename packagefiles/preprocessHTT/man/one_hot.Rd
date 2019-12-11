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
##
> one_hot(mtcars, c("gear", "cyl"))
                     mpg  disp  hp drat    wt  qsec vs am carb gear.3 gear.4 gear.5 cyl.4
Mazda RX4           21.0 160.0 110 3.90 2.620 16.46  0  1    4      0      1      0     0
Mazda RX4 Wag       21.0 160.0 110 3.90 2.875 17.02  0  1    4      0      1      0     0
Datsun 710          22.8 108.0  93 3.85 2.320 18.61  1  1    1      0      1      0     1
Hornet 4 Drive      21.4 258.0 110 3.08 3.215 19.44  1  0    1      1      0      0     0
Hornet Sportabout   18.7 360.0 175 3.15 3.440 17.02  0  0    2      1      0      0     0
                    cyl.6 cyl.8
Mazda RX4               1     0
Mazda RX4 Wag           1     0
Datsun 710              0     0
Hornet 4 Drive          1     0
Hornet Sportabout       0     1
...

## the function is currently defined as
one_hot <- function (data, col)
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
