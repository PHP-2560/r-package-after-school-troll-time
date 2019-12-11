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
##
> train_test_split(mtcars)
$train
                     mpg cyl  disp  hp drat    wt  qsec vs am gear carb
Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
...
$test
                   mpg cyl  disp  hp drat    wt  qsec vs am gear carb
Mazda RX4         21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
Hornet Sportabout 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
Merc 240D         24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
Merc 280          19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
Honda Civic       30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
...

## the function is currently defined a  s
train_test_split <- function (data, test_perc = 0.2)
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
