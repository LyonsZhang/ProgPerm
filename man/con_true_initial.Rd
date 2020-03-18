\name{con_true_initial}
\alias{con_true_initial}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{con_true_initial}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
con_true_initial(variable, testdata, top_pm = 50, method = "kendall", goodtype = "goodpvname")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variable}{
%%     ~~Describe \code{variable} here~~
}
  \item{testdata}{
%%     ~~Describe \code{testdata} here~~
}
  \item{top_pm}{
%%     ~~Describe \code{top_pm} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{goodtype}{
%%     ~~Describe \code{goodtype} here~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (variable, testdata, top_pm = 50, method = "kendall", 
    goodtype = "goodpvname") 
{
    testlevel <- testdata
    testlevel[[variable]] <- NULL
    n <- dim(testdata)[1]
    group <- testdata[[variable]]
    indx <- 1:n
    indx0 <- sample(indx, n)
    indx1 <- indx[!indx \%in\% indx0]
    group[indx1] <- group[sample(indx1)]
    otu.test <- apply(testlevel, 2, function(x) {
        ct <- cor.test(x = x, y = group, method = method)
        return(data.frame(pv = ct$p.value, es = ct$statistic, 
            cr = ct$estimate))
    })
    otu.test <- do.call(rbind, otu.test)
    select <- switch(goodtype, goodpvname = order(otu.test$pv)[1:top_pm], 
        goodesname = order(abs(otu.test$es))[1:top_pm], goodcrname = order(abs(otu.test$cr))[1:top_pm], 
        stop("goodtype must be one of the follows: 'goodpvname', 'goodesname' and 'goodcrname'. "))
    selname <- row.names(otu.test)[select]
    return(list(select = select, selname = selname, testlevel = testlevel, 
        n = n, otu.test = otu.test))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
