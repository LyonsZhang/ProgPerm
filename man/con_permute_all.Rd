\name{con_permute_all}
\alias{con_permute_all}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{con_permute_all}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
con_permute_all(variable, testdata, top_pm, zoomn, sequence = NULL, method = "kendall", goodtype = "goodpvname", alpha = 0.05, psigalpha = 0.05, initial_fun = con_true_initial, test_fun = con_multitest, exchange_fun = con_exchange, paral = "no")
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
  \item{zoomn}{
%%     ~~Describe \code{zoomn} here~~
}
  \item{sequence}{
%%     ~~Describe \code{sequence} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{goodtype}{
%%     ~~Describe \code{goodtype} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{psigalpha}{
%%     ~~Describe \code{psigalpha} here~~
}
  \item{initial_fun}{
%%     ~~Describe \code{initial_fun} here~~
}
  \item{test_fun}{
%%     ~~Describe \code{test_fun} here~~
}
  \item{exchange_fun}{
%%     ~~Describe \code{exchange_fun} here~~
}
  \item{paral}{
%%     ~~Describe \code{paral} here~~
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
function (variable, testdata, top_pm, zoomn, sequence = NULL, 
    method = "kendall", goodtype = "goodpvname", alpha = 0.05, 
    psigalpha = 0.05, initial_fun = con_true_initial, test_fun = con_multitest, 
    exchange_fun = con_exchange, paral = "no") 
{
    trueinitial <- initial_fun(variable = variable, testdata = testdata, 
        top_pm = top_pm, method = method, goodtype = goodtype)
    select <- trueinitial$select
    selname <- trueinitial$selname
    n <- trueinitial$n
    testlevel <- trueinitial$testlevel
    if (is.null(sequence)) {
        sequence <- round(seq(0, n, length.out = 10))
    }
    require(doParallel)
    ncores <- parallel::detectCores()
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    out <- {
        foreach(k = sequence) \%dopar\% exchange_fun(k, top_pm = top_pm, 
            n = n, zoomn = zoomn, variable = variable, select = select, 
            selname = selname, testdata = testdata, testlevel = testlevel, 
            alpha = alpha, psigalpha = psigalpha, method = method, 
            test_fun = test_fun, paral = paral)
    }
    parallel::stopCluster(cl)
    out <- do.call(Map, c(f = rbind, out))
    return(list(alldist = out, selname = selname))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
