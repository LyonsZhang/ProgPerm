\name{bin_permute_best}
\alias{bin_permute_best}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{bin_permute_best}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bin_permute_best(variable, testdata, top_pm, zoomn, alpha = 0.05, psigalpha = 0.05, method = "kruskal", goodtype = "goodpvname", initial_fun = bin_true_initial, test_fun = bin_multitest, exchange_fun = bin_exchange, paral = "yes")
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
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{psigalpha}{
%%     ~~Describe \code{psigalpha} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{goodtype}{
%%     ~~Describe \code{goodtype} here~~
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
function (variable, testdata, top_pm, zoomn, alpha = 0.05, psigalpha = 0.05, 
    method = "kruskal", goodtype = "goodpvname", initial_fun = bin_true_initial, 
    test_fun = bin_multitest, exchange_fun = bin_exchange, paral = "yes") 
{
    trueinitial <- initial_fun(variable = variable, testdata = testdata, 
        top_pm = top_pm, method = method, goodtype = goodtype)
    select <- trueinitial$select
    selname <- trueinitial$selname
    position0 <- trueinitial$position0
    position1 <- trueinitial$position1
    levs <- trueinitial$levs
    n <- trueinitial$n
    N <- trueinitial$N
    testlevel <- trueinitial$testlevel
    psig0 <- sum(!trueinitial$otu.test$pv > psigalpha, na.rm = T)
    pvalue0 <- -log10(trueinitial$otu.test$pv[select])
    effectsize0 <- trueinitial$otu.test$es
    effectsize0 <- effectsize0[select]
    out <- exchange_fun(k = n, top_pm = top_pm, n = n, zoomn = zoomn, 
        variable = variable, select = select, selname = selname, 
        testdata = testdata, testlevel = testlevel, levs = levs, 
        position0 = position0, position1 = position1, alpha = alpha, 
        psigalpha = psigalpha, method = method, test_fun = test_fun, 
        paral = paral)
    goodpsigname <- selname[!(out[["psigdist"]][, "q1"] <= psig0 & 
        psig0 <= out[["psigdist"]][, "q2"])]
    goodpvname <- selname[!(out[["pvdist"]][, "q1"] <= pvalue0 & 
        pvalue0 <= out[["pvdist"]][, "q2"])]
    goodesname <- selname[!(out[["esdist"]][, "q1"] <= effectsize0 & 
        effectsize0 <= out[["esdist"]][, "q2"])]
    return(list(psig0 = psig0, pvalue0 = pvalue0, effectsize0 = effectsize0, 
        selname = selname, worst_case = out, goodpsigname = goodpsigname, 
        goodpvname = goodpvname, goodesname = goodesname))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
