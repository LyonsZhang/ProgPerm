\name{con_permute_best}
\alias{con_permute_best}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{con_permute_best}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
con_permute_best(variable, testdata, top_pm, zoomn, alpha = 0.05, psigalpha = 0.05, method = "kendall", goodtype = "goodpvname", initial_fun = con_true_initial, test_fun = con_multitest, exchange_fun = con_exchange, paral = "yes")
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
    method = "kendall", goodtype = "goodpvname", initial_fun = con_true_initial, 
    test_fun = con_multitest, exchange_fun = con_exchange, paral = "yes") 
{
    trueinitial <- initial_fun(variable = variable, testdata = testdata, 
        top_pm = top_pm, method = method, goodtype = goodtype)
    select <- trueinitial$select
    selname <- trueinitial$selname
    n <- trueinitial$n
    testlevel <- trueinitial$testlevel
    psig0 <- sum(!trueinitial$otu.test$pv > psigalpha, na.rm = T)
    pvalue0 <- -log10(trueinitial$otu.test$pv[select])
    effectsize0 <- trueinitial$otu.test$es[select]
    correlation0 <- trueinitial$otu.test$cr[select]
    out <- exchange_fun(k = n, top_pm = top_pm, n = n, zoomn = zoomn, 
        variable = variable, select = select, selname = selname, 
        testdata = testdata, testlevel = testlevel, alpha = alpha, 
        psigalpha = psigalpha, method = method, test_fun = test_fun, 
        paral = paral)
    goodpsigname <- selname[!(out[["psigdist"]][, "q1"] <= psig0 & 
        psig0 <= out[["psigdist"]][, "q2"])]
    goodpvname <- selname[!(out[["pvdist"]][, "q1"] <= pvalue0 & 
        pvalue0 <= out[["pvdist"]][, "q2"])]
    goodesname <- selname[!(out[["esdist"]][, "q1"] <= effectsize0 & 
        effectsize0 <= out[["esdist"]][, "q2"])]
    goodcrname <- selname[!(out[["crdist"]][, "q1"] <= correlation0 & 
        correlation0 <= out[["crdist"]][, "q2"])]
    return(list(psig0 = psig0, pvalue0 = pvalue0, effectsize0 = effectsize0, 
        correlation0 = correlation0, selname = selname, worst_case = out, 
        goodpsigname = goodpsigname, goodpvname = goodpvname, 
        goodesname = goodesname, goodcrname = goodcrname))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
