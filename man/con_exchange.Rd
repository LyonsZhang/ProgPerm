\name{con_exchange}
\alias{con_exchange}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{con_exchange}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
con_exchange(k, top_pm, n, zoomn, variable, select, selname, testdata, testlevel, alpha, psigalpha, method, test_fun = con_multitest, paral)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{top_pm}{
%%     ~~Describe \code{top_pm} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{zoomn}{
%%     ~~Describe \code{zoomn} here~~
}
  \item{variable}{
%%     ~~Describe \code{variable} here~~
}
  \item{select}{
%%     ~~Describe \code{select} here~~
}
  \item{selname}{
%%     ~~Describe \code{selname} here~~
}
  \item{testdata}{
%%     ~~Describe \code{testdata} here~~
}
  \item{testlevel}{
%%     ~~Describe \code{testlevel} here~~
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
  \item{test_fun}{
%%     ~~Describe \code{test_fun} here~~
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
function (k, top_pm, n, zoomn, variable, select, selname, testdata, 
    testlevel, alpha, psigalpha, method, test_fun = con_multitest, 
    paral) 
{
    if (paral == "no") {
        testresults <- lapply(1:(k * zoomn + 1), test_fun, k = k, 
            n = n, variable = variable, select = select, testdata = testdata, 
            testlevel = testlevel, method = method, psigalpha = psigalpha)
    }
    else if (paral == "yes") {
        require(doParallel)
        ncores <- parallel::detectCores()
        cl <- parallel::makeCluster(ncores)
        doParallel::registerDoParallel(cl)
        testresults <- foreach(i = 1:(k * zoomn + 1)) \%dopar\% 
            test_fun(i, k = k, n = n, variable = variable, select = select, 
                testdata = testdata, testlevel = testlevel, method = method, 
                psigalpha = psigalpha)
        parallel::stopCluster(cl)
    }
    else {
        stop("Enter something that parallels me!")
    }
    testresults <- lapply(purrr::transpose(testresults), function(l) do.call(rbind, 
        l))
    psig <- testresults[[1]]
    pvalue <- testresults[[2]]
    effectsize <- testresults[[3]]
    correlation <- testresults[[4]]
    dif <- k
    psigdist <- matrix(0, 1, 6)
    colnames(psigdist) <- c("mean", "std", "median", "q1", "q2", 
        "dif")
    psigdist[, 1] <- mean(psig)
    if (length(psig) == 1) {
        psigdist[, 2] <- 0
    }
    else {
        psigdist[, 2] <- sd(psig)
    }
    psigdist[, 3] <- median(psig)
    psigdist[, 4] <- quantile(psig, probs = alpha/2)
    psigdist[, 5] <- quantile(psig, probs = (1 - alpha/2))
    psigdist[, 6] <- dif
    pvdist <- matrix(0, top_pm, 8)
    colnames(pvdist) <- c("mean", "std", "median", "q1", "q2", 
        "variable", "dif", "rep")
    pvdist[, 1] <- colMeans(pvalue)
    if (dim(pvalue)[1] == 1) {
        pvdist[, 2] <- 0
        pvdist[, 8] <- pvalue
    }
    else {
        pvdist[, 2] <- matrixStats::colSds(pvalue)
        pvdist[, 8] <- pvalue[1, ]
    }
    pvdist[, 3] <- matrixStats::colMedians(pvalue)
    pvdist[, 4] <- matrixStats::colQuantiles(pvalue, probs = alpha/2)
    pvdist[, 5] <- matrixStats::colQuantiles(pvalue, probs = (1 - 
        alpha/2))
    pvdist[, 6] <- selname
    pvdist[, 7] <- dif
    esdist <- matrix(0, top_pm, 7)
    colnames(esdist) <- c("mean", "std", "median", "q1", "q2", 
        "variable", "dif")
    esdist[, 1] <- colMeans(effectsize)
    if (dim(effectsize)[1] == 1) {
        esdist[, 2] <- 0
    }
    else {
        esdist[, 2] <- matrixStats::colSds(effectsize)
    }
    esdist[, 3] <- matrixStats::colMedians(effectsize)
    esdist[, 4] <- matrixStats::colQuantiles(effectsize, probs = alpha/2)
    esdist[, 5] <- matrixStats::colQuantiles(effectsize, probs = (1 - 
        alpha/2))
    esdist[, 6] <- selname
    esdist[, 7] <- dif
    crdist <- matrix(0, top_pm, 7)
    colnames(crdist) <- c("mean", "std", "median", "q1", "q2", 
        "variable", "dif")
    crdist[, 1] <- colMeans(correlation)
    if (dim(correlation)[1] == 1) {
        crdist[, 2] <- 0
    }
    else {
        crdist[, 2] <- matrixStats::colSds(correlation)
    }
    crdist[, 3] <- matrixStats::colMedians(correlation)
    crdist[, 4] <- matrixStats::colQuantiles(correlation, probs = alpha/2)
    crdist[, 5] <- matrixStats::colQuantiles(correlation, probs = (1 - 
        alpha/2))
    crdist[, 6] <- selname
    crdist[, 7] <- dif
    return(list(psigdist = psigdist, pvdist = pvdist, esdist = esdist, 
        crdist = crdist))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
