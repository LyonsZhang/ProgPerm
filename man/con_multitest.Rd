\name{con_multitest}
\alias{con_multitest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{con_multitest}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
con_multitest(i, k, n, variable, select, testdata, testlevel, method, psigalpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i}{
%%     ~~Describe \code{i} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{variable}{
%%     ~~Describe \code{variable} here~~
}
  \item{select}{
%%     ~~Describe \code{select} here~~
}
  \item{testdata}{
%%     ~~Describe \code{testdata} here~~
}
  \item{testlevel}{
%%     ~~Describe \code{testlevel} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{psigalpha}{
%%     ~~Describe \code{psigalpha} here~~
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
function (i, k, n, variable, select, testdata, testlevel, method, 
    psigalpha = 0.05) 
{
    group <- testdata[[variable]]
    indx <- 1:n
    indx0 <- sample(indx, n - k)
    indx1 <- indx[!indx \%in\% indx0]
    group[indx1] <- group[sample(indx1)]
    otu.test <- apply(testlevel, 2, function(x) {
        ct <- cor.test(x = x, y = group, method = method)
        return(data.frame(pv = ct$p.value, es = ct$statistic, 
            cr = ct$estimate))
    })
    otu.test <- do.call(rbind, otu.test)
    psig <- sum(!otu.test$pv > psigalpha, na.rm = T)
    pvalue <- -log10(otu.test$pv[select])
    effectsize <- otu.test$es[select]
    correlation <- otu.test$cr[select]
    return(list(psig = psig, pvalue = pvalue, effectsize = effectsize, 
        correlation = correlation))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
