\name{bin_multitest}
\alias{bin_multitest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{bin_multitest}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bin_multitest(i, k, variable, select, testdata, testlevel, levs, position0, position1, method, psigalpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i}{
%%     ~~Describe \code{i} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
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
  \item{levs}{
%%     ~~Describe \code{levs} here~~
}
  \item{position0}{
%%     ~~Describe \code{position0} here~~
}
  \item{position1}{
%%     ~~Describe \code{position1} here~~
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
function (i, k, variable, select, testdata, testlevel, levs, 
    position0, position1, method, psigalpha = 0.05) 
{
    group <- testdata[[variable]]
    n1 <- length(position0)
    n2 <- length(position1)
    group[sample(position0, k)] <- levs[2]
    group[sample(position1, k)] <- levs[1]
    group <- factor(group)
    if (method == "kruskal") {
        otu.test <- apply(testlevel, 2, function(x) {
            kt <- kruskal.test(x = x, g = group)
            return(data.frame(pv = kt$p.value, es = kt$statistic/sqrt(n1^2 + 
                n2^2)))
        })
        otu.test <- do.call(rbind, otu.test)
    }
    else if (method == "wilcox") {
        otu.test <- apply(testlevel, 2, function(x) {
            wt <- wilcox.test(x ~ group)
            return(data.frame(pv = wt$p.value, es = wt$statistic/sqrt(n1^2 + 
                n2^2)))
        })
        otu.test <- do.call(rbind, otu.test)
    }
    psig <- sum(!otu.test$pv > psigalpha, na.rm = T)
    pvalue <- -log10(otu.test$pv[select])
    effectsize <- otu.test$es
    effectsize <- effectsize[select]
    return(list(psig = psig, pvalue = pvalue, effectsize = effectsize))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
