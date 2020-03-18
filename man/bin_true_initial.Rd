\name{bin_true_initial}
\alias{bin_true_initial}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{bin_true_initial}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bin_true_initial(variable, testdata, top_pm = 50, method = "kruskal", goodtype = "goodpvname")
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
function (variable, testdata, top_pm = 50, method = "kruskal", 
    goodtype = "goodpvname") 
{
    response <- testdata[[variable]]
    levs <- unique(response)
    if (length(levs) != 2) {
        stop("The variable does not have two levels.")
    }
    else {
        position0 <- grep(levs[1], response)
        position1 <- grep(levs[2], response)
        fliplen <- min(length(position0), length(position1))
        N <- fliplen + 1
        n1 <- length(position0)
        n2 <- length(position1)
        testlevel <- testdata
        testlevel[[variable]] <- NULL
        n <- round((N + 1)/2)
        group <- factor(response)
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
                options(warn = -1)
                wt <- wilcox.test(x ~ group)
                return(data.frame(pv = wt$p.value, es = wt$statistic/sqrt(n1^2 + 
                  n2^2)))
            })
            otu.test <- do.call(rbind, otu.test)
        }
        else {
            stop("Set up method either as 'kruskal' or 'wilcox'!")
        }
        select <- switch(goodtype, goodpvname = order(otu.test$pv)[1:top_pm], 
            goodesname = order(abs(otu.test$es))[1:top_pm], stop("goodtype must be one of the follows: 'goodpvname' and 'goodesname'. "))
        selname <- row.names(otu.test)[select]
    }
    return(list(select = select, selname = selname, position0 = position0, 
        position1 = position1, levs = levs, testlevel = testlevel, 
        n = n, N = N, otu.test = otu.test))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
