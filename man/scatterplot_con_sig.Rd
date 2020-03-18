\name{scatterplot_con_sig}
\alias{scatterplot_con_sig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{scatterplot_con_sig}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
scatterplot_con_sig(select, variable, testdata, folder, method = "kendall")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{select}{
%%     ~~Describe \code{select} here~~
}
  \item{variable}{
%%     ~~Describe \code{variable} here~~
}
  \item{testdata}{
%%     ~~Describe \code{testdata} here~~
}
  \item{folder}{
%%     ~~Describe \code{folder} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
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
function (select, variable, testdata, folder, method = "kendall") 
{
    dir.create(path = folder, showWarnings = FALSE)
    library("ggpubr")
    library("ggplot2")
    p <- ggscatter(testdata, x = variable, y = select, add = "reg.line", 
        conf.int = TRUE, cor.coef = TRUE, cor.method = method, 
        cor.coef.size = 8, xlab = variable, ylab = select) + 
        theme_bw() + theme(plot.title = element_text(size = 30, 
        face = "bold"), legend.title = element_text(size = 30), 
        legend.text = element_text(size = 16)) + theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(), panel.border = element_blank(), 
        panel.background = element_blank(), axis.line.x = element_line(color = "black", 
            size = 1), axis.line.y = element_line(color = "black", 
            size = 1), axis.text = element_text(size = 16, face = "bold"), 
        axis.title = element_text(size = 20, face = "bold"))
    p
    ggsave(filename = paste(folder, "/", select, ".pdf", sep = ""), 
        width = 10, height = 7)
    return(select)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
