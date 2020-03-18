\name{dotplot_bin_sig}
\alias{dotplot_bin_sig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{dotplot_bin_sig}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dotplot_bin_sig(select, variable, testdata, folder, method = "kruskal.test", s = 15)
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
  \item{s}{
%%     ~~Describe \code{s} here~~
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
function (select, variable, testdata, folder, method = "kruskal.test", 
    s = 15) 
{
    library(ggplot2)
    library(see)
    dir.create(path = folder, showWarnings = FALSE)
    binwidth = (max(testdata[[select]], na.rm = T)/2 - mean(testdata[[select]], 
        na.rm = T))/s
    stackratio = 1
    dotsize = 1.2
    p <- ggplot(testdata, aes_string(x = variable, y = select, 
        fill = variable, group = variable)) + geom_dotplot(binaxis = "y", 
        stackdir = "center", position = "identity", method = "dotdensity", 
        binwidth = binwidth, stackratio = stackratio, dotsize = dotsize) + 
        scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + 
        scale_fill_manual(values = c("#00b0f6", "#f8766d")) + 
        stat_summary(fun.ymin = function(z) {
            quantile(z, 0.025)
        }, fun.ymax = function(z) {
            quantile(z, 0.975)
        }, fun.y = median, shape = 18, size = 2, geom = "pointrange", 
            color = "black", show.legend = FALSE) + ggpubr::stat_compare_means(method = method, 
        size = 10) + ylim(-0.1 * max(testdata[[select]]), 1.1 * 
        max(testdata[[select]])) + theme_bw() + theme(plot.title = element_text(size = 24, 
        face = "bold"), legend.title = element_text(size = 20), 
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
