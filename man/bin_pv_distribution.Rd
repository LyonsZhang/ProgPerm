\name{bin_pv_distribution}
\alias{bin_pv_distribution}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{bin_pv_distribution}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bin_pv_distribution(alloutputs, lowindx, folder, pvtitle, pvpicdim)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alloutputs}{
%%     ~~Describe \code{alloutputs} here~~
}
  \item{lowindx}{
%%     ~~Describe \code{lowindx} here~~
}
  \item{folder}{
%%     ~~Describe \code{folder} here~~
}
  \item{pvtitle}{
%%     ~~Describe \code{pvtitle} here~~
}
  \item{pvpicdim}{
%%     ~~Describe \code{pvpicdim} here~~
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
function (alloutputs, lowindx, folder, pvtitle, pvpicdim) 
{
    dir.create(path = folder, showWarnings = FALSE)
    df <- alloutputs$alldist$pvdist
    df <- data.frame(df)
    df$pvalue <- 10^(-as.numeric(as.character((df[, "rep"]))))
    library(ggplot2)
    for (i in 0:lowindx) {
        p <- ggplot(df[df$dif == i, ], aes(x = pvalue)) + xlim(0, 
            1) + geom_histogram(aes(fill = ..count..), color = "black", 
            bins = 21, position = "dodge", boundary = 0)
        if (i == 0) {
            temp <- ggplot_build(p)$data[1]
        }
        p + geom_vline(aes(xintercept = 0.05), color = "#ff00cc", 
            linetype = "dashed", size = 2, show.legend = F) + 
            ylim(c(0, 1.1 * (max(temp[[1]]$y)))) + ggtitle(paste(pvtitle, 
            "permutation proportion", round(i/lowindx, 2), sep = " ")) + 
            theme_bw() + theme(plot.title = element_text(hjust = 0.5, 
            size = 30), legend.title = element_text(size = 20), 
            legend.text = element_text(size = 12)) + theme(panel.background = element_blank(), 
            axis.text = element_text(size = 24, face = "bold"), 
            axis.title = element_text(size = 20, face = "bold"), 
            panel.border = element_rect(colour = "black", fill = NA, 
                size = 0.5))
        ggsave(filename = paste(folder, "/", round(i/lowindx, 
            2), "_permute", "_pvalue.eps", sep = ""), width = 7, 
            height = 7)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
