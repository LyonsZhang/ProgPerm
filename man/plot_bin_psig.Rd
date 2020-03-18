\name{plot_bin_psig}
\alias{plot_bin_psig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plot_bin_psig}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot_bin_psig(alloutputs, psigtitle, psigyrange = NULL, savepsigfile, psigpicdim)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alloutputs}{
%%     ~~Describe \code{alloutputs} here~~
}
  \item{psigtitle}{
%%     ~~Describe \code{psigtitle} here~~
}
  \item{psigyrange}{
%%     ~~Describe \code{psigyrange} here~~
}
  \item{savepsigfile}{
%%     ~~Describe \code{savepsigfile} here~~
}
  \item{psigpicdim}{
%%     ~~Describe \code{psigpicdim} here~~
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
function (alloutputs, psigtitle, psigyrange = NULL, savepsigfile, 
    psigpicdim) 
{
    require(ggplot2)
    results <- alloutputs$alldist
    selname0 <- alloutputs$selname
    df <- as.data.frame(results$psigdist)
    df$highlight <- ifelse(df$dif == 0, "real data", "permutation")
    df$highlight <- factor(df$highlight, levels = c("real data", 
        "permutation"))
    mycolours <- c(permutation = "black", `real data` = "red")
    N0 <- max(df$dif)
    if (sum(is.na(df))) {
        stop("Missing values exist in psigdist")
    }
    else {
        df$dif <- df$dif/N0
        if (is.null(psigyrange)) {
            y_range <- range(floor(min(df$q1)), ceiling(max(df$q2)))
        }
        else {
            y_range <- psigyrange
        }
        p <- ggplot(df, aes(x = dif, y = median)) + geom_point(size = 5, 
            aes(colour = highlight)) + scale_color_manual("Scenarios", 
            values = mycolours) + geom_pointrange(aes(ymin = q1, 
            ymax = q2, colour = highlight), size = 0.5) + geom_line(size = 1) + 
            xlab("Proportion of exchanges") + ylab("Number of significant hits") + 
            ggtitle(psigtitle) + ylim(y_range) + theme_bw() + 
            theme(plot.title = element_text(size = 30, face = "bold"), 
                legend.title = element_text(size = 20), legend.text = element_text(size = 16)) + 
            theme(axis.line = element_line(colour = "black"), 
                panel.grid.major = element_blank(), panel.border = element_blank(), 
                panel.background = element_blank(), axis.line.x = element_line(color = "black", 
                  size = 1.5), axis.line.y = element_line(color = "black", 
                  size = 1.5), axis.text = element_text(size = 16, 
                  face = "bold"), axis.title = element_text(size = 20, 
                  face = "bold"))
        ggsave(filename = savepsigfile, width = psigpicdim[1], 
            height = psigpicdim[2])
    }
    return(p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
