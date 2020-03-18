\name{plot_bin_pv}
\alias{plot_bin_pv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plot_bin_pv}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot_bin_pv(alloutputs, top_pm, lgndcol = 2, legend = FALSE, pvtitle, pvyrange = NULL, savepvfile, pvpicdim)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alloutputs}{
%%     ~~Describe \code{alloutputs} here~~
}
  \item{top_pm}{
%%     ~~Describe \code{top_pm} here~~
}
  \item{lgndcol}{
%%     ~~Describe \code{lgndcol} here~~
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
}
  \item{pvtitle}{
%%     ~~Describe \code{pvtitle} here~~
}
  \item{pvyrange}{
%%     ~~Describe \code{pvyrange} here~~
}
  \item{savepvfile}{
%%     ~~Describe \code{savepvfile} here~~
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
function (alloutputs, top_pm, lgndcol = 2, legend = FALSE, pvtitle, 
    pvyrange = NULL, savepvfile, pvpicdim) 
{
    require(ggplot2)
    results <- alloutputs$alldist
    selname0 <- alloutputs$selname
    if (top_pm > length(selname0)) {
        stop("top_pm should be smaller than the number of observations.")
    }
    df <- as.data.frame(results$pvdist)
    N0 <- max(as.numeric(as.character(df$dif)))
    N <- length(unique(df$dif))
    df$dif <- factor(df$dif, levels = as.character(0:(N - 1)))
    selname <- selname0[1:top_pm]
    if (top_pm < length(selname0)) {
        tt <- split(df, df$dif)
        ss <- lapply(tt, function(x) {
            return(x[1:top_pm, ])
        })
        df <- do.call("rbind", ss)
    }
    if (sum(is.na(df))) {
        stop("Missing values exist in pvdist")
    }
    else {
        df$dif <- as.numeric(as.character(df[, "dif"]))/N0
        df$median <- as.numeric(as.character(df[, "median"]))
        df$q1 <- as.numeric(as.character(df[, "q1"]))
        df$q2 <- as.numeric(as.character(df[, "q2"]))
        df$variable <- factor(df$variable, levels = selname)
        if (is.null(pvyrange)) {
            y_range <- range(floor(min(df$q1)), ceiling(max(df$q2)))
        }
        else {
            y_range <- pvyrange
        }
        p <- ggplot(data = df, aes(x = dif, y = median, colour = variable, 
            group = variable)) + geom_line() + geom_point() + 
            geom_pointrange(aes(ymin = q1, ymax = q2)) + ylim(y_range) + 
            xlab("Proportion of exchanges") + ylab(paste("Log P-values of top", 
            paste(top_pm, "important hits", sep = " "), sep = " ")) + 
            ggtitle(pvtitle) + theme_bw() + theme(plot.title = element_text(size = 30, 
            face = "bold"), axis.line = element_line(colour = "black"), 
            panel.grid.major = element_blank(), panel.border = element_blank(), 
            panel.background = element_blank(), axis.line.x = element_line(color = "black", 
                size = 1), axis.line.y = element_line(color = "black", 
                size = 1), axis.text = element_text(size = 16, 
                face = "bold"), axis.title = element_text(size = 20, 
                face = "bold"))
        if (isFALSE(legend)) {
            p <- p + theme(legend.position = "none")
        }
        else {
            p <- p + guides(col = guide_legend(ncol = lgndcol)) + 
                theme(legend.title = element_text(size = 14), 
                  legend.text = element_text(size = 10))
        }
        ggsave(filename = savepvfile, plot = p, width = pvpicdim[1], 
            height = pvpicdim[2])
    }
    return(p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
