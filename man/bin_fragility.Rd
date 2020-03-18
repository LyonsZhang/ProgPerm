\name{bin_fragility}
\alias{bin_fragility}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{bin_fragility}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bin_fragility(alloutputs, lowindx, top_pm, alpha = 0.05, legend = FALSE, lgndcol = 2, yrange = NULL, pvtitle = NULL, savepvfile, pvpicdim)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alloutputs}{
%%     ~~Describe \code{alloutputs} here~~
}
  \item{lowindx}{
%%     ~~Describe \code{lowindx} here~~
}
  \item{top_pm}{
%%     ~~Describe \code{top_pm} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
}
  \item{lgndcol}{
%%     ~~Describe \code{lgndcol} here~~
}
  \item{yrange}{
%%     ~~Describe \code{yrange} here~~
}
  \item{pvtitle}{
%%     ~~Describe \code{pvtitle} here~~
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
function (alloutputs, lowindx, top_pm, alpha = 0.05, legend = FALSE, 
    lgndcol = 2, yrange = NULL, pvtitle = NULL, savepvfile, pvpicdim) 
{
    results <- alloutputs$alldist
    selname0 <- alloutputs$selname
    if (top_pm > length(selname0)) {
        stop("top_pm should be smaller than the number of observations.")
    }
    df <- as.data.frame(results$pvdist)
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
    temp <- matrix(as.numeric(as.character(df$rep)), N, top_pm, 
        byrow = T)
    dat <- temp[1:lowindx, ]
    df <- data.frame(fragility = integer(top_pm), variable = character(top_pm), 
        stringsAsFactors = FALSE)
    df$fragility <- colSums(dat > -log10(alpha))
    df$variable <- selname
    df <- df[order(-df$fragility), ]
    df$variable <- factor(selname, levels = selname)
    df$group <- factor(df$fragility, levels = sort(unique(df$fragility), 
        decreasing = T))
    p <- ggplot(data = df, aes(x = variable, y = fragility, fill = group)) + 
        geom_bar(stat = "identity")
    if (is.null(yrange)) {
        y_range <- range(0, ceiling(max(df$fragility)))
    }
    else {
        y_range <- yrange
    }
    p <- ggplot(data = df, aes(x = variable, y = fragility, fill = variable)) + 
        geom_bar(stat = "identity") + scale_fill_manual(aes(labels = variable), 
        values = ggplot_build(p)$data[[1]]$fill, drop = FALSE) + 
        ylab("Fragility Index") + xlab(paste("Top", top_pm, "significant hits with decreasing order", 
        sep = " ")) + ggtitle(pvtitle) + ylim(y_range) + theme_bw() + 
        theme(plot.title = element_text(size = 24, face = "bold")) + 
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
            axis.line = element_line(colour = "black"), panel.border = element_blank(), 
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
            theme(legend.title = element_text(size = 20), legend.text = element_text(size = 12))
    }
    ggsave(filename = savepvfile, plot = p, width = pvpicdim[1], 
        height = pvpicdim[2])
    return(list(p = p, df = df))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
