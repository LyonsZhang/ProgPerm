\name{bin_progresscoverage}
\alias{bin_progresscoverage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{bin_progresscoverage}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bin_progresscoverage(alloutputs, lowindx, top_pm, lgndcol = 2, pvtitle, savepvfile, pvpicdim, estitle, saveesfile, espicdim)
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
  \item{lgndcol}{
%%     ~~Describe \code{lgndcol} here~~
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
  \item{estitle}{
%%     ~~Describe \code{estitle} here~~
}
  \item{saveesfile}{
%%     ~~Describe \code{saveesfile} here~~
}
  \item{espicdim}{
%%     ~~Describe \code{espicdim} here~~
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
function (alloutputs, lowindx, top_pm, lgndcol = 2, pvtitle, 
    savepvfile, pvpicdim, estitle, saveesfile, espicdim) 
{
    library(ggplot2)
    results <- alloutputs$alldist
    selname0 <- alloutputs$selname
    if (top_pm > length(selname0)) {
        stop("top_pm should be smaller than the number of observations.")
    }
    df <- as.data.frame(results$psigdist)
    N0 <- max(as.numeric(as.character(df$dif)))
    trueorg <- df[df$dif == 0, "median"]
    nsig_prop <- sum(df$q1[2:lowindx] <= trueorg & trueorg <= 
        df$q2[2:lowindx])/lowindx
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
    trueorg <- as.numeric(as.character(df[df$dif == 0, "median"]))
    pv_cover <- matrix(0, N - 1, top_pm)
    colnames(pv_cover) <- selname
    for (i in 1:(N - 1)) {
        pv_cover[i, ] <- as.numeric(as.character(df[df$dif == 
            i, "q1"])) <= trueorg & trueorg <= as.numeric(as.character(df[df$dif == 
            i, "q2"]))
    }
    pv_prop <- colSums(pv_cover)/N0
    df <- data.frame(pv_prop)
    df$variable <- selname
    df <- df[order(df$pv_prop), ]
    df$variable <- factor(df$variable, levels = df$variable)
    ggplot(data = df, aes(x = variable, y = 1 - pv_prop, fill = variable)) + 
        geom_bar(stat = "identity") + ylim(0, 1) + ylab("Proportion of noncoverage") + 
        xlab(paste("Top", top_pm, "significant features with decreasing order", 
            sep = " ")) + guides(fill = guide_legend(ncol = lgndcol)) + 
        ggtitle(pvtitle) + theme_bw() + theme(plot.title = element_text(size = 24, 
        face = "bold"), legend.title = element_text(size = 20), 
        legend.text = element_text(size = 12)) + theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), panel.background = element_blank(), 
        axis.line.x = element_line(color = "black", size = 1), 
        axis.line.y = element_line(color = "black", size = 1), 
        axis.text = element_text(size = 16, face = "bold"), axis.title = element_text(size = 20, 
            face = "bold"))
    ggsave(filename = savepvfile, width = pvpicdim[1], height = pvpicdim[2])
    df <- as.data.frame(results$esdist)
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
    trueorg <- as.numeric(as.character(df[df$dif == 0, "median"]))
    es_cover <- matrix(0, N - 1, top_pm)
    colnames(es_cover) <- selname
    for (i in 1:(N - 1)) {
        es_cover[i, ] <- as.numeric(as.character(df[df$dif == 
            i, "q1"])) <= trueorg & trueorg <= as.numeric(as.character(df[df$dif == 
            i, "q2"]))
    }
    es_prop <- colSums(es_cover)/N0
    df <- data.frame(es_prop)
    df$variable <- selname
    df <- df[order(df$es_prop), ]
    df$variable <- factor(df$variable, levels = df$variable)
    ggplot(data = df, aes(x = variable, y = 1 - es_prop, fill = variable)) + 
        geom_bar(stat = "identity") + ylim(0, 1) + ylab("Proportion of noncoverage") + 
        xlab(paste("Top", top_pm, "significant features with decreasing order", 
            sep = " ")) + guides(fill = guide_legend(ncol = lgndcol)) + 
        theme_bw() + theme(plot.title = element_text(size = 24, 
        face = "bold"), legend.title = element_text(size = 20), 
        legend.text = element_text(size = 12)) + theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), panel.background = element_blank(), 
        axis.line.x = element_line(color = "black", size = 1), 
        axis.line.y = element_line(color = "black", size = 1), 
        axis.text = element_text(size = 16, face = "bold"), axis.title = element_text(size = 20, 
            face = "bold"))
    ggsave(filename = saveesfile, width = espicdim[1], height = espicdim[2])
    return(list(nsig_prop = nsig_prop, pv_prop = pv_prop, es_prop = es_prop))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
