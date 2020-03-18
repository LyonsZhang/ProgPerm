\name{plot_con_permute_best}
\alias{plot_con_permute_best}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plot_con_permute_best}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot_con_permute_best(bestoutputs, pvtitle, savepvfile, pvpicdim, estitle, saveesfile, espicdim, crtitle, savecrfile, crpicdim)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bestoutputs}{
%%     ~~Describe \code{bestoutputs} here~~
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
  \item{crtitle}{
%%     ~~Describe \code{crtitle} here~~
}
  \item{savecrfile}{
%%     ~~Describe \code{savecrfile} here~~
}
  \item{crpicdim}{
%%     ~~Describe \code{crpicdim} here~~
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
function (bestoutputs, pvtitle, savepvfile, pvpicdim, estitle, 
    saveesfile, espicdim, crtitle, savecrfile, crpicdim) 
{
    worstpermute <- bestoutputs$worst_case
    top_pm <- length(bestoutputs$selname)
    df1 <- data.frame(matrix(0, top_pm, 4))
    rownames(df1) <- bestoutputs$selname
    colnames(df1) <- c("trueorg", "median", "q1", "q2")
    df1$variable <- factor(rownames(df1), levels = rownames(df1))
    df1$trueorg <- bestoutputs$pvalue0
    df1$median <- as.numeric(as.character(worstpermute$pvdist[, 
        "median"]))
    df1$q1 <- as.numeric(as.character(worstpermute$pvdist[, "q1"]))
    df1$q2 <- as.numeric(as.character(worstpermute$pvdist[, "q2"]))
    df1$coverage <- ifelse((df1$q1 <= df1$trueorg & df1$trueorg <= 
        df1$q2), "inside", "outside")
    ggplot(data = df1, aes(x = variable)) + geom_errorbar(aes(ymax = q2, 
        ymin = q1)) + geom_point(aes(y = median), shape = 1, 
        size = 3) + geom_point(aes(y = trueorg, colour = coverage), 
        shape = 16, size = 3) + ylab("-log10(p-value)") + xlab(paste("Top", 
        top_pm, "significant features with decreasing order", 
        sep = " ")) + coord_flip() + ggtitle(pvtitle) + theme_bw() + 
        theme(plot.title = element_text(size = 30, face = "bold"), 
            legend.title = element_text(size = 20), legend.text = element_text(size = 16)) + 
        theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), 
            panel.background = element_blank(), axis.line.x = element_line(color = "black", 
                size = 1), axis.line.y = element_line(color = "black", 
                size = 1), axis.text = element_text(size = 16, 
                face = "bold"), axis.title = element_text(size = 20, 
                face = "bold"))
    ggsave(filename = savepvfile, width = pvpicdim[1], height = pvpicdim[2])
    df2 <- data.frame(matrix(0, top_pm, 4))
    rownames(df2) <- bestoutputs$selname
    colnames(df2) <- c("trueorg", "median", "q1", "q2")
    df2$variable <- factor(rownames(df2), levels = rownames(df2))
    df2$trueorg <- bestoutputs$effectsize0
    df2$median <- as.numeric(as.character(worstpermute$esdist[, 
        "median"]))
    df2$q1 <- as.numeric(as.character(worstpermute$esdist[, "q1"]))
    df2$q2 <- as.numeric(as.character(worstpermute$esdist[, "q2"]))
    df2$coverage <- ifelse((df2$q1 <= df2$trueorg & df2$trueorg <= 
        df2$q2), "inside", "outside")
    ggplot(data = df2, aes(x = variable)) + geom_errorbar(aes(ymax = q2, 
        ymin = q1)) + geom_point(aes(y = median), shape = 1, 
        size = 3) + geom_point(aes(y = trueorg, colour = coverage), 
        shape = 16, size = 3) + ylab("Effect size") + xlab(paste("Top", 
        top_pm, "significant features with decreasing order", 
        sep = " ")) + coord_flip() + ggtitle(estitle) + theme_bw() + 
        theme(plot.title = element_text(size = 30, face = "bold"), 
            legend.title = element_text(size = 20), legend.text = element_text(size = 16)) + 
        theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), 
            panel.background = element_blank(), axis.line.x = element_line(color = "black", 
                size = 1), axis.line.y = element_line(color = "black", 
                size = 1), axis.text = element_text(size = 16, 
                face = "bold"), axis.title = element_text(size = 20, 
                face = "bold"))
    ggsave(filename = saveesfile, width = espicdim[1], height = espicdim[2])
    df3 <- data.frame(matrix(0, top_pm, 4))
    rownames(df3) <- bestoutputs$selname
    colnames(df3) <- c("trueorg", "median", "q1", "q2")
    df3$variable <- factor(rownames(df3), levels = rownames(df3))
    df3$trueorg <- bestoutputs$correlation0
    df3$median <- as.numeric(as.character(worstpermute$crdist[, 
        "median"]))
    df3$q1 <- as.numeric(as.character(worstpermute$crdist[, "q1"]))
    df3$q2 <- as.numeric(as.character(worstpermute$crdist[, "q2"]))
    df3$coverage <- ifelse((df3$q1 <= df3$trueorg & df3$trueorg <= 
        df3$q2), "inside", "outside")
    ggplot(data = df3, aes(x = variable)) + geom_errorbar(aes(ymax = q2, 
        ymin = q1)) + geom_point(aes(y = median), shape = 1, 
        size = 3) + geom_point(aes(y = trueorg, colour = coverage), 
        shape = 16, size = 3) + ylab("Correlation") + xlab(paste("Top", 
        top_pm, "significant features with decreasing order", 
        sep = " ")) + coord_flip() + ggtitle(crtitle) + theme_bw() + 
        theme(plot.title = element_text(size = 30, face = "bold"), 
            legend.title = element_text(size = 20), legend.text = element_text(size = 16)) + 
        theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), 
            panel.background = element_blank(), axis.line.x = element_line(color = "black", 
                size = 1), axis.line.y = element_line(color = "black", 
                size = 1), axis.text = element_text(size = 16, 
                face = "bold"), axis.title = element_text(size = 20, 
                face = "bold"))
    ggsave(filename = savecrfile, width = crpicdim[1], height = crpicdim[2])
    return(list(pvtable = df1, estable = df2, crtable = df3))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
