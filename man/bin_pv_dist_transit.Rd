\name{bin_pv_dist_transit}
\alias{bin_pv_dist_transit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{bin_pv_dist_transit}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bin_pv_dist_transit(alloutputs, lowindx, cutoff, folder, pvtitle, pvpicdim)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alloutputs}{
%%     ~~Describe \code{alloutputs} here~~
}
  \item{lowindx}{
%%     ~~Describe \code{lowindx} here~~
}
  \item{cutoff}{
%%     ~~Describe \code{cutoff} here~~
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
function (alloutputs, lowindx, cutoff, folder, pvtitle, pvpicdim) 
{
    library(ggplot2)
    dir.create(path = folder, showWarnings = FALSE)
    df <- alloutputs$alldist$pvdist
    df <- data.frame(df)
    df$pvalue <- 10^(-as.numeric(as.character((df[, "rep"]))))
    scenario0 <- df[df$dif == 0, ]
    scenario0$Status <- "stay"
    scenario0[which(scenario0$pvalue > cutoff), "Status"] <- paste("leave", 
        cutoff, sep = "")
    scenario0 <- na.omit(scenario0)
    if (length(unique(scenario0$Status)) == 1) {
        cl0 <- "#00BFC4"
    }
    else {
        cl0 <- c("#F8766D", "#00BFC4")
    }
    p <- ggplot(scenario0, aes(x = pvalue, fill = Status)) + 
        xlim(0, 1) + geom_histogram(color = "black", fill = "#00BFC4", 
        bins = 21, position = "stack", boundary = 0) + scale_fill_manual(values = cl0)
    sc <- ggplot_build(p)$data[1]
    p <- p + geom_vline(aes(xintercept = cutoff), color = "#ff00cc", 
        linetype = "dashed", size = 2, show.legend = F) + geom_text(aes(x = cutoff, 
        label = paste("p=", cutoff, eps = ""), y = -0.5), colour = "blue", 
        vjust = 1.2, size = 5) + ylim(c(-0.5, 1.1 * max(sc[[1]]$y)))
    if (!is.null(pvtitle)) {
        p <- p + ggtitle(paste(pvtitle, "permutation proportion", 
            round(0/lowindx, 2), sep = " "))
    }
    p + theme_bw() + theme(plot.title = element_text(hjust = 0.5, 
        size = 30), legend.title = element_text(size = 20), legend.text = element_text(size = 12)) + 
        theme(panel.background = element_blank(), axis.text = element_text(size = 24, 
            face = "bold"), axis.title = element_text(size = 20, 
            face = "bold"), panel.border = element_rect(colour = "black", 
            fill = NA, size = 0.5))
    ggsave(filename = paste(folder, "/", round(0/lowindx, 2), 
        "_permute", "_pvalue.eps", sep = ""), width = 7, height = 7)
    list0 <- (scenario0$pvalue < cutoff)
    len0 <- sum(list0)
    for (i in 1:lowindx) {
        scenario <- df[df$dif == i, ]
        temp <- scenario[1:len0, ]
        scenario$Status <- "stay"
        scenario[c(temp$pvalue > cutoff, rep(FALSE, dim(scenario)[1] - 
            len0)), "Status"] <- paste("leave", cutoff, sep = "")
        if (length(unique(scenario$Status)) == 2) {
            cl <- c("#F8766D", "#00BFC4")
        }
        else {
            if (unique(scenario$Status) == "stay") {
                cl <- "#00BFC4"
            }
            else {
                cl <- "#F8766D"
            }
        }
        p <- ggplot(scenario, aes(x = pvalue, fill = Status)) + 
            xlim(0, 1) + geom_histogram(color = "black", bins = 21, 
            position = "stack", boundary = 0) + scale_fill_manual(values = cl)
        p <- p + geom_vline(aes(xintercept = cutoff), color = "#ff00cc", 
            linetype = "dashed", size = 2, show.legend = F) + 
            geom_text(aes(x = cutoff, label = paste("p=", cutoff, 
                eps = ""), y = -0.5), colour = "blue", vjust = 1.2, 
                size = 5) + ylim(c(-0.5, 1.1 * max(sc[[1]]$y)))
        if (!is.null(pvtitle)) {
            p <- p + ggtitle(paste(pvtitle, "permutation proportion", 
                round(i/lowindx, 2), sep = " "))
        }
        p + theme_bw() + theme(plot.title = element_text(hjust = 0.5, 
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
