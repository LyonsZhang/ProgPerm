\name{plot_bin_permute_sigcurve}
\alias{plot_bin_permute_sigcurve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot_bin_permute_sigcurve
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot_bin_permute_sigcurve(alloutputs, samsize, psigtitle, savepsigfile, psigpicdim)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alloutputs}{
%%     ~~Describe \code{alloutputs} here~~
}
  \item{samsize}{
%%     ~~Describe \code{samsize} here~~
}
  \item{psigtitle}{
%%     ~~Describe \code{psigtitle} here~~
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
function (alloutputs, samsize, psigtitle, savepsigfile, psigpicdim) 
{
    require(ggplot2)
    results <- alloutputs$alldist
    selname0 <- alloutputs$selname
    df <- as.data.frame(results$psigdist)
    df$highlight <- ifelse(df$dif == 0, "real data", "permutation")
    df$highlight <- factor(df$highlight, levels = c("real data", 
        "permutation"))
    df$prop <- df$median/samsize
    len <- length(df$prop)
    mycolours <- c(permutation = "black", `real data` = "red")
    N0 <- max(df$dif)
    if (sum(is.na(df))) {
        stop("Missing values exist in psigdist")
    }
    else {
        df$dif <- df$dif/N0
        auc <- DescTools::AUC(df$dif, df$prop, method = "trapezoid", 
            na.rm = FALSE)
        aoi <- mean(df$prop[c(1, length(df$prop))])
        slope <- diff(df$prop)/diff(df$dif)
        p <- ggplot(df, aes(x = dif, y = prop)) + geom_point(size = 5, 
            aes(colour = highlight)) + scale_color_manual("Scenarios", 
            values = mycolours) + geom_pointrange(aes(ymin = q1/samsize, 
            ymax = q2/samsize, colour = highlight), size = 0.5) + 
            geom_line(size = 1) + geom_ribbon(aes(ymin = 0, ymax = prop), 
            alpha = 0.2, fill = "purple") + geom_ribbon(aes(ymin = prop, 
            ymax = seq(prop[1], prop[len], length.out = len)), 
            alpha = 0.2, fill = "green") + geom_ribbon(aes(ymin = seq(prop[1], 
            prop[len], length.out = len), ymax = 1), alpha = 0.2, 
            fill = "gold") + xlab("Proportion of exchanges") + 
            ylab("Proportion of significant features") + ggtitle(psigtitle) + 
            ylim(c(0, 1)) + annotate("text", x = 0.5, y = 0.9, 
            label = paste("aoi=", round(aoi, 3)), size = 6, colour = "green") + 
            annotate("text", x = 0.5, y = 0.8, label = paste("auc=", 
                round(auc, 3)), size = 6, colour = "purple") + 
            annotate("text", x = 0.5, y = 0.7, label = paste("slope=", 
                round(slope[1], 3)), size = 6, colour = "red") + 
            theme_bw() + theme(plot.title = element_text(size = 30, 
            face = "bold"), legend.title = element_text(size = 20), 
            legend.text = element_text(size = 16)) + theme(axis.line = element_line(colour = "black"), 
            panel.grid.major = element_blank(), panel.border = element_blank(), 
            panel.background = element_blank(), axis.line.x = element_line(color = "black", 
                size = 1.5), axis.line.y = element_line(color = "black", 
                size = 1.5), axis.text = element_text(size = 16, 
                face = "bold"), axis.title = element_text(size = 20, 
                face = "bold"))
        ggsave(filename = savepsigfile, plot = p, width = psigpicdim[1], 
            height = psigpicdim[2], device = cairo_ps)
        return(p)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
