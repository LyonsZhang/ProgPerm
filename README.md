# ProgPermute
> Progressive permutation for a dynamic representation of the robustness of microbiome discoveries

The proposed method progressively permutes the grouping factor labels of microbiome and performs multiple differential abundance tests in each scenario. We compare the signal strength of top hits from the original data with their performance in permutations, and will observe an apparent decreasing trend if these top hits are true positives identified from the data. To help understand the robustness of the discoveries and identify best hits, we develop a user-friendly and efficient RShiny tool. Simulations and applications on real data show that the proposed method can evaluate the overall association between microbiome and the grouping factor, rank the robustness of the discovered microbes, and list the discoveries, their effect sizes, and individual abundances.

---

# Table of Contents
<!--ts-->
- [Installation](#installation)
- [Example](#example)
- [RShiny App](#rshiny_app)
- [Contact](#contact)
<!--te-->

# Installation

1. If you don't have "devtools" package, you need to download it by using 
```R
install.packages("devtools")
```

2. Run the following codes:
```R
library(devtools)
install_github("LyonsZhang/ProgPermute")
library(ProgPermute)
```
3. Run the example codes in "Execute_ProgPermute.R" in the test folder https://github.com/LyonsZhang/ProgPermute/tree/master/test.

# Example

### Preparation
>clear current session
```R
closeAllConnections()
rm(list=ls())
```
>load data
```R
data(testdata1)
```
>set the working path where the figures will be saved
```R
setwd("/Users/lzhang27/Documents/ProgPermute/R")
```
### Analyze overall association
>run progressive permutation
```R
results<-bin_permute_all(variable="variable",testdata=testdata1,top_pm=267,zoomn=15,alpha=0.05)
```
>show U-Curve of number of significant features
```R
sigloc<-plot_bin_psig(alloutputs=results,psigtitle=NULL,psigyrange=c(0,170),savepsigfile ="sigfeatures.eps", psigpicdim=c(10,7))
```
>show the traces of Pvalues
```R
pvloc<-plot_bin_pv(alloutputs=results,top_pm=267,pvtitle="",pvyrange=c(0,7),savepvfile ="pvfeatures.eps", pvpicdim=c(10,7))
```
>show the scaled U-Curve
```R
sigsum<-plot_bin_permute_sigcurve(alloutputs=results,samsize=267,lgndcol=2,psigtitle=NULL,savepsigfile="bin_sigcurve.eps",psigpicdim=c(10,7))
```
>Calculate Fragility Index
```R
intres<-bin_true_initial(variable="variable",testdata=testdata1,top_pm=267)
fragility<-bin_fragility(alloutputs=results,lowindx=intres$n,top_pm=50,lgndcol=2,yrange=c(0,7),pvtitle=NULL,savepvfile="locationPvfragility.eps",pvpicdim=c(15,7))
```
>Calculate the progressive coverage
```R
intres<-bin_true_initial(variable="variable",testdata=testdata1,top_pm=267)
coverage<-bin_progresscoverage(alloutputs=results,lowindx=intres$n,top_pm=50,lgndcol=2,pvtitle=NULL,savepvfile="Pvcoverage.eps",pvpicdim=c(15,7),estitle=NULL,saveesfile="effectcoverage.eps",espicdim=c(15,7))
```
>Observe the distributions of Pvalues
```R
bin_pv_distribution(alloutputs=results,lowindx=intres$n,folder="dist1",pvtitle=NULL,pvpicdim=c(7,7))
bin_pv_dist_transit(alloutputs=results,lowindx=intres$n,cutoff=0.05,folder="results1",pvtitle="",pvpicdim=c(7,7))
```
### Identify the best features
>run the full permutation
```R
best<-bin_permute_best(variable="variable",testdata=testdata1,top_pm=50,zoomn=100,alpha=0.05)
```
>Real hits lie outside of 95% quantile intervals of full permuted hits
```R
cinfresults<-plot_bin_permute_best(bestoutputs=best,top_pm=50,pvtitle="Coverage plot",savepvfile="pvalue_Coverageplot.eps",pvpicdim=c(15,10),estitle="Coverage plot",saveesfile="effectsize_Coverageplot.eps",espicdim=c(15,10))
```
>List the signed effect sizes of the discoveries
```R
plot_bin_effectsize(bestoutputs=best,variable="variable",testdata=testdata1,estitle=NULL,saveesfile="signedeffectsize_plot.eps",espicdim=c(15,10))
```
>Show the dot plot of each individual discovery
```R
lapply(best$goodpvname,dotplot_bin_sig,variable="variable",testdata=testdata1,folder="individual1")
```
# RShiny App
You can access the App on https://biostatistics.mdanderson.org/shinyapps/ProgPerm
# Contact
If you have any questions, please contact me at liangliangzhang.stat@gmail.com
