# ProgPermute
> Progressive permutation for a dynamic representation of the robustness of microbiome discoveries
The proposed method progressively permutes the grouping factor labels of microbiome and performs multiple differential abundance tests in each scenario. We compare the signal strength of top hits from the original data with their performance in permutations, and will observe an apparent decreasing trend if these top hits are true positives identified from the data. To help understand the robustness of the discoveries and identify best hits, we develop a user-friendly and efficient RShiny tool. Simulations and applications on real data show that the proposed method can evaluate the overall association between microbiome and the grouping factor, rank the robustness of the discovered microbes, and list the discoveries, their effect sizes, and individual abundances.

---

## Table of Contents (Optional)

> If your `README` has a lot of info, section headers might be nice.

- [Installation](#installation)
- [Features](#features)
- [Contributing](#contributing)
- [Team](#team)
- [FAQ](#faq)
- [Support](#support)
- [License](#license)


### How to install the Package in R/RStudio?

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
3. Run the example codes in "Exucute_ProgPermute.R" in the test folder.

#
If you have any questions, please contact me at liangliangzhang.stat@gmail.com
