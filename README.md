# demoSML

This demoSML (demonstration of supervised machine learning) R package will contain the supplementary material of a manuscript that shall be published. The working title of the manuscript is: A demonstration of supervised machine learning for psychological researchers.

Download complete demoSML supplementary material within R like this:
```R
# Only required if you have not installed the devtools package yet.
install.packages("devtools")
# Install demoSML from GitHub
devtools::install_github(repo="https://github.com/mmiche/demoSML",
                      dependencies = "Imports", build_vignettes = TRUE)
# Load the demoSML package
library(demoSML)
# Open the package documentation, notice the link 'User guides, package vignettes ...'.
help(package="demoSML")
```

Current (updated) status of the manuscript:

Published (where, when) - No.

Under peer review. Yes (since 2022-11-16).

Submitted to a peer reviewed journal. Yes (since 2022-09-28).

Awaiting co-author(s) feedback. All agreed.
