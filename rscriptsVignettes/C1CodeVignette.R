# Source of this R script:
# https://github.com/mmiche/demoSML
# Code that belongs to demoSML package vignette:
# 'C1: R code Kuhn et al. (2021) data set'
# ------------------------------------------------------------

library(demoSML)
library(mlr3verse)
#
d <- supplc

names(d)

# Do not include BADE_total in the model (ignore index 11).
(analysis <- colnames(d)[c(1:10,12:13)])

# Linear regression model
LINREG <- lrn("regr.lm")
# z-transform predictors: po("scale", center=TRUE), add this to LINREG
linreg.z <- po("scale", center = TRUE) %>>% po("learner", LINREG)

# Lasso regression model
LASSO <- lrn("regr.glmnet", alpha = 1, s=1.1)
# z-transform predictors: po("scale", center=TRUE), add this to LASSO
lasso.z <- po("scale", center = TRUE) %>>% po("learner", LASSO)



# Recompute with mlr3 (expect exact same result)
# -------------------

# Reg regression, d name of data set, backend: the data, target: name of outcome variable
(tskReg <- TaskRegr$new(id="dReg", backend=d[,analysis], target="covid_general_total"))
# Output


# Same 10 seeds as in C0Code
seeds <- c(24388, 59521, 43307, 69586, 11571,
           25173, 32618, 13903, 8229, 25305)
trainLs <- testLs <- list()
for(i in 1:length(seeds)) {
    set.seed(seeds[i])
    trainLs[[i]] <- sample(nrow(d), .8 * nrow(d))
    testLs[[i]] <- setdiff(seq_len(nrow(d)), trainLs[[i]])
}

# Select custom resampling
resamplingCustom <- rsmp("custom")
# Instantiate custom resampling with a list for training and for testing.
resamplingCustom$instantiate(tskReg, train_sets = trainLs, test_sets = testLs)

sapply(trainLs, FUN = length) # 1277 training subsets (80%)
1277/1597
sapply(testLs, FUN = length) # 320 training subsets (20%)
320/1597

# Set up the competition design.
design <- benchmark_grid(tasks = tskReg,
                         learners=list(linreg.z,
                                       lasso.z),
                         resamplings = resamplingCustom)
# Start the competition. bmr = benchmark result
bmr <- benchmark(design, store_models = TRUE)

as.data.table(bmr$score()[,c("learner_id", "regr.mse")])

bmr$aggregate()

# ---------------------------------------
# Result from C0 vignette
#
#     trainLM   testLM  trainLO   testLO
# 1  390.9892 416.6534 398.5033 416.0374
# 2  380.9190 456.7386 388.4370 458.3217
# 3  395.7678 394.8990 403.1938 392.7143
# 4  397.6245 389.0075 404.6908 397.7975
# 5  391.6454 414.1738 399.3375 414.7999
# 6  398.9630 383.3298 406.3591 386.4126
# 7  395.7425 399.1159 403.2739 402.4591
# 8  388.2609 426.4046 395.1203 426.0281
# 9  382.9265 447.0375 390.4505 456.7870
# 10 381.7515 451.1563 389.2311 455.2052
# # Overall mean performance across all 10 different runs.
# colMeans(lm.lasso.Df1)
# trainLM   testLM  trainLO   testLO 
# 390.4590 417.8517 397.8597 420.6563
# ---------------------------------------

# Detailed prediction performances for the test subsets, compare with columns testLM and testLO, respectively.
as.data.table(bmr$score()[,c("learner_id", "regr.mse")])
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -