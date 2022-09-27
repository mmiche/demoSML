# Source of this R script:
# https://github.com/mmiche/demoSML
# Code that belongs to demoSML package vignette:
# 'C2: R code Kuhn et al. (2021) data set'
# ------------------------------------------------------------

library(demoSML)
library(mlr3verse)
library(ggplot2)
#
d <- supplc
names(d)

# Do not include BADE_total in the model (ignore index 11).
(analysis <- colnames(d)[c(1:10,12:13)])

# # lrnTbl: table with all learners (algorithms) available in mlr3.
# lrnTbl <- as.data.table(mlr_learners)
# # All regression algorithms available in mlr3
# lrnTbl[grepl("regr", lrnTbl$key),c("key", "packages")]
# # key                      packages
# # 1:   regr.cv_glmnet      mlr3,mlr3learners,glmnet
# # 2:       regr.debug                          mlr3
# # 3: regr.featureless                    mlr3,stats
# # 4:      regr.glmnet      mlr3,mlr3learners,glmnet
# # 5:        regr.kknn        mlr3,mlr3learners,kknn
# # 6:          regr.km mlr3,mlr3learners,DiceKriging
# # 7:          regr.lm       mlr3,mlr3learners,stats
# # 8:      regr.ranger      mlr3,mlr3learners,ranger
# # 9:       regr.rpart                    mlr3,rpart
# # 10:         regr.svm       mlr3,mlr3learners,e1071
# # 11:     regr.xgboost     mlr3,mlr3learners,xgboost

# Set the prediction task to be regression (TaskRegr)
tskReg <- TaskRegr$new(id="dReg", backend=d[,analysis], target="covid_general_total")

# Set inner resampling to cross-validation with 4 folds.
inner_rsmp <- rsmp("cv", folds = 4)

# Set the number of different tuning parameters, after which to stop.
terminator <- trm("evals", n_evals = 7)

# Set the automatic tuning to be random.
tuner <- tnr("random_search")

# Select mean squared error as the prediction performance measure.
measure = msr("regr.mse")

# -----
# Select the linear regression regression model
linreg <- lrn("regr.lm", predict_sets=c("train", "test"))
# Command to z-transform predictors prior to model estimation.
linreg.z <- po("scale", center = TRUE) %>>% po("learner", linreg)
# -----

# -----
# Select the lasso regression model (glmnet, set alpha to 1).
lasso <- lrn("regr.glmnet", alpha = 1, predict_sets=c("train", "test"))
# Set the search space for the lasso tuning parameter lambda.
# Set the search space for the lasso tuning parameter lambda (s = selected lambda value).
# s means "selected lambda" from the many internal models that are run.
search_spaceLO <- ps(s = p_dbl(lower = 0.001, upper = 0.5))
# Set the auto tuner, by providing all elements that are required.
# at = auto tuning, lo = lasso
at.lo <- AutoTuner$new(lasso, inner_rsmp, measure, terminator, tuner,
                       search_spaceLO, store_models = TRUE)
# atz = auto tuning, using z-transformed predictors
at.lo.z <- po("scale", center = TRUE) %>>% po("learner", at.lo)
# -----
# Select the k-nearest neighbors model (knn).
knn <- lrn("regr.kknn", predict_sets=c("train", "test"))
# Set the search space for the knn tuning parameter k.
# k = number of data points in the neighborhood of any given data point, upon which the algorithm shall base its prediction.
searchSpaceKNN <- ps(k = p_int(lower = 1, upper = 20))
# Set the auto tuner, by providing all elements that are required.
# at = auto tuning, knn = k-nearest neighbors
at.knn <- AutoTuner$new(knn, inner_rsmp, measure, terminator, tuner,
                        searchSpaceKNN, store_models = TRUE)
# atz = auto tuning, using z-transformed predictors
at.knn.z <- po("scale", center = TRUE) %>>% po("learner", at.knn)
# -----
# Select the recursive partitioning (rpart) model.
rPart <- lrn("regr.rpart", predict_sets=c("train", "test"))
# Set the search space for the rpart tuning parameter cp.
# cp = factor by which the prediction performance shall be improved when adding another predictor.
searchSpaceRP <- ps(cp = p_dbl(lower = 0, upper = 0.04))
# Set the auto tuner, by providing all elements that are required.
# at = auto tuning, rp = rpart
at.rp <- AutoTuner$new(rPart, inner_rsmp, measure, terminator, tuner,
                       searchSpaceRP, store_models = TRUE)
# atz = auto tuning, using z-transformed predictors
at.rp.z <- po("scale", center = TRUE) %>>% po("learner", at.rp)
# -----
# Random forest (regr.ranger)
rf <- lrn("regr.ranger", predict_sets=c("train", "test"))
# Set the search space for the random forest parameter num.trees.
searchSpaceRF <- ps(num.trees = p_int(lower = 1, upper = 1000))
# at = auto tuning, rf = random forest
at.rf <- AutoTuner$new(rf, inner_rsmp, measure, terminator, tuner,
                           searchSpaceRF, store_models = TRUE)
# atz = auto tuning, using z-transformed predictors
at.rf.z <- po("scale", center = TRUE) %>>% po("learner", at.rf)
# -----
# Support vector machine (regr.svm)
svm <- lrn("regr.svm", kernel="radial", type="eps-regression", predict_sets=c("train", "test"))
# Set the search space for the svm parameter cost.
searchSpaceSVM = ps(cost = p_dbl(lower = 0, upper = 1))
# at = auto tuning, svm = support vector machine
at.svm <- AutoTuner$new(svm, inner_rsmp, measure, terminator, tuner,
                        searchSpaceSVM, store_models = TRUE)
# atz = auto tuning, using z-transformed predictors
at.svm.z <- po("scale", center = TRUE) %>>% po("learner", at.svm)
# -----

# -----
# Set the outer resampling to repeated cross-validation, with 2 repetitions and 3 folds.
outer_rsmp <- rsmp("repeated_cv", repeats = 2, folds = 3)
# Set up the competition design.
design <- benchmark_grid(tasks = tskReg,
                         learners=list(linreg.z,
                                       at.lo.z,
                                       at.rp.z,
                                       at.knn.z,
                                       at.rf.z,
                                       at.svm.z),
                         resamplings = outer_rsmp)
# set.seed ensures reproducibility of results.
set.seed(1)
# Start the competition (takes about 2 minutes). bmr = benchmark result
bmr <- benchmark(design, store_models = TRUE)

# Display the overall average outer prediction performance.
bmr$aggregate()[,c("learner_id", "regr.mse")]

# Collect the six validation performances of all algorithms
bmrPerf <-
as.data.frame(as.data.table(bmr$score()[,c("learner_id", "regr.mse")]))
# Make the names of the algorithms shorter by removing 'scale.regr'
bmrPerf$learner_id <- gsub("scale.regr.", "", bmrPerf$learner_id)

# Display boxplot
ggplot(data=bmrPerf, aes(x=learner_id, y=regr.mse)) +
    geom_boxplot() +
    theme_bw(base_size=16) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2.5)) +
    # Set y-axis so that both boxplot figures can be easily compared.
    scale_y_continuous(breaks = c(350, 400, 450, 500))
# --------------------------------------------------------
# Same set up, no tuning, compare performances with tuning.

# Select models
# linear regression
linreg <- lrn("regr.lm", predict_sets=c("train", "test"))
linreg.z <- po("scale", center = TRUE) %>>% po("learner", linreg)
# Lasso regression
lasso <- lrn("regr.glmnet", alpha = 1, predict_sets=c("train", "test"))
lo.z <- po("scale", center = TRUE) %>>% po("learner", lasso)
# k-nearest neighbor
knn <- lrn("regr.kknn", predict_sets=c("train", "test"))
knn.z <- po("scale", center = TRUE) %>>% po("learner", knn)
# Recursive partitioning
rPart <- lrn("regr.rpart", predict_sets=c("train", "test"))
rp.z <- po("scale", center = TRUE) %>>% po("learner", rPart)
# Random forest
rf <- lrn("regr.ranger", predict_sets=c("train", "test"))
rf.z <- po("scale", center = TRUE) %>>% po("learner", rf)
# Support vector machine
svm <- lrn("regr.svm", predict_sets=c("train", "test"))
svm.z <- po("scale", center = TRUE) %>>% po("learner", svm)

# Set the prediction task to be regression (TaskRegr)
tskReg <- TaskRegr$new(id="dReg", backend=d[,analysis], target="covid_general_total")
# Set inner resampling to cross-validation with 4 folds.
inner_rsmp <- rsmp("cv", folds = 4)
# Set the number of different tuning parameters, after which to stop.
terminator <- trm("evals", n_evals = 7)
# Set the automatic tuning to be random.
tuner <- tnr("random_search")
# Select mean squared error as the prediction performance measure.
measure = msr("regr.mse")
# Resampling for the final prediction performance
outer_rsmp <- rsmp("repeated_cv", repeats = 2, folds = 3)
# Set up the competition design.
design <- benchmark_grid(tasks = tskReg,
                         learners=list(linreg.z,
                                       lo.z,
                                       rp.z,
                                       knn.z,
                                       rf.z,
                                       svm.z),
                         resamplings = outer_rsmp)
# set.seed ensures reproducibility of results.
set.seed(1)
# Start the competition (takes about 10 seconds). bmr = benchmark result
bmr <- benchmark(design, store_models = TRUE)
# Warning by glmnet can be ignored. It says that the user has not selected a specific lambda value (hyperparameter value), therefore the value 0.001 has been selected automatically. lambda = 0.001 means 'almost' no tuning has been conducted, which can be seen in 'almost' the same performance of the glmnet.tuned model (see x-axis in the boxplot) and the lm model.

# Display the overall average outer prediction performance.
bmr$aggregate()[,c("learner_id", "regr.mse")]

# Collect the six validation performances of all algorithms
bmrPerf <-
    as.data.frame(as.data.table(bmr$score()[,c("learner_id", "regr.mse")]))
# Make the names of the algorithms shorter by removing 'scale.regr'
bmrPerf$learner_id <- gsub("scale.regr.", "", bmrPerf$learner_id)

# Display boxplot
ggplot(data=bmrPerf, aes(x=learner_id, y=regr.mse)) + geom_boxplot() +
    theme_bw(base_size=16) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2.5)) +
    # Set y-axis so that both boxplot figures can be easily compared.
    scale_y_continuous(breaks = c(350, 400, 450, 500))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -