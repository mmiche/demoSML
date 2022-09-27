# Source of this R script:
# https://github.com/mmiche/demoSML
# Code that belongs to demoSML package vignette:
# 'D: R code Wahl et al. (2022) data set'
# ------------------------------------------------------------
#
library(demoSML)
library(mlr3verse)
library(ggplot2)
#
# The sensor data were recorded with a smartwatch (at 50 Hertz):
# 1. Accelerometer with three axes x, y, z: acc_x, acc_y, acc_z
# 2. Gyroscope with three axes x, y, z: gyr_x, gyr_y, gyr_z
# 3. Magnetometer with three axes x, y, z: mag_x, mag_y, mag_z
# 4. Orientation (calculated from sensors 1-3) with four components ("quaternion"):
# q0_x, q1_y, q2_z, q3_z
# ---------------------
# For interested readers, regarding the computation of a "quaternion", see:
# https://nitinjsanket.github.io/tutorials/attitudeest/madgwick
# and/or
# Sensor Fusion on Android Devices: A Revolution in Motion Processing (youtube, 46:26 Min.)
# https://www.youtube.com/watch?v=C7JQ7Rpwn2k
# -----------------------------------------------------------

# Data set 'suppld' is automatically loaded, once the demoSML package has been loaded.
d <- suppld
dim(d)
#
# -o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o
# Step 1: Preprocessing
# ---------------------
#
# Skipped ... (not part of machine learning per se, but part of preparing for the application of sML; preprocessing details for the raw data are reported in these three sections of Wahl et al. (2022):
# 2.7. Synchronization and ground-truth annotation
# 2.8. Feature extraction and selection
# 2.9. Data balancing
#
#
# Display structure of d
str(d)
# Number of human subjects in the data set:
max(d$subject)
# Overview of class balance of the binary outcome 'washType'.
table(d$washType)
# Output in console
# compulsive    natural 
# 303        150

# Outcome base rate 
prop.table(table(d$washType))
# Output in console
# compulsive    natural 
# 0.6688742  0.3311258
#
# 'Mildly' unbalanced: There seem to be no clear criteria as to when outocme class 'imbalance' requires to be taken into account when applying sML. One paper (He and Garcia, 2009) indicates that class ratios of between 100:1 and 10'000:1 can be considered severely imbalanced data. Compared to that, the class ratio of 2:1 appears to be 'mild'.
# He, H., & Garcia, E. A. (2009). Learning from imbalanced data. IEEE Transactions on knowledge and data engineering, 21(9), 1263-1284.
#
# Start using functions provided by the mlr3verse package family:
# --------------------------------------------------------------
#
# -o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o
# Step 2: Model selection # See Wahl et al. (2022)
# https://mlr3learners.mlr-org.com/
# -----------------------
#
# Support vector machine (SVM)
# ----------------------------
# https://mlr3learners.mlr-org.com/reference/mlr_learners_classif.svm.html
SVM <- lrn("classif.svm", predict_type = "prob", predict_sets=c("train", "test"))
#
# SVMRB with radial basis function:
# --------------------------------
# https://mlr3gallery.mlr-org.com/posts/2021-03-09-practical-tuning-series-tune-a-support-vector-machine/
SVMRB <- lrn("classif.svm", type = "C-classification", kernel = "radial", predict_type = "prob", predict_sets=c("train", "test"))
# Rename the id so that the user can discern the difference between SVM and SVMRB later on (in step 5 of the ML procedure).
SVMRB$id <- "classif.svm_rb"
#
# Naive Bayes
# -----------
NAIVEBAYES <- lrn("classif.naive_bayes", predict_type = "prob", predict_sets=c("train", "test"))
#
# Random Forest
# -------------
RANDOMFOREST <- lrn("classif.ranger", predict_type = "prob", predict_sets=c("train", "test"))
#
# -o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o
# Step 3: Model tuning
# --------------------
#
# No explicit model tuning in Wahl et al. (2022), due to the exploratory research stage being the pilot stage. That is, tuning is refinement, which makes sense at stages that are closer to the possible deployment of an algorithm.
# In Wahl et al. (2022), see section 2.9.1 Machine learning model.
#
# -o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o
# Step 4: Model training
# ----------------------
#
# Resampling: Leave one subject out resampling. That is, use 20 of the 21 subjects for training the algorithm, use the left out subject to test the trained algorithm. This automatically determines that there will be 21 test data sets, across which prediction performance can be evaluated, using performance metrics for a binary classification task.
# In Wahl et al. (2022), see section 2.10 Cross-validation setup.
#
# For something such as leave-one-subject out resampling, mlr3 provides "custom resampling" among the available resampling setups. To set this up, we need the column "subject" from the d dataset, and create two list objects, one for training, one for testing (evaluation).
#
# Two empty lists (trainRsmp and testRsmp) which consecutively will be filled with subjects' data; consecutively = 21 loops (one loop per subject)
trainRsmp <- testRsmp <- list()
for(i in unique(d$subject)) {
    # In increasing order (subject 1, 2, ..., 21), predict this subject's outcome,
    # using the remaining 20 subjects for training the model, ...
    trainRsmp[[as.character(i)]] <- (1:nrow(d))[-which(d$subject == i)]
    # ... while using one subject's outcome data to test (evaluate) the training model.
    testRsmp[[as.character(i)]] <- which(d$subject == i)
}
# Check that each of the two lists contains 21 entries (of d row indices).
length(trainRsmp); length(testRsmp)
#
# ---------------------------------------------
# 21 runs overall, each time data of 20 persons = training,
# while data of hold-out person = test (validation).
# Training data size, for each of the 21 overall runs:
lapply(trainRsmp, length)
# Test (validation) data size, for each of the 21 overall runs:
lapply(testRsmp, length)
# ---------------------------------------------
#
# Custom resampling (leave one subject out), based on d dataset.

# Set the task to 'classificiation' (due to outcome being dichotomous/binary).
# tskBin = task for binary outcome.
(tskBin <- TaskClassif$new(id="dClassif", backend=d[,-c(1,3)], target="washType", positive = "compulsive"))
#
# Select mlr3's custom resampling option
resamplingCustom <- rsmp("custom")
# Instantiate custom resampling with the lists for training and for testing.
resamplingCustom$instantiate(tskBin, train_sets = trainRsmp, test_sets = testRsmp)
#
# Leave-one-subject-out cross validation setup
design = benchmark_grid(
    tasks = tskBin,
    learners = list(SVM,
                    SVMRB,
                    RANDOMFOREST,
                    NAIVEBAYES),
    resamplings = resamplingCustom)

# bmr: benchmark result. 21 validations and four selected algorithms = 84 iterations in total. Time to finish approximately 10 seconds.
bmr <- benchmark(design, store_models = TRUE)
#
# -o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o
# Step 5. Evaluate prediction performance
# ---------------------------------------
# 
measures <- list(msr("classif.auc", id = "AUC"))
# Output in console
bmr$aggregate(measures)[,c("learner_id", "AUC")]

bmrScore <- bmr$score(measures)[,c("learner_id", "AUC")]

# Collect the six validation performances of all algorithms
bmrPerf <-
    as.data.frame(as.data.table(bmr$score(measures)[,c("learner_id", "AUC")]))
# Make the names of the algorithms shorter by removing 'scale.regr'
bmrPerf$learner_id <- gsub("classif.", "", bmrPerf$learner_id)

# Display boxplot
ggplot(data=bmrPerf, aes(x=learner_id, y=AUC)) +
    geom_boxplot() +
    theme_bw(base_size=16)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -