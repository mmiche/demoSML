# Source of this R script:
# https://github.com/mmiche/demoSML
# Code that belongs to demoSML package vignette:
# 'B1: R code for appendix C'
# ------------------------------------------------------------

# B.1 R code that underlay appendix C

# Code underlying appendix C

library(ggplot2)

# Small standardization example

values <- c(5, 6, 7, 8)
# Compute standardized values manually
(values - mean(values))/sd(values)
# Output

# Compute standardized values with R function scale()
scale(values) # mean = 6.5, sd = 1.290994
# Output

# Confirm that the transformed values have mean = 0 and sd = 1
mean(scale(values)) # mean = 0
# Output

sd(scale(values)) # sd = 1
# Output

# ------------------------------------------------------------
# Standardization (z-transformation) of predictor(s) in sML

trainingValues <- c(5, 6, 7, 8)
# Save mean of trainingValues
trainingMean <- mean(trainingValues)
# Save sd of trainingValues
trainingSD <- sd(trainingValues)
testValues <- c(6, 7, 8)
# Scale the test values according to mean and sd of the training values
(testValuesScaled <- scale(testValues, center = trainingMean, scale = trainingSD))
# Output

# Unless training and test values are exactly alike, test mean is not 0, test sd is not 1.
mean(testValuesScaled)
# Output

sd(testValuesScaled)
# Output
# ------------------------------------------------------------


# Apply demoSML function 'makeData'.
# --------------------------------------------

d15 <- makeData(n=15, errSD = 2)
d45 <- makeData(n=45, errSD = 2)
d70 <- makeData(n=70, errSD = 2)
d100 <- makeData(n=100, errSD = 2)


# d15
ggplot2::ggplot(data = d15, ggplot2::aes(x=x, y=y)) +
    ggplot2::geom_point(size=2) + theme_classic(base_size = 16) +
    xlab("Predictor") + ylab("Outcome")
# d45
ggplot2::ggplot(data = d45, ggplot2::aes(x=x, y=y)) +
    ggplot2::geom_point(size=2) + theme_classic(base_size = 16) +
    xlab("Predictor") + ylab("Outcome")
# d70
ggplot2::ggplot(data = d70, ggplot2::aes(x=x, y=y)) +
    ggplot2::geom_point(size=2) + theme_classic(base_size = 16) +
    xlab("Predictor") + ylab("Outcome")
# d100
ggplot2::ggplot(data = d100, ggplot2::aes(x=x, y=y)) +
    ggplot2::geom_point(size=2) + theme_classic(base_size = 16) +
    xlab("Predictor") + ylab("Outcome")

# --------------------------------------------


# Function myDemoSML1
myDemoSML1 <- function(data=NULL, tune=NULL, seed=NULL) {
    # Explain abbreviations:
    # mse: mean squared error (= mean of residual sum of squared errors)
    # V = Validation subset; Te = Test subset
    # TeLost = 3 other test cases that did not win
    # mseTr = mse of training subset
    # coefs = Intercept and regression weight of each of the
    # 20 models (5 outer loops times 4 inner loops = 20).
    # idxBest: Collect the best among the 4 test performances.
    # Remember: Test performance occurs in the inner loop.
    mseV <- mseTe <- mseTeLost <- mseTr <- coefs <- idxBest <- c()
    
    # Complement to idxBest (per 4 test performances: 1 win, 3 losses)
    idxLostLs <- list()
    
    # Split total sample into 5 subsets of equal size:
    idxSplit5 <- rep(1:5, each=nrow(data)/5)
    # Set a predefined seed (ensure reproducibility)
    set.seed(seed)
    # Generate random assignment to one of the 5 subsets
    idx_i <- sample(idxSplit5)
    
    for(i in 1:5) {

        # Select the validation subset
        # (hold-out until final performance estimation is due)
        validationRows <- which(idx_i==i)
        
        # vd: validation sample (one fifth of total sample)
        vd <- ds[validationRows,]
        # ttd: training and test sample (four fifths of total sample)
        ttd <- ds[-validationRows,]
        
        # mse_j: mean squared error across the 4 test subsets.
        mse_j <- c()
        # List with which to collect the linear model coefficients.
        lms <- list()
        
        # mnSdLs: List with which to collect the mean and the standard
        # deviation of the predictor values in the training subset.
        mnSdLs <- list()
        
        # For the training and subsequent test session, use all indices
        # of idx_i, except for the held-out validation subset.
        idx_j <- idx_i[idx_i!=i]
        
        # trainingSequence: A variable that will increase from 1 to 4
        # within each training session.
        trainingSequence <- 1
        # Inner loop
        # (4 training sessions for each of the 5 outer loops)
        for(j in sort(unique(idx_j))) {
            
            # Set the training subset (three fifths of the total sample)
            trainingRows <- which(idx_j!=j)
            
            # Set the test subset (one fifth of the total sample)
            testRows <- which(idx_j==j)
            
            # td: training data
            td <- ttd[-testRows,]
            
            # mean and sd of td (collect in the list mnSdLs)
            mnSdLs[[trainingSequence]] <- c(mean(td$x), sd(td$x))
            
            # scale td
            td <- data.frame(x=scale(td$x),
                             y=td$y)
            
            # T R A I N I N G
            # lmj: linear model in loop number j
            lmj <- lm(y ~ x, data = td)
            
            # Nudge the regression weight a bit?
            if(!is.null(tune)) {
                # T U N I N G
                # Execute the dummy tuning
                lmj$coefficients[2] <- lmj$coefficients[2] + tune
            }
            
            # Add the fitted linear model to the list
            lms[[trainingSequence]] <- lmj
            
            # Fit the training model to the training sample
            # Sometimes called: Apparent prediction performance
            ftr <- predict(lmj, newdata=td)
            # sqrdTrThreeObs: squared residuals of the training subset.
            sqrdTrThreeObs <- (td[,"y"]-ftr)**2
            # mse of training subset
            mseTr <- c(mseTr, sum(sqrdTrThreeObs)/length(sqrdTrThreeObs))
            
            # T E S T I N G
            
            # z-transform test data, using the training data mean and sd
            # ---------------------
            ttd.z <- data.frame(
                x=scale(ttd[testRows,"x"],
                        center = mnSdLs[[trainingSequence]][1],
                        scale = mnSdLs[[trainingSequence]][2]),
                y=ttd[testRows,"y"])
            
            # Fit the training model to the test sample (one fifth of total sample)
            ft <- predict(lmj, newdata=ttd.z)
            # Collect the test prediction performance result.
            mse_j <- c(mse_j, sum(((ttd.z[,"y"]-ft)**2))/length(ft))
            
            # Increase training session counter by 1
            trainingSequence <- trainingSequence + 1
        }
        # After each inner loop (4 iterations), collect the
        # intercept and the regression weight of the fitted
        # linear models.
        coefs <- c(coefs, unlist(lapply(lms, FUN = function(x) {
            as.numeric(coefficients(x))
        })))
        
        # Which training session yielded the best test performance?
        # That is, which had the minimum mean squared prediction error?
        idxBest_i <- which(mse_j == min(mse_j))
        # Collect the winner after each inner loop.
        # idxBest includes the index of the winning model, that is
        # a value between 1 and 4.
        idxBest <- c(idxBest, idxBest_i)
        # Collect the best test prediction performance estimate.
        # Best performance = smallest mean squared prediction error.
        mseTe <- c(mseTe, mse_j[idxBest_i])
        # Collect the remaining test prediction performance estimates.
        # Always three mean squared prediction errors, the ones that were
        # not the smallest of the four mean squared prediction errors.
        mseTeLost <- c(mseTeLost, mse_j[-idxBest_i])
        # Collect the indices of the 3 remaining test prediction performances.
        idxLostLs[[i]] <- which(mse_j > min(mse_j))
        
        # V A L I D A T I N G
        
        # z-transform validation data, using the training data mean and sd
        # ---------------------------
        vd.z <- data.frame(
            x=scale(vd[,"x"],
                    center = mnSdLs[[idxBest_i]][1],
                    scale = mnSdLs[[idxBest_i]][2]),
            y=vd$y)
        
        # ftv: Fit the best training model to the validation sample.
        ftv <- predict(lms[[idxBest_i]], newdata = vd.z)
        # Collect the final real-world prediction performance estimate.
        # V = validation subset
        mseV <- c(mseV, sum(((vd.z[,"y"]-ftv)**2))/length(ftv))
    }
    
    # Held-out validation subsets 1-5
    # (1 held-out subset for each of the 5 runs; runs = outer loop).
    Vcase <- 1:5
    # 3 loosing test performances per outer loop.
    # (Use as index vector in for-loop below)
    mseTeLostOrdIdx <- rep(1:5, each=3)
    
    # mseTeAll: Vector to collect all test performances,
    # i.e., 1 winning and 3 loosing performances per run.
    mseTeAll <- c()
    for(k in Vcase) {
        # mseTe.k: All 4 test performances
        mseTe.k <- c(mseTe[k], mseTeLost[mseTeLostOrdIdx==k])
        # Mixed-up order of all 4 test performances
        mseTe.kIdx <- c(idxBest[k], idxLostLs[[k]])
        # Generate correct order of all 4 test performances.
        mseTe.k <- mseTe.k[order(mseTe.kIdx)]
        # Collect all 4 test performances in their correct order.
        mseTeAll <- c(mseTeAll, mseTe.k)
    }
    # Repeat index of best test performance 4 times, i.e.,
    # 5 winning indices times 4 equals a vector of length 20.
    bestTest <- rep(as.numeric(idxBest), each=4)
    # Generate another vector of length 20, which contains 5
    # times the same increasing sequence 1, 2, 3, 4.
    idxBestTest <- rep(1:4, times=5)
    # By testing both vectors 'bestVec' and 'idxBestTest' for
    # equality (==), we obtain a logical vector, where TRUE
    # are the 5 winning performances across all 20 performances.
    # This logical vector is used in the detailed output below.
    bestValidation <- which(bestTest == idxBestTest)
    
    # perfAll: All 20 performances in detail.
    perfAll <- data.frame(Run=rep(1:5, each=4),
                          bestTest=idxBestTest,
                          mseTraining=mseTr,
                          mseTest=mseTeAll,
                          mseValidation=rep(mseV, each=4))
    # Here, the logical vector bestValidation is used. All
    # validation performances that were NOT best (per inner
    # loop), are set to NA.
    perfAll$mseValidation[-bestValidation] <- NA
    
    # Data set that shows test and validation performance,
    # and the gap of the mean squared residual error per run.
    testValidDf <- data.frame(mseTe, mseV, gap=mseV-mseTe)
    
    return(list(testValidDf=testValidDf,
                # coefsMat: col 1 = intercept, col 2 = x weight
                # Four rows for each of the five runs = 20 rows.
                # Each of the four rows represents four different
                # test performances, since one out of the remaining
                # four subsets is successively used as test subset.
                coefsMat=matrix(data = coefs, ncol=2, byrow = TRUE),
                # 3 loosing test performances per run.
                mseTeLost=matrix(mseTeLost, ncol=5),
                # 4 training performances per run.
                # (mse: mean squared error)
                mseTr=matrix(mseTr, ncol=5),
                # All 4 test performances (win and loose) per run.
                mseTeAll=matrix(mseTeAll, ncol=5),
                # All 20 performances in detail.
                perfAll=perfAll))
}


# ------------------------------------------------------------

# Run 18 a-e cycles
# -----------------

# testPerfNoTune: Empty vector with which to collect test performance results, when no tuning took place.
# validPerfNoTune: Empty vector with which to collect validation performance results, when no tuning took place.
# testPerfTune: Empty vector with which to collect test performance results, when tuning took place.
# validPerfTune: Empty vector with which to collect validation performance results, when tuning took place.
# tuneVec: Empty vector with which to collect the selected tuning values (range .001-1.0).
# nvec: Empty vector with which to collect the sample size of the respective performance results.
testPerfNoTune <- validPerfNoTune <- testPerfTune <- validPerfTune <- tuneVec <- nvec <- c()

# Set 18 different sample sizes: 15, 20, ..., 100
smps <- seq(15, 100, by=5)

# Start for-loop, one loop for each sample size.
for(n in smps) {
    # Print start to R console (shows that the loop started)
    print("loop start ...")
    
    # ds: data set with n many observations. Draw predictor and prediction error values from a normal distribution, the error values have a mean of zero and a standard deviation of 2. The functional form of the association is linear (intercept is set to 1, regression weight is set to 3); see figure 1C in appendix C of the main document.
    ds <- makeData(n=n, errSD = 2)
    
    # Collect current sample size
    nvec <- c(nvec, rep(n, times=5))
    
    # --------------------------
    # W I T H O U T  T U N I N G
    # One a-e cycle is initiated by using the function myDemoSML1, here: without tuning.
    lm1 <- myDemoSML1(data=ds, seed = 1)
    
    # Collect test and validation prediction performance (all 5 outer runs)
    testPerfNoTune <- c(testPerfNoTune, lm1$testValidDf[,1])
    validPerfNoTune <- c(validPerfNoTune, lm1$testValidDf[,2])
    
    # --------------------------
    # W I T H  T U N I N G
    # Experiment with 100 different tuning values, using a range between .001 and 1.
    tunerGrid <- seq(.001, 1, length.out=100)
    # meanPerf: Vector with which to collect the mean test performance of an a-e cycle, here: with tuning.
    meanPerf <- c()
    for(t in tunerGrid) {
        meanPerf <- c(meanPerf, colMeans(
            myDemoSML1(data=ds, seed = 1, tune=t)$testValidDf)[1])
    }
    # Put all 100 tuned test performances in a data.frame.
    tuneLin <- data.frame(x=1:length(tunerGrid), y=meanPerf, t=tunerGrid)
    # Select tuning value with the minimum test prediction error.
    linMin <- which(tuneLin$y == min(tuneLin$y))
    # Run again with the tuning value being set:
    lm2 <- myDemoSML1(data=ds, seed = 1,
                      tune = tuneLin[linMin,"t"])
    
    # Collect test and validation prediction performance (all 5 outer runs)
    testPerfTune <- c(testPerfTune, lm2$testValidDf[,1])
    validPerfTune <- c(validPerfTune, lm2$testValidDf[,2])
    
    # Collect the tuning value with which the best test performance was achieved.
    tuneVec <- c(tuneVec, tuneLin[linMin,"t"])
    # Print end to R console (shows that the loop finished)
    print(paste0("loop end. Sample size: ", n))
}



# df = data.frame, Box = boxplot
dfBox <- data.frame(testPerfNoTune, validPerfNoTune, testPerfTune, validPerfTune)
# Prepare dfBox for visualization in ggplot2
dfBox1 <- reshape2::melt(dfBox, measure.vars = names(dfBox))
# Add sample sizes to dfBox1
dfBox1$xax <- nvec
# Add the condition of the performance results (test versus validation)
dfBox1$condition <- rep(rep(c("Test", "Validation"), each=length(smps)*5), times=2)
# Add column that shows whether or not tuning took place
dfBox1$tuned <- rep(c("no", "yes"), each=length(nvec)*2)
# Transform column xax (x-axis) to the class factor (use the forcats R package)
dfBox1$xax <- forcats::as_factor(dfBox1$xax)

ggplot(data=dfBox1, aes(x=xax, y=value, color=tuned)) +
    geom_boxplot() +
    scale_color_manual(values=c("darkgray", "black")) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2.5)) +
    xlab(label="Sample size") + ylab(label = "Mean squared error") +
    facet_wrap(~condition) +
    theme_bw(base_size = 16)

# -----------
# Plot for MS (only for the four selected sample sizes, show boxplots only, no points)
#
idxBox <- dfBox1$xax %in% c(15,45,70,100)
# dfBox1$value[!idxBox] <- NA
dfBox2Main <- dfBox1[idxBox,]
dfBox2Main$xax <- forcats::as_factor(dfBox2Main$xax)

ggplot(data=dfBox2Main, aes(x=xax, y=value, color=tuned)) +
    geom_boxplot() +
    # geom_point(aes(x=xax, y=value1, color=tuned)) +
    scale_color_manual(values=c("darkgray", "black")) +
    # scale_x_discrete(guide = guide_axis(n.dodge = 2.5)) +
    xlab(label="Sample size") + ylab(label = "Mean squared error") +
    facet_wrap(~condition) +
    theme_bw(base_size = 16)


# # Dummy-tuning values
(tuneVecDf <- data.frame(smps, t=tuneVec))
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -