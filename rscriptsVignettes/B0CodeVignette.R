# Source of this R script:
# https://github.com/mmiche/demoSML
# Code that belongs to demoSML package vignette:
# 'B0: R code underlying the manuscript and appendices A and B'
# ------------------------------------------------------------


(ds <- data.frame(x=c(4, 9, 10, 12, 15),
                  y=c(25, 40, 55, 80, 100)))

# install.packages("ggplot2")
library(ggplot2)

ggplot2::ggplot(data = ds, ggplot2::aes(x=x, y=y)) +
    ggplot2::geom_point(size=3) +
    theme_bw(base_size = 16) +
    xlab("Predictor") + ylab("Outcome")

# Step 1: Preprocessing
ds$x2 <- ds$x**2
# - - - -

ggplot(ds, aes(x, y)) +
    geom_point(size=3) +
    stat_smooth(method = "lm", se = FALSE, formula = y ~ I(x^2), size = 1, colour = "black") +
    theme_bw(base_size = 14) +
    xlab("Predictor") + ylab("Outcome")
# ---------------------------------------------------

# Linear model with squared predictor values
modQuad <- lm(y ~ x2, data = ds)
summary(modQuad)

# New predictor value
newPredVal <- 7
# Present new data with the squared predictor value to the 'predict' function.
predict(modQuad, newdata = data.frame(x2=newPredVal**2))

# Column 1 of upper table in figure 3
# Display the data set (ds) with the squared predictor values.
ds[,c("x2", "y")]
# Validation case = Row 1
# -------------------------------------
# Training session 1 (trS1; training cases: rows 3,4,5, test case = row 2)
# Generate model coefficient estimates
trS1 <- lm(y ~ x2, data = ds[c(3,4,5),])
coefficients((trS1))
# Apply trS1 to held-out test case, i.e., predict first of four outcome (p1.4)
(p1.4 <- predict(trS1, newdata = ds[2,]))
# Squared prediction error for test case (row no.2 in ds)
(ds[2,"y"] - p1.4)**2
# -------------------------------------
# Training session 2 (trS2; training cases: rows 2,4,5, test case = row 3)
# Generate model coefficient estimates
trS2 <- lm(y ~ x2, data = ds[c(2,4,5),])
coefficients((trS2))
# Apply trS2 to held-out test case, i.e., predict first of four outcome (p2.4)
(p2.4 <- predict(trS2, newdata = ds[3,]))
# Squared prediction error for test case (row no.3 in ds)
(ds[3,"y"] - p2.4)**2
# -------------------------------------
# Training session 3 (trS3; training cases: rows 2,3,5, test case = row 4)
# Generate model coefficient estimates
trS3 <- lm(y ~ x2, data = ds[c(2,3,5),])
coefficients((trS3))
# Apply trS3 to held-out test case, i.e., predict first of four outcome (p3.4)
(p3.4 <- predict(trS3, newdata = ds[4,]))
# Squared prediction error for test case (row no.4 in ds)
(ds[4,"y"] - p3.4)**2
# -------------------------------------
# Training session 4 (trS4; training cases: rows 2,3,4, test case = row 5)
# Generate model coefficient estimates
trS4 <- lm(y ~ x2, data = ds[c(2,3,4),])
coefficients((trS4))
# Apply trS4 to held-out test case, i.e., predict first of four outcome (p4.4)
(p4.4 <- predict(trS4, newdata = ds[5,]))
# Squared prediction error for test case (row no.5 in ds)
(ds[5,"y"] - p4.4)**2
# -------------------------------------
# -------------------------------------
# Best test model is trS2, therefore apply to held-out validation case
(v1 <- predict(trS2, newdata = ds[1,]))
# How close is the predicted from the actual outcome of the validation case?
(ds[1,"y"] - v1)**2
# -------------------------------------

# Linear model with original predictor values
modLin <- lm(y ~ x, data = ds)
summary(modLin)

# ---------------------------------------------------

# Function name: myDemoSML
# data = data set to be given to the function.
# quad = whether the predictor values shall be squared
# (quadratic term) or not; default is FALSE.
# tune = the value that shall be added to the linear model
# regression weight after model fitting; default is NULL,
# which means 'do not tune'.
myDemoSML <- function(data=NULL, quad=FALSE, tune=NULL) {
    # Explain abbreviations:
    # sqrd: squared residual (single case)
    # V = Validation case; Te = Test case
    # TeLost = 3 other test cases that did not win
    # rssTr = residual sum of squares (of the 3 training cases)
    # coefs = Intercept and regression weight of each of the
    # 20 models (5 outer loops times 4 inner loops = 20).
    # idxBest: Collect the best among the 4 test performances.
    # Remember: Test performance occurs in the inner loop.
    sqrdV <- sqrdTe <- sqrdTeLost <- rssTr <- coefs <- idxBest <- c()
    # Outer loop: A total of 5 validation performance estimates.
    # i <- 1
    for(i in 1:5) {
        # vd: hold out the validation sample (one observation)
        vd <- ds[i,]
        # It remains:
        # ttd: training and test sample (four observations)
        ttd <- ds[-i,]
        # An empty vector with which to collect the results,
        # one result per loop.
        # sqrd: squared residual of the test sample.
        sqrd <- c()
        # List with which to collect the linear model coefficients.
        lms <- list()
        # Inner loop
        # (4 training sessions for each of the 5 outer loops)
        for(j in 1:4) {
            # td: training data (three observations)
            td <- ttd[-j,]
            
            # T R A I N I N G (run the linear model)
            # lmj: linear model in loop number j
            if(quad) {
                # Quadratic predictor values are used.
                lmj <- lm(y ~ x2, data = td)
            } else {
                # Original predictor values are used.
                lmj <- lm(y ~ x, data = td)
            }
            
            # Nudge the regression weight of the linear model a bit?
            if(!is.null(tune)) {
                # T U N I N G
                # Execute the dummy tuning
                lmj$coefficients[2] <- lmj$coefficients[2] + tune
            }
            
            # Add the fitted linear model to the list
            lms[[j]] <- lmj
            
            # Fit the training model to the training sample.
            # Sometimes called: Apparent prediction performance
            ftr <- predict(lmj, newdata=td)
            # sqrdTrThreeObs: squared residuals of the three
            # observations in the training subset.
            sqrdTrThreeObs <- (td[,"y"]-ftr)**2
            # rssTr: residual sum of squares in the training subset.
            rssTr <- c(rssTr, sum(sqrdTrThreeObs))
            
            # T E S T I N G
            # Fit the training model to the test sample (one observation)
            ft <- predict(lmj, newdata=ttd[j,])
            # Collect the test prediction performance result.
            sqrd <- c(sqrd, (ttd[j,"y"]-ft)**2)
        }
        # After each inner loop (4 iterations), collect the
        # intercept and the regression weight of the fitted
        # linear models.
        coefs <- c(coefs, unlist(lapply(lms, FUN = function(x) {
            as.numeric(coefficients(x))
        })))
        
        # Which training session yielded the best test performance?
        # That is, which had the minimum squared prediction error?
        idxBest_i <- which(sqrd == min(sqrd))
        # Collect the winner after each inner loop.
        # idxBest includes the index of the winning model, that is
        # a value between 1 and 4.
        idxBest <- c(idxBest, idxBest_i)
        # Collect the best test prediction performance estimate.
        # Best performance = smallest squared prediction error.
        sqrdTe <- c(sqrdTe, sqrd[idxBest_i])
        # Collect the remaining test prediction performance estimates.
        # Always three squared prediction errors, the ones that were
        # not the smallest of the four squared prediction errors.
        sqrdTeLost <- c(sqrdTeLost, sqrd[-idxBest_i])
        
        # V A L I D A T I N G
        # ftv: Fit the best training model to the validation sample.
        ftv <- predict(lms[[idxBest_i]], newdata = vd)
        # Collect the final real-world prediction performance estimate.
        # V = validation case
        sqrdV <- c(sqrdV, (vd[,"y"]-ftv)**2)
    }
    
    # Held-out validation case 1-5
    # (1 held-out case for each of the 5 runs; runs = outer loop).
    Vcase <- 1:5
    # 3 loosing test performances per outer loop.
    # (Use as index vector in for-loop below)
    sqrdTeLostOrdIdx <- rep(1:5, each=3)
    
    # sqrdTeAll: Vector to collect ALL test performances,
    # i.e., 1 winning and 3 loosing performances per run.
    sqrdTeAll <- c()
    for(k in Vcase) {
        # sqrdTe.k: All 4 test performances
        sqrdTe.k <- c(sqrdTe[k], sqrdTeLost[sqrdTeLostOrdIdx==k])
        # Mixed-up order of all 4 test performances
        sqrdTe.kIdx <- as.numeric(names(sqrdTe.k))
        # Generate correct order of all 4 test performances.
        sqrdTe.k <- sqrdTe.k[order(sqrdTe.kIdx)]
        # Collect all 4 test performances in their correct order.
        sqrdTeAll <- c(sqrdTeAll, sqrdTe.k)
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
    bestValidation <- bestTest == idxBestTest
    
    # perfAll: All 20 performances in detail.
    perfAll <- data.frame(Run=rep(1:5, each=4),
                          bestTest=idxBestTest,
                          rssTraining=rssTr,
                          sqrdTest=sqrdTeAll,
                          sqrdValidation=rep(sqrdV, each=4))
    # Here, the logical vector bestValidation is used. All
    # validation performances that were NOT best (per inner
    # loop), are set to NA.
    perfAll$sqrdValidation[!bestValidation] <- NA
    
    # Data set that shows test and validation performance,
    # and the gap of the squared residual error per run.
    testValidDf <- data.frame(sqrdTe, sqrdV, gap=sqrdV-sqrdTe)
    
    return(list(testValidDf=testValidDf,
                # coefsMat: col 1 = intercept, col 2 = x weight
                # Four rows for each of the five runs = 20 rows.
                # Each of the four rows represents four different
                # test performances, since one out of the remaining
                # four rows is successively used as test case.
                coefsMat=matrix(data = coefs, ncol=2, byrow = TRUE),
                # 3 loosing test performances per run.
                sqrdTeLost=matrix(sqrdTeLost, ncol=5),
                # 4 training performances per run.
                # (rss: residual sum of squares)
                rssTr=matrix(rssTr, ncol=5),
                # All 4 test performances (win and loose) per run.
                sqrdTeAll=matrix(sqrdTeAll, ncol=5),
                # All 20 performances in detail.
                perfAll=perfAll))
}


# ---------------------------------
# Quadratic relationship
#
lmQuad <- myDemoSML(data=ds, quad = TRUE)

lmQuad$testValidDf
round(lmQuad$testValidDf, 2)

# Compute column means of columns 1 and 2
colMeans(lmQuad$testValidDf[,1:2])
round(colMeans(lmQuad$testValidDf[,1:2]), 2)

# coefs = coefficients of the fitted linear model
# Mat = matrix
lmQuad$coefsMat
round(lmQuad$coefsMat, 2)

# sqrd = squared residuals, Te = test performance
# Lost = test performance not being the best performance
lmQuad$sqrdTeLost
round(lmQuad$sqrdTeLost, 2)

# rss = residual sum of squared error
# Tr = training (3 of the total of 5 observations)
lmQuad$rssTr
round(lmQuad$rssTr, 2)

# sqrd = squared residuals, Te = test performance
# All = all four test performances after each training session
lmQuad$sqrdTeAll
round(lmQuad$sqrdTeAll, 2)

# perfAll = all 20 performances.
lmQuad$perfAll
round(lmQuad$perfAll, 2)

# 100 values between .001 and .1, all being equally
# distant to one another.
tunerGrid <- seq(.001, .1, length.out=100)
# Empty vector, with which to collect the prediction performance.
meanPerf <- c()
# For-loop to run the 100 tuning experiments.
for(t in tunerGrid) {
    # Run the function 'myDemoSML', then extract the
    # first column of the first element of the output
    # of the function myDemoSML (testValidDf[1] = 5
    # best test performances).
    meanPerf <- c(meanPerf, colMeans(myDemoSML(data=ds, quad = TRUE, tune=t)$testValidDf)[1])
}
# Put all 100 performances and the experimented tuning values
# in a data.frame.
tuneQuad <- data.frame(x=1:length(tunerGrid), y=meanPerf, t=tunerGrid)
# quadMin = quadratic model with the smallest mean prediction error.
quadMin <- which(tuneQuad$y == min(tuneQuad$y))
tuneQuad[quadMin,]

lmQuadTuned <- myDemoSML(data=ds, quad = TRUE, tune = tuneQuad[quadMin,"t"])
lmQuadTuned$testValidDf
round(lmQuadTuned$testValidDf, 2)

# Compute column means of columns 1 and 2
colMeans(lmQuadTuned$testValidDf[,1:2])
round(colMeans(lmQuadTuned$testValidDf[,1:2]), 2)
# ---------------------------------

# ---------------------------------
# Table 1B (left part, without tuning)
# ---------------------------------
# Linear relationship
#
lmLin <- myDemoSML(data=ds)
lmLin$testValidDf[,1:2]
round(lmLin$testValidDf[,1:2], 2)

# Compute column means of columns 1 and 2
colMeans(lmLin$testValidDf[,1:2])
round(colMeans(lmLin$testValidDf[,1:2]), 2)

# 100 values between .001 and .75, all being equally
# distant to one another.
tunerGrid <- seq(.001, .75, length.out=100)
# Empty vector, with which to collect the prediction performance.
meanPerf <- c()
# For-loop to run the 100 tuning experiments.
for(t in tunerGrid) {
    # Run the function 'myDemoSML', then extract the
    # second column of the first element of the output
    # of the function myDemoSML (= 5 best validation
    # performances).
    meanPerf <- c(meanPerf, colMeans(myDemoSML(data=ds, tune=t)$testValidDf)[1])
}
# Put all 100 performances and the experimented tuning values
# in a data.frame.
tuneLin <- data.frame(x=1:length(tunerGrid), y=meanPerf, t=tunerGrid)
# linMin = linear model with the smallest mean prediction error.
linMin <- which(tuneLin$y == min(tuneLin$y))
tuneLin[linMin,]

lmLinTuned <- myDemoSML(data=ds, tune = tuneLin[linMin,"t"])
lmLinTuned$testValidDf
round(lmLinTuned$testValidDf, 2)

# Compute column means of columns 1 and 2
colMeans(lmLinTuned$testValidDf[,1:2])
round(colMeans(lmLinTuned$testValidDf[,1:2]), 2)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -