# Source of this R script:
# https://github.com/mmiche/demoSML
# Code that belongs to demoSML package vignette:
# 'C0: R code Kuhn et al. (2021) data set'
# ------------------------------------------------------------

library(demoSML)
library(car)
library(ggplot2)
library(reshape2)
library(glmnet)
library(glmnetUtils)
#
# Introduction: C0 shows an actual, yet minimal, competition between two regression algorithms: The multiple linear regression model (baseline model) and the lasso (least absolute shrinkange and selection operator) regression model. Both models belong to the same family of models.
#
d <- supplc

# I M P O R T A N T:
# The code below works only, as long as the outcome is the first column in d, followed by the predictors.

dBox <- reshape2::melt(d, measure.vars = names(d))

ggplot(data=dBox, aes(x=variable, y=value)) + geom_boxplot() +
    theme_bw(base_size=16) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2.5))

# Transform outcome values with logarithm, append it as new column to d.
d[,"covid_general_totalLog"] <- 
    # Addition of 1 to each original outcome value, otherwise the value 0 gets removed (log of 0 is not defined).
    log(d[,"covid_general_total"]+1)

cbind(colnames(d))

dLogBox <- reshape2::melt(d[ncol(d)], measure.vars = names(d)[ncol(d)])

ggplot(data=dLogBox, aes(x=variable, y=value)) + geom_boxplot() +
    theme_bw(base_size=18)

# Original outcome value
(analysis <- colnames(d)[-14])
# Log-transformed outcome values
(analysisLog <- colnames(d)[c(14,2:13)])

# Step 1: Preprocessing: Check Multicollinearity

# Check multicollinearity with car::vif (variance inflation factor)

# Setup the model formula for the linear regression model
(fmla <- formula(x=paste(analysis[1], "~", paste(analysis[-1], collapse = "+"))))

# vif > 5 (or 10) are heuristics to assume the presence of multicollinearity
car::vif(lm(fmla, data = d))
# Error message 'there are aliased coefficients in the model' mean that there is 'perfect multicollinearity' among the current predictors; = at least one of the predictors is a linear transformation of other predictors in the model; = linear dependency exists among the predictors.

# Check correlation matrix for particularly high correlations.

# Correlation matrix (without the log-transformed outcome, column 14)
corMat <- cor(d[,-14])
# Diagonal is irrelevant, therefore set to 0:
diag(corMat) <- 0
# Correlations > .8
cor.8 <- corMat > .8
# Display variable names
cbind(apply(cor.8, 1, function(x) any(x==TRUE)))
# Display correlation matrix of the BADE variables:
cor(d[,c("BADE_total", "BADE_emot", "BADE_neutr")])

# Remove BADE_total from model with original outcome values.
# Remove by ignoring index 11.
(analysis <- colnames(d)[c(1:10,12:13)])
# Remove BADE_total from model with log-tranformed outcome values.
# Remove by ignoring index 11.
(analysisLog <- colnames(d)[c(14,2:10,12:13)])

# Model formula (without BADE_total)
fmla <- formula(x=paste(analysis[1], "~", paste(analysis[-1], collapse = "+")))

# Range of VIF values across all predictors (after removal of BADE_total)
range(car::vif(lm(fmla, data = d)))

# Function my.lm to run supervised ML procedure, using the linear regression model (lm).
my.lm <- function(d=NULL, f=NULL, seeds=NULL) {
    # Three vectors, with which to collect
    # the estimated model coefficients (intercept
    # and regression weights), and the training
    # and test subset mean squared error (mse).
    coefs <- mseTrain <- mseTest <- c()
    # Two lists, with which to collect the detailed
    # training and test predictions of the linear
    # prediction model.
    trainDetail <- testDetail <- list()
    for(i in 1:length(seeds)) {
        # T R A I N I N G
        # Training data set for iteration i
        set.seed(seeds[i])
        trainIdx <- sample(nrow(d), .8*nrow(d))
        train_i <- d[trainIdx,]
        # Compute mean of all columns.
        mean_i <- colMeans(train_i)
        # Compute standard deviation of all columns.
        sd_i <- apply(train_i, MARGIN = 2, FUN = sd)
        # z-transform all columns in training data.
        train_i_z <- data.frame(scale(train_i))
        # Re-insert the original outcome values.
        train_i_z[,1] <- d[trainIdx,1]
        # Train/develop the model by the training data.
        mod <- lm(f, data=train_i_z)
        # Estimated model coefficients
        coefs <- c(coefs, mod$coefficients)
        # Collect the mean squared error (mse) within the training data.
        mseTrain <- c(mseTrain, mean(mod$residuals**2))
        # Collect the detailed training predictions.
        trainDetail[[i]] <- data.frame(actual=d[trainIdx,1],
                                       fitted=as.numeric(mod$fitted),
                                       itrn=i)
        # T E S T  (V A L I D A T I N G)
        testIdx <- (1:nrow(d))[-trainIdx]
        # z-transformed test data set for iteration i, use mean and sd from training
        test_i_z <- data.frame(scale(d[testIdx,], center = mean_i, scale = sd_i))
        # Re-insert the original outcome values.
        test_i_z[,1] <- d[testIdx,1]
        # Test/evaluate model = Apply training model to holdout test subset.
        # fitted = predicted outcome values in test data set.
        fittedTest <- predict(mod, newdata=test_i_z)
        # Residuals in test data
        rsdlTest <- as.numeric(test_i_z[,1] - fittedTest)
        # Save mean squared error (mse) within the test data.
        mseTest <- c(mseTest, mean(rsdlTest**2))
        # Collect the detailed test predictions.
        testDetail[[i]] <- data.frame(actual=d[testIdx,1],
                                      fitted=as.numeric(fittedTest),
                                      itrn=i)
    }
    # Output returned to the user.
    return(list(
        # MSE in the training subset (apparent performance).
        mseTrain=mseTrain,
        # MSE in the test subset (validation performance).
        mseTest=mseTest,
        # Estimated model coefficients.
        coefs=coefs,
        # Detailed overview of prediction performance in the training subset.
        trainDetail=trainDetail,
        # Detailed overview of prediction performance in the test subset.
        testDetail=testDetail))
}

# Function my.lasso to run supervised ML procedure, using the lasso regression model.
my.lasso <- function(d=NULL, s=NULL, seeds=NULL) {
    # Three vectors, with which to collect
    # the estimated model coefficients (intercept
    # and regression weights), and the training
    # and test subset mean squared error (mse).
    lassoCoefs <- mseTrain <- mseTest <- c()
    # Two lists, with which to collect the detailed
    # training and test predictions of the linear
    # prediction model.
    trainDetail <- testDetail <- list()
    for(i in 1:length(seeds)) {
        # ---------------
        # T R A I N I N G
        set.seed(seeds[i])
        trainIdx <- sample(nrow(d), .8*nrow(d))
        # Extract the training data, only the predictors.
        train_i <- d[trainIdx,-1]
        # Compute mean of all predictors.
        mean_i <- colMeans(train_i)
        # Compute standard deviation of all predictors.
        sd_i <- apply(train_i, MARGIN = 2, FUN = sd)
        # z-transform all predictors in training data.
        train_i_z <- scale(train_i)
        # Train/develop the model with the training data.
        set.seed(12345)
        mod <- glmnet(x=train_i_z,
                      # Outcome values are not z-transformed.
                      y=d[trainIdx,1],
                      # gaussian = regression, alpha = 1 = lasso.
                      family="gaussian", alpha=1)
        # Estimated model coefficients for lambda value of s,
        # e.g., s = 1.
        lassoCoefs <- c(lassoCoefs, as.numeric(coef(mod, s = s)))
        # Fit the model to the training data (obtain apparent performance)
        fittedTrain <- predict(mod, newx = train_i_z, s = s)
        # Compute residuals (prediction errors)
        rsdlTrain <- d[trainIdx,1] - fittedTrain
        # Save the mean squared error (mse) within the training data.
        mseTrain <- c(mseTrain, mean(rsdlTrain**2))
        # Collect the detailed training predictions.
        trainDetail[[i]] <- data.frame(actual=d[trainIdx,1],
                                       fitted=as.numeric(fittedTrain),
                                       itrn=i)
        # ------------------------------
        # T E S T  (V A L I D A T I N G)
        # z-transformed test data set for iteration i, use mean and sd from training.     
        testIdx <- (1:nrow(d))[-trainIdx]
        test_i_z <- scale(d[testIdx,-1], center = mean_i, scale = sd_i)
        # Test/evaluate model = Apply training model to holdout test subset.
        # fitted = predicted outcome values in test data set.
        fittedTest <- predict(mod, newx=test_i_z, s = s)
        # Residuals in test data
        rsdlTest <- d[testIdx,1] - fittedTest
        # Save mean squared error (mse) within the test data.
        mseTest <- c(mseTest, mean(rsdlTest**2))
        # Collect the detailed test predictions.
        testDetail[[i]] <- data.frame(actual=d[testIdx,1],
                                      fitted=as.numeric(fittedTest),
                                      itrn=i)
    }
    # Output returned to the user.
    return(list(
        # MSE in the training subset (apparent performance).
        mseTrain=mseTrain,
        # MSE in the test subset (validation performance).
        mseTest=mseTest,
        # Estimated model coefficients.
        lassoCoefs=lassoCoefs,
        # Detailed overview of prediction performance in the training subset.
        trainDetail=trainDetail,
        # Detailed overview of prediction performance in the test subset.
        testDetail=testDetail))
}

# Guarantee to replicate results by using set.seed.
set.seed(1)
mod_cv <- glmnet::cv.glmnet(
    x=scale(d[,analysis[-1]]),
    y=d[,analysis[1]], family='gaussian',
    alpha=1, nfolds = 10)
(min1se <- c(mod_cv$lambda.min, mod_cv$lambda.1se))
# Output

mean(min1se)
# Output


# 10 arbitrary seeds, to demonstrate 10 sML runs which are guaranteed to replicate.
seeds <- c(24388, 59521, 43307, 69586, 11571,
           25173, 32618, 13903, 8229, 25305)

# Outcome: covid_general_total

# Linear regression
lmRes <- my.lm(d=d[,analysis], f=fmla, seeds = seeds)

# Lasso regression

# Using a single fixed lambda tuning parameter of 1.1 in all 10 different runs.
lassoRes <- my.lasso(d=d[,analysis], s=1.1, seeds = seeds)
# ------------

# Comparison between lm and lasso, of performances in training and test subsets.
(lm.lasso.Df1 <- data.frame(trainLM=lmRes$mseTrain,
                           testLM=lmRes$mseTest,
                           trainLO=lassoRes$mseTrain,
                           testLO=lassoRes$mseTest))
# Overall mean performance across all 10 different runs.
colMeans(lm.lasso.Df1)

lm.lasso.Df1Box <- reshape2::melt(lm.lasso.Df1, measure.vars = names(lm.lasso.Df1))

ggplot(data=lm.lasso.Df1Box, aes(x=variable, y=value)) + geom_boxplot() +
    theme_bw(base_size=16) +
    ylab(label="mean squared error (mse)") +
    xlab(label="Training-/Test performance of each model")

# Lasso can nudge model parameter estimates down to zero, therefore removing the respective predictor from the model. This can be relevant to minimize resources in a real-world setting. For instance, if one or more predictors from a set of predictors do not need to be obtained, in order to achieve a certain prediction performance, time and costs can be saved for measuring these predictors.
coefLO <- matrix(data=lassoRes$lassoCoefs, nrow=length(analysis))
rownames(coefLO) <- c("Intercept", analysis[-1])
# For a lambda value of 1.1, three of the 11 predictors (age, country, and BADE_neutr) have been removed by the lasso algorithm in all of the 10 runs. BADE_emot was removed in 8 of 10 runs, indicating that it also is not very important to achieve a similar prediction success as the linear regression model, which uses all of the 11 predictors in each of the 10 runs.
round(coefLO, digits=2)
# ------------

# ------------

# Compute optimal lambda values for log-transformed outcome values

# Guarantee to replicate results by using set.seed.
set.seed(1)
mod_cvLog <- glmnet::cv.glmnet(
    x=scale(d[,analysisLog[-1]]),
    y=d[,analysisLog[1]], family='gaussian',
    alpha=1, nfolds = 10)
(min1seLog <- c(mod_cvLog$lambda.min, mod_cvLog$lambda.1se))
# Output

mean(min1seLog)
# Output
# ------------

# Outcome: covid_general_totalLog (log-transformed outcome values)

# Linear regression

# Set up the model formula for covid_general_totalLog
fmlaLog <- formula(x=paste(analysisLog[1], "~", paste(analysisLog[-1], collapse = "+")))
lmResLog <- my.lm(d=d[,analysisLog], f=fmlaLog, seeds = seeds)

# Lasso
lassoResLog <- my.lasso(d=d[,analysisLog], s=0.05, seeds = seeds)

(lm.lasso.Df1Log <- data.frame(trainLM=lmResLog$mseTrain,
                           testLM=lmResLog$mseTest,
                           trainLO=lassoResLog$mseTrain,
                           testLO=lassoResLog$mseTest))
colMeans(lm.lasso.Df1Log)


coefLog <- matrix(data=lassoResLog$lassoCoefs, nrow=length(analysisLog))
rownames(coefLog) <- c("Intercept", analysisLog[-1])
round(coefLog, digits=2)


# -------------------------------------------

summary(lm(formula=fmla, data = d))

summary(lm(formula=fmlaLog, data = d))

# -------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -