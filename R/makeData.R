#' @title Simulate data.
#
#' @description Simulated data set with one continuous predictor and one continuous outcome, select sample size.
#
#' @param n Selected sample size of simulated data set, a single integer value greater than 0. Default value is 500.
#
#' @param xCoef Regression coefficient of a simulated model output, defaults to the coefficient 3.
#
#' @param errMean Mean of the normal distribution, from which the error values shall be sampled. Default value is 0.
#
#' @param errSD Standard deviation of the normal distribution, from which the error values shall be sampled. Default value is 1.
#
#' @param intercept Intercept of a simulated model output, defaults value is 1.
#
#' @param seed A single integer value. Setting a seed ensures reproducibility of a once simulated data set.
#
#' @return simData A data.frame with one continuous predictor column x and one continuous outcome column y.
#
#' @author Marcel Miché
#
#' @importFrom stats var
#
#' @examples
#' # Simulate data set with sample size of 100
#' dfSim <- makeData(n=100)
#
#' @export
#
makeData <- function(n=500, errMean=0, errSD=1, seed=1, intercept = 1, xCoef = 3) {
    
    # Error handling ...
    if(any(c(is.na(n),
             is.na(errMean),
             is.na(errSD),
             is.na(seed),
             is.na(intercept),
             is.na(xCoef)))) {
        stop("All function arguments must contain a numeric value. Please check. Default values are:\nn = 10\nerrMean = 0\nerrSD = 1\nseed = 1\nintercept = 1\nxCoef = 3")
    }
    
    if(any(c(is.null(n),
             is.null(errMean),
             is.null(errSD),
             is.null(seed),
             is.null(intercept),
             is.null(xCoef)))) {
        stop("All function arguments must contain a numeric value. Please check. Default values are:\nn = 10\nerrMean = 0\nerrSD = 1\nseed = 1\nintercept = 1\nxCoef = 3")
    }
    
    if(any(c(!is.numeric(n),
             !is.numeric(errMean),
             !is.numeric(errSD),
             !is.numeric(seed),
             !is.numeric(intercept),
             !is.numeric(xCoef)))) {
        stop("All function arguments must contain a numeric value. Please check. Default values are:\nn = 10\nerrMean = 0\nerrSD = 1\nseed = 1\nintercept = 1\nxCoef = 3")
    }
    
    # Set seed for reproducibility
    set.seed(seed)
    # Sample predictor values from normal distribution (mean = 0, sd = 1)
    x <- stats::rnorm(n=n)
    # Sample error values from normal distribution (select mean and sd yourself)
    err <- stats::rnorm(n=n, mean=errMean, sd=errSD) # error term
    # Setup functional form
    z <- intercept + xCoef*x
    # Generate outcome values
    y <- z + err
    # Put predictor and outcome in data set (x = predictor, y = outcome)
    simData <- data.frame(x=x, y=y)
    # Return data set to user.
    return(simData)
}
