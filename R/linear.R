#' Fit a linear model
#'
#' `linear()` fits a linear regression model given a single numerical response
#' variable and at least one numerical explanatory variable.
#'
#' @param
#' `y` = Name of the response variable in the specified dataset
#' If `data = NULL`, then y will be taken as the vector of the response
#' `x` = Names of the explanatory variables from the specified dataset
#' If `data = NULL`, then x will be taken as a matrix with one variable per column
#' `data` =  Dataframe in which `y` and `x` are variables. Defaults to `data = NULL`
#' `cat` = Logical: T`TRUE` if data contains categorical variables, `FALSE` otherwise.
#'
#' @return
#' `beta` = Vector of fitted parameter values from the model fit.
#' `fits` = Vector of fitted values of `y` from the regression model.
#' `res` = Vector containing the residuals from the regression model.
#' `SSq` = The Sums of Squares from the model fit.
#' `R.Sq` = The R-Squared value for the model fit.
#' `F.Test` = The ANOVA table from the model fit.
#' `model` = Matrix of the variables used in the model. First column corresponds to the response variable, `y`,
#' followed by the explanatory variables, `x`, in the order that they were specified in the function.
#'
#' @section Limitations:
#' `linear()` does not have built in support for interaction terms.
#' If it is desired to fit a model with an interaction, it must be coded as a separate variable
#' and then given to `linear()` as an additional variable within the `x` argument.
#'
#' @export

linear <- function(y, x, data = NULL, cat = FALSE) {

  ## Differentiate between cases where data are specified vs not specified
  if (is.null(data) == TRUE) {
    X <- x
    Y <- y
  } else if (is.null(data) == FALSE) {
    data <- na.omit(data)
    X <- data[, x]
    Y <- data[, y]
  }

  Y <- as.matrix(Y)

  ## Include categorical variables in the model if specified
  if (cat == TRUE) {
    X <- fastDummies::dummy_cols(X, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
    x <- colnames(X)
  }

  ## Save the model to be returned at the end
  model <- cbind(Y, X)

  ## Create the design matrix based on our explanatory variables
  # Differentiate between Simple Linear Regression and Multiple Linear Regression
  if (is.null(dim(X)) == TRUE) {
    X <- as.matrix(cbind(rep(1, length(X)), X))
  } else {
    X <- as.matrix(cbind(rep(1, nrow(X)), X))
  }

  ## Compute the regression parameters
  # This will compute the regression parameters for the specified model as well
  # as nested models in order to determine the Sums of Squares
  betaM <- matrix(rep(0, (dim(X)[2] -1) * dim(X)[2]), dim(X)[2], dim(X)[2]-1)

  for (i in 1:(dim(X)[2]-1)) {
    betaM[1:(i+1), i] <- solve(t(X[, 1:(i+1)]) %*% X[, 1:(i+1)]) %*% t(X[, 1:(i+1)]) %*% Y
  }

  beta <- as.matrix(betaM[, dim(betaM)[2]]) # Return ony the final column, corresponding to the desired model

  if (is.null(data) == FALSE) {
    rownames(beta) <- c("Intercept", x)
  } else {
    rownames(beta) <- c("Intercept", paste("x", as.character(1:(dim(beta)[1]-1)), sep = ""))
  }

  ## Compute the fitted values of the linear regression
  fitsM <- matrix(rep(0, (dim(X)[2]-1) * dim(X)[1]), dim(X)[1], dim(X)[2]-1)

  for (i in 1:(dim(X)[2]-1)) {
    fitsM[, i] <- X[, 1:(i+1)] %*% betaM[1:(i+1), i]
  }

  fits <- fitsM[, dim(fitsM)[2]] # Return only the final column, corresponding to the desired model

  ## Compute the residuals of the linear regression
  resM <- matrix(rep(Y, dim(fitsM)[2]), dim(fitsM)[1], dim(fitsM)[2]) - fitsM
  res <- Y - fits

  ## Compute the sums of squares of the fit
  SSE <- colSums(resM^2)
  SSR <- colSums((fitsM - mean(Y))^2)
  SST <- SSR + SSE
  SSq <- cbind(SSR, SSE, SST)[length(SSE), ] # Returns only final row corresponding to specified model
                                             # Other rows used to compute F.Test below

  ## Compute the R-squared value of the fit
  R.sq <- SSR/SST

  ## Compute the ANOVA Table
  F.Test <- matrix(rep(NA, 5 * dim(X)[2]), dim(X)[2], 5)

  # Determine the DF column
  F.Test[1:(dim(F.Test)[1]-1), 1] <- 1
  F.Test[dim(F.Test)[1], 1] <- dim(X)[1] - dim(F.Test)[1]

  # Determine the Sum Sq Column
  F.Test[1, 2] <- SSR[1]

  if (dim(F.Test)[1] > 2) {
    F.Test[2:(dim(F.Test)[1]-1), 2] <- diff(SSR)
  }

  F.Test[dim(F.Test)[1], 2] <- SSE[length(SSE)]

  # Determine the Mean Sq Column
  F.Test[, 3] <- F.Test[, 2] / F.Test[, 1]

  # Return the F Test Statistics
  F.Test[1:(dim(F.Test)[1]-1), 4] <- F.Test[1:(dim(F.Test)[1]-1), 3] / F.Test[dim(F.Test)[1], 3]

  # Return the p-values for the F test statistics
  F.Test[1:(dim(F.Test)[1]-1), 5] <- 1- pf(F.Test[1:(dim(F.Test)[1]-1), 4], 1, F.Test[dim(F.Test)[1], 1])

  # Name all rows/cols
  if (is.null(data) == TRUE) {
    rownames(F.Test) <- c(paste("x", as.character(1:(dim(F.Test)[1]-1)), sep = ""), "Residuals")
  } else if (is.null(data) == FALSE) {
    rownames(F.Test) <- c(x, "Residuals")
  }

  colnames(F.Test) <- c("DF", "Sum Sq", "Mean Sq", "F Value", "p-value")

  ## Return all of the values computed above
  return(list(beta = t(beta),
              fits = fits,
              res = res,
              SSq = SSq,
              R.sq = R.sq,
              F.Test = F.Test,
              model = model))
}
