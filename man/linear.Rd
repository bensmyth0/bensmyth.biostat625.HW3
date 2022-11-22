\name{linear}
\alias{linear}

\title{
Fit a Linear Model
}
\description{
Fit a linear model with y as the response variable and x as the predictor(s).
}
\usage{
linear(y, x, data = NULL)
}

\arguments{
  \item{y}{
    Name of the response variable from data to be used in fitting a linear model. If data = NULL, then y will be taken to be a vector containing the response variable.
  }
  \item{x}{
    Vector containing the names of the explanatory variables from data to be used to predict y. Variables must be numerical or interpretable as numerical. If data = NULL, then x will be taken to be a matrix of the the explanatory variables, with each column containing one variable.
  }
  \item{data}{
    Dataframe containing the response and explanatory variables.
  }
  \item{cat}{
    Logical: TRUE if the specified data contain categorical variables, FALSE if they do not. Defaults to FALSE.
  }
}
\value{
  \item{beta}{
    Vector containing the list of fitted parameter values from the linear model fit.
  }
  \item{fits}{
    Vector containing the fitted values of y from the regression model.
  }
  \item{res}{
    Vector containing the residuals of the fitted regression model.
  }
  \item{SSq}{
    The Sums of Squares of the linear model fit. Returns SSE (Sum of Squares Error), SSR (Sum of Squares Regression), and SST (Sum of Squares Total).
  }
  \item{R.Sq}{
    The R-Squared value of the linear model fit.
  }
  \item{F.Test}{
    The ANOVA table for the linear model fit. Includes a row for each variable in the fitted model. Computes the Degrees of Freedom, the Sum of Squares, Mean Square, F Test statistic, and p-value for each variable in the fitted linear model.
  }
  \item{model}{
    Matrix of the variables used in the model. First column corresponds to the response variable, y, followed by the explanatory variables, x, in the order that they were specified in the function.
  }
}

\author{
  Benjamin Smith
}

\examples{
## Assuming data = NULL
## Simple Linear Regression using vectors of values for y and x
y <- c(1,2,3,4,5)
x <- c(1.1, 1.9, 2.8, 4.1, 5.2)
mod.1 <- linear(y, x)

## View paremeter fits and ANOVA table from simple linear regression
mod.1$beta
mod.1$F.Test

## Multiple linear regression with x as a matrix of several variables
x.2 <- matrix(c(1.1, 1.9, 2.8, 4.1, 5.1, 1.9, 3.1, 3.9, 5.3, 6), 5, 2)
mod.2 <- linear(y, x.2)

## View parameter fits and ANOVA table from simple linear regression
mod.2$beta
mod.2$F.Test

## Simple Linear Regression using the swiss dataset (built into R)
## Predicting Infant.Mortality using Fertility as a sole predictor
mod.3 <- linear(y = "Infant.Mortality", x = "Fertility", data = swiss)
mod.3$beta

## Include Agriculture and Education as additional predictor variables
mod.4 <- linear(y = "Infant.Mortality", x = c("Fertility", "Agriculture", "Education"), data = swiss)
mod.4$beta

## Interaction terms are not natively supported by the linear() function.
## However, they may be passed to the linear() function as an additional variable from the dataset.
swiss.2 <- swiss  # Copy the dataset from Base R
swiss.2$Interaction <- swiss.2$Agriculture * swiss.2$Education  # Create the interaction variable

mod.5 <- linear(y = "Infant.Mortality", x = c("Fertility", "Agriculture", "Education", "Interaction"), data = swiss.2)
mod.5$beta

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
