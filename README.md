# bensmyth.625HW3
Submission for BIOSTAT 625 Homework 3

This package seeks to reimplement the command for linear models in base R (lm) as a new command linear.
* linear() runs a linear regression model given the specified response variable and predictor variables.

## Installation
Using the package `devtools`, the package may be installed directly from github in the following manner:

```{r}
install.packages("devtools")
devtools::install_github("bensmyth0/bensmyth.625HW3")
```
## Usage
```{r}
# Using the built-in dataset swiss

head(swiss)
#>              Fertility Agriculture Examination Education Catholic Infant.Mortality
#> Courtelary        80.2        17.0          15        12     9.96             22.2
#> Delemont          83.1        45.1           6         9    84.84             22.2
#> Franches-Mnt      92.5        39.7           5         5    93.40             20.2
#> Moutier           85.8        36.5          12         7    33.77             20.3
#> Neuveville        76.9        43.5          17        15     5.16             20.6
#> Porrentruy        76.1        35.3           9         7    90.57             26.6

# Predicting Infant Mortality based on Fertility

model1 <- linear(y = "Infant.Mortality", x = "Fertility", data = swiss)
model1$beta
#>      Intercept  Fertility
#> [1,]   13.1297 0.09712863
model1$F.Test
#>           DF    Sum Sq   Mean Sq  F Value     p-value
#> Fertility  1  67.71661 67.716611 9.447708 0.003585238
#> Residuals 45 322.53828  7.167517       NA          NA

# We can also run a multiple regression model with additional predictors Agriculture and Education
model2 <- linear(y = "Infant.Mortality", x = c("Fertility", "Agriculture", "Education"), data = swiss)
model2$beta
#>      Intercept Fertility Agriculture Education
#> [1,]  10.14163 0.1420803 -0.01754503 0.0659342
model2$F.Test
#>             DF     Sum Sq   Mean Sq   F Value     p-value
#> Fertility    1  67.716611 67.716611 9.8243896 0.003099901
#> Agriculture  1  19.276707 19.276707 2.7966829 0.101721627
#> Education    1   6.875299  6.875299 0.9974749 0.323509615
#> Residuals   43 296.386276  6.892704        NA          NA

# In the absence of a dataset, it will instead interpret the y and x inputs as the data itself

model3 <- linear(y = testY, x = testX)
model3$beta
#>      Intercept        x1
#> [1,] 0.1206454 0.9534287
model3$F.Test
#>           DF     Sum Sq    Mean Sq  F Value      p-value
#> x1         1 9.91565823 9.91565823 352.6957 0.0003295758
#> Residuals  3 0.08434177 0.02811392       NA           NA

```
