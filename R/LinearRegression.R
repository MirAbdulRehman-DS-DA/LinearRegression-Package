names <- c("Thor Wahlestedt", "Mir Abdul Rehman")
liuid <- c("thowa194", "abdmi452")

# Load required packages
library(ggplot2)

# Linear Regression Reference Class
# This class performs linear regression analysis and provides methods
# to access the results.
#@field coefficients Numeric vector of regression coefficients.
#@field fitted.values Numeric vector of fitted values.
#@field residuals Numeric vector of residuals.
#@field df.residual Degrees of freedom for residuals.
#@field residual.variance Variance of the residuals.
#@field var.coef Variance-covariance matrix of the coefficients.
#@field se.coef Standard errors of the coefficients.
#@field t.values T-values for each coefficient.
#@field p.values P-values for each coefficient.
linreg <- setRefClass(
  "linreg",
  fields = list(
    coefficients = "numeric",
    fitted.values = "numeric",
    residuals = "numeric",
    df.residual = "numeric",
    residual.variance = "numeric",
    var.coef = "matrix",
    se.coef = "numeric",
    t.values = "numeric",
    p.values = "numeric"
  ),
  methods = list(
    #@description Initialize the linreg object with a regression model.
    #@param formula A formula object specifying the model.
    #@param data A data frame containing the variables in the model.
    initialize = function(formula, data) {

      X <<- model.matrix(formula, data)
      y <<- data[[all.vars(formula)[1]]]

      coefficients <<- as.numeric(solve(t(X) %*% X) %*% (t(X) %*% y))
      fitted.values <<- as.numeric(X %*% coefficients)
      residuals <<- y - fitted.values

      n <- nrow(X)
      p <- ncol(X)
      df.residual <<- n - p

      residual.variance <<- sum(residuals^2) / df.residual
      var.coef <<- residual.variance * solve(t(X) %*% X)
      se.coef <<- sqrt(diag(var.coef))
      t.values <<- coefficients / se.coef
      p.values <<- 2 * (1 - pt(abs(t.values), df = df.residual))
    },

    #@description Print the coefficients and their names.
    print = function() {
      base::print(names(coefficients))
      base::print(coefficients)
    },

    #@description Plot diagnostic plots: residuals vs fitted values and histogram of residuals.
    plot = function() {
      # Residuals vs Fitted
      p1 <- ggplot(data = data.frame(Fitted = fitted.values, Residuals = residuals),
                   aes(x = Fitted, y = Residuals)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")

      # Histogram of Residuals
      p2 <- ggplot(data = data.frame(Residuals = residuals), aes(x = Residuals)) +
        geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
        geom_vline(aes(xintercept = median(Residuals)), color = "red", linetype = "dashed") +
        labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency")

      # Print plots
      print(p1)
      print(p2)
    },

    #@description Return the vector of residuals.
    #@return A numeric vector of residuals.
    resid = function() {
      return(residuals)
    },

    #@description Return the predicted values.
    #@return A numeric vector of predicted values.
    pred = function() {
      return(fitted.values)
    },

    #@description Return the coefficients as a named vector.
    #@return A named numeric vector of coefficients.
    coef = function() {
      return(coefficients)
    },

    #@description Return a summary of the regression model.
    #This method provides a summary similar to the lm class,
    #including coefficients, standard errors, t-values, p-values,
    #and model statistics.
    summary = function() {
      cat("Coefficients:\n")
      coefs <- data.frame(
        Estimate = coefficients,
        `Std. Error` = se.coef,
        `t value` = t.values,
        `Pr(>|t|)` = p.values,
        row.names = names(coefficients)
      )
      base::print(coefs)
      cat("\nResidual Variance:", residual.variance, "\n")
      cat("Degrees of Freedom:", df.residual, "\n")
      cat("Estimated σ (sigma hat):", sqrt(residual.variance), "\n")
    }
  )
)

# Example usage
# Assuming 'data' is your data frame and 'response ~ predictor1 + predictor2' is your formula
# model <- linreg$new(response ~ predictor1 + predictor2, data = your_data_frame)
# model$print()
# model$plot()
# model$resid()
# model$pred()
# model$coef()
# model$summary()

