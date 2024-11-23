# Load necessary libraries
#' @import methods
#' @import ggplot2
#' @importFrom ggplot2 ggplot aes geom_point
#' @importFrom gridExtra grid.arrange
#' @importFrom methods setClass setMethod new
#' @title Linear Regression using Ordinary Least Squares (OLS)
#'
#' @field formula The formula object defining the regression model.
#' @field data A data frame containing the variables for the regression.
#' @field X Design matrix (model matrix).
#' @field y Response variable.
#' @field beta_hat Coefficients estimated by OLS.
#' @field y_hat Fitted values.
#' @field residuals Residuals (differences between observed and fitted values).
#' @field sigma_hat Residual standard error.
#' @field t_values T-statistics for the coefficients.
#' @field p_values P-values associated with each coefficient
#' @export
#' @export linreg
linreg <- setRefClass(
  "linreg",

  fields = list(
    formula = "formula",
    data = "data.frame",
    X = "matrix",
    y = "numeric",
    beta_hat = "numeric",
    y_hat = "numeric",
    residuals = "numeric",
    sigma_hat = "numeric",
    t_values = "numeric",
    p_values = "numeric",
    data_name = "character"
  ),

  methods = list(

    # Constructor method for initialization
    initialize = function(formula, data) {
      if (!inherits(formula, "formula") || !is.data.frame(data)) {
        stop("Invalid input: formula must be a formula and data must be a data frame.")
      }

      formula <<- formula
      data <<- data
      data_name <<- deparse(substitute(data))
      X <<- model.matrix(formula, data)
      y <<- data[[all.vars(formula)[1]]]
      fit_model()
    },

    # Method to fit the model
    fit_model = function() {
      # Calculate beta coefficients using OLS
      beta_hat_matrix <- solve(t(X) %*% X) %*% t(X) %*% y
      beta_hat <<- as.numeric(beta_hat_matrix)

      # Fitted values
      y_hat_matrix <- X %*% beta_hat_matrix
      y_hat <<- as.numeric(y_hat_matrix)

      # Residuals
      residuals_matrix <- y - y_hat_matrix
      residuals <<- as.numeric(residuals_matrix)

      # Degrees of freedom
      df <- nrow(X) - ncol(X)

      # Residual variance
      sigma_hat <<- sum(residuals^2) / df

      # Variance of beta coefficients
      var_beta_hat <- sigma_hat * solve(t(X) %*% X)

      # T-values and p-values for coefficients
      t_values <<- beta_hat / sqrt(diag(var_beta_hat))
      p_values <<- 2 * pt(-abs(t_values), df)
    },

    # Method to print the coefficients
    print = function() {
      # Print the formula and dataset used
      cat("linreg(formula = ", deparse(formula), ", data = ", data_name, ")\n", sep = "")

      # Print coefficients names in a single line
      cat("\nCoefficients:\n")
      coef_names <- colnames(X)
      coef_names[1] <- "(Intercept)"

      # Print all names on the same line, separated by spaces
      cat(paste(coef_names, collapse = " "), "\n")
    },

    # Method to return coefficients as a named vector
    coef = function() {
      coef_names <- colnames(X)
      coef_names[1] <- "(Intercept)"
      return(setNames(beta_hat, coef_names))
    },

    # Method to return residuals
    resid = function() {
      return(residuals)
    },

    # Method to return fitted (predicted) values
    pred = function() {
      return(y_hat)
    },

    # Method to plot residuals vs fitted values and scale-location plot
    plot = function() {
      # Residuals vs Fitted plot
      plot_data <- data.frame(Fitted = y_hat, Residuals = residuals)
      p1 <- ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
        theme_minimal()

      # Scale-Location plot
      scale_loc <- sqrt(abs(residuals))
      plot_data$ScaleLocation <- scale_loc
      p2 <- ggplot(plot_data, aes(x = Fitted, y = ScaleLocation)) +
        geom_point() +
        geom_smooth(se = FALSE, linetype = "dashed", color = "blue") +
        labs(title = "Scale-Location Plot", x = "Fitted Values", y = "Sqrt(|Residuals|)") +
        theme_minimal()

      # Print both plots
      grid.arrange(p1, p2, ncol=1)
    },

    # Method to show summary (similar to summary.lm())
    summary = function() {
      cat("Summary of Linear Regression Model:\n\n")

      # Significance stars
      stars <- sapply(p_values, function(p) {
        if (p < 0.001) return("***")
        else if (p < 0.01) return("**")
        else if (p < 0.05) return("*")
        else return("")
      })

      # Manually format the summary output to match test expectations
      coef_names <- colnames(X)
      coef_names[1] <- "(Intercept)"

      cat(sprintf("%-15s %12s %12s %12s %10s\n", "Estimate", "Std. Error", "t value", "Pr(>|t|)", "Signif"))
      for (i in 1:length(beta_hat)) {
        cat(sprintf("%-15s %12.4f %12.4f %12.4f %10s\n",
                    coef_names[i],
                    beta_hat[i],
                    sqrt(diag(sigma_hat * solve(t(X) %*% X)))[i],
                    t_values[i],
                    stars[i]))
      }

      # Degrees of freedom and residual standard error
      df <- nrow(X) - ncol(X)
      cat("\nResidual standard error:", round(sqrt(sigma_hat), 3), "on", df, "degrees of freedom\n")
    }
  )
)
