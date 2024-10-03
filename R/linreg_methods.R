# Print method for linreg objects
# print.linreg <- function(model) {
#   cat("Linear Regression Model:\n\n")
#   cat("Coefficients:\n")
#   print(model$beta_hat)
# }

# Plot method for linreg objects
# plot.linreg <- function(model) {
#   library(ggplot2)
#
#   plot_data <- data.frame(Fitted = model$y_hat, Residuals = model$residuals)

#   ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
#     geom_point() +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
#     labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
#     theme_minimal()
# }

# Summary method for linreg objects
# summary.linreg <- function(model) {
#   cat("Summary of Linear Regression Model:\n\n")

#   summary_table <- data.frame(
#     Estimate = as.numeric(model$beta_hat),
#     `Std. Error` = sqrt(diag(model$var_beta_hat)),
#     `t value` = model$t_values,
#     `Pr(>|t|)` = model$p_values
#   )

#   print(summary_table)

#   cat("\nResidual standard error:", sqrt(model$sigma_hat), "\n")
#   cat("Degrees of freedom:", model$df, "\n")
# }
