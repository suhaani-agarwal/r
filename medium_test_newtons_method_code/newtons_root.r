
options(repos = c(CRAN = "https://cran.rstudio.com/"))

library(ggplot2)
library(animint2)
library(dplyr)

f <- function(x) 5*(x^3) - 7*(x^2) - 40*x + 100  # Example function
f_prime <- function(x) 15*(x^2) - 14*x - 40 # Derivative

# Initial guess
x0 <- 10
tolerance <- 1e-4
max_iterations <- 30

# Data frame to store iteration details
iterations_df <- tibble(
  iteration = numeric(0),
  x = numeric(0),
  f_x = numeric(0),
  tangent_intercept = numeric(0)
)

# Performing the Newton-Raphson method
for (i in 1:max_iterations) {
  f_x0 <- f(x0)
  f_prime_x0 <- f_prime(x0)
  
  # Calculating the next approximation
  x1 <- x0 - f_x0 / f_prime_x0
  
  # Storing the iteration details
  iterations_df <- bind_rows(iterations_df, tibble(
    iteration = i,
    x = x0,
    f_x = f_x0,
    tangent_intercept = x1
  ))
  
  # Checking for convergence
  if (abs(x1 - x0) < tolerance) {
    break
  }
  
  # Updating x0
  x0 <- x1
}

# Generating x values for plotting the function
x_vals <- seq(floor(min(iterations_df$x)), ceiling(max(iterations_df$x)), length.out = 500)
function_data_df <- tibble(x = x_vals, y = f(x_vals))

# Data frame for tangent lines
tangent_data_df <- iterations_df %>%
  mutate(
    slope = f_prime(x),
    intercept = f(x) - slope * x
  )

viz <- list(
  newtonRaphsonPlot = ggplot() +
    geom_hline(yintercept = 0, color = "gray", linetype = "dashed",
               help = "This line represents f(x) = 0, showing where we seek the root.") +
    
    geom_line(
      data = function_data_df, aes(x = x, y = y),
      color = "black", size = 1.2,
      help = "The black curve represents the function f(x), whose root we are approximating."
    ) +
    
    geom_abline(
      data = tangent_data_df, aes(slope = slope, intercept = intercept),
      color = "red", linetype = "dashed", alpha = 0.5,
      showSelected = "iteration",
      help = "The red dashed lines are the tangent lines at each iteration of the Newton-Raphson method."
    ) +
    
    geom_point(
      data = iterations_df, aes(x = x, y = f_x),
      color = "blue", size = 3, showSelected = "iteration",
      help = "The blue points indicate the successive approximations of the root."
    ) +
    
    geom_vline(
      data = iterations_df, aes(xintercept = tangent_intercept),
      color = "orange", size = 1, showSelected = "iteration",
      help = "The orange vertical lines mark the next x-values computed at each iteration."
    ) +
    
    geom_text(
      data = iterations_df, aes(
        x = tangent_intercept, y = 0,
        label = paste("x:", sprintf("%.4f", tangent_intercept))
      ),
      vjust = -1.5, hjust = 0.5, color = "black", size = 15,
      showSelected = "iteration",
      help = "The text annotations show the numerical value of x at each iteration."
    ) +
    
    labs(
      title = "Newton-Raphson Method - Root Finding",
      subtitle = "Approximating roots using tangent lines",
      x = "x",
      y = "f(x)"
    ) +
    
    scale_x_continuous(
      limits = c(floor(min(x_vals)), ceiling(max(x_vals))),
      breaks = pretty(x_vals, n = 10),
      expand = c(0.05, 0.05)
    ) +
    
    scale_y_continuous(
      limits = c(floor(min(function_data_df$y)), ceiling(max(function_data_df$y))),
      breaks = pretty(function_data_df$y, n = 10),
      expand = c(0.05, 0.05)
    ) ,

  time = list(variable = "iteration", ms = 1000)
)

# Save the animation to a directory
animint2dir(viz, out.dir = "newton_raphson_method")

# Printing the final root and function value
final_root <- iterations_df$tangent_intercept[nrow(iterations_df)]
cat("Approximated Root:", final_root, "\n")
cat("Function Value at Root:", f(final_root), "\n")