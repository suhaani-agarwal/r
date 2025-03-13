# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Load required libraries
library(ggplot2)
library(animint2)
library(dplyr)

# Define a more complex test function
f <- function(x) x^4 - 3 * x^3 + 2 * x^2 - 5 * x + 1

# Initial interval [a, b]
a <- -2
b <- 4
tolerance <- 1e-4
max_iterations <- 30

# Data frame to store iteration details
iterations_df <- tibble(
  iteration = numeric(0),
  a = numeric(0),
  b = numeric(0),
  c = numeric(0),
  f_c = numeric(0)
)

# Perform the bisection method
for (i in 1:max_iterations) {
  c <- (a + b) / 2
  f_a <- f(a)
  f_c <- f(c)

  iterations_df <- bind_rows(iterations_df, tibble(
    iteration = i,
    a = a,
    b = b,
    c = c,
    f_c = f_c
  ))

  if (abs(f_c) < tolerance || (b - a) / 2 < tolerance) {
    break
  }

  if (sign(f_a) * sign(f_c) < 0) {
    b <- c
  } else {
    a <- c
  }
}

# Generate x values for plotting the function
x_vals <- seq(floor(min(iterations_df$a)), ceiling(max(iterations_df$b)), length.out = 500)
function_data_df <- tibble(x = x_vals, y = f(x_vals))

# Data frame for midpoints
midpoints_df <- iterations_df %>%
  mutate(y = 0)

# Create the interactive visualization
viz <- list(
  bisectionMethodPlot = ggplot() +
    geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
    geom_line(
      data = function_data_df, aes(x = x, y = y),
      color = "black", size = 1.2
    ) +
    geom_vline(
      data = iterations_df, aes(xintercept = a),
      linetype = "dashed", color = "red", alpha = 0.5,
      showSelected = "iteration"
    ) +
    geom_vline(
      data = iterations_df, aes(xintercept = b),
      linetype = "dashed", color = "red", alpha = 0.5,
      showSelected = "iteration"
    ) +
    geom_point(
      data = midpoints_df, aes(x = c, y = 0),
      color = "blue", size = 3, showSelected = "iteration"
    ) +
    geom_vline(
      data = iterations_df, aes(xintercept = c),
      color = "blue", size = 1, showSelected = "iteration"
    ) +
    geom_text(
      data = iterations_df, aes(
        x = c, y = 0,
        label = paste("c:", sprintf("%.4f", c))
      ),
      vjust = -1.5, hjust = 0.5, color = "blue", size = 15,
      showSelected = "iteration"
    ) +
    labs(
      title = "Bisection Method - Root Finding",
      subtitle = "Solving f(x) = x⁴ - 3x³ + 2x² - 5x + 1",
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

  # Add a tour guide
  tour = list(
    list(
      element = "bisectionMethodPlot",
      text = "This is the graph of the function f(x) = x⁴ - 3x³ + 2x² - 5x + 1. The red dashed lines represent the interval [a, b], and the blue line represents the midpoint c."
    ),
    list(
      element = "bisectionMethodPlot",
      text = "As the iterations progress, the interval [a, b] shrinks, and the midpoint c gets closer to the root of the function."
    ),
    list(
      element = "bisectionMethodPlot",
      text = "The blue point and vertical line represent the current midpoint c. The label shows the value of c at each iteration."
    )
  ),

  # Animation settings
  time = list(variable = "iteration", ms = 1000)
)

# Save the animation to a directory
animint2dir(viz, out.dir = "bisection_method")

# Print the final root and function value
final_root <- iterations_df$c[nrow(iterations_df)]
cat("Approximated Root:", final_root, "\n")
cat("Function Value at Root:", f(final_root), "\n")