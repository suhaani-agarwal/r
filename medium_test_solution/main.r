# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))
# Install required packages
install.packages("ggplot2")
install.packages("animint2")
install.packages("animation")

library(ggplot2)
library(animation)
library(animint2)

# Gradient descent function
gradient_descent <- function(f, grad, init, gamma, tol, max_iter) {
  x <- init[1]
  y <- init[2]
  trajectory <- data.frame(x = x, y = y, z = f(x, y))
  
  for (i in 1:max_iter) {
    # Compute gradient
    grad_x <- grad(x, y)[1]
    grad_y <- grad(x, y)[2]
    
    # Update x and y
    x_new <- x - gamma * grad_x
    y_new <- y - gamma * grad_y
    
    # Check for convergence
    if (abs(x_new - x) < tol && abs(y_new - y) < tol) {
      break
    }
    
    # Update values
    x <- x_new
    y <- y_new
    
    # Store trajectory
    trajectory <- rbind(trajectory, data.frame(x = x, y = y, z = f(x, y)))
  }
  
  return(trajectory)
}

# Define the objective function and its gradient
f <- function(x, y) x^2 + y^2
grad <- function(x, y) c(2 * x, 2 * y)

# Run gradient descent
grad_descent_data <- gradient_descent(
  f = f,
  grad = grad,
  init = c(-1.5, 1.5),  # Initial point
  gamma = 0.05,         # Learning rate
  tol = 0.001,          # Tolerance for convergence
  max_iter = 50         # Maximum number of iterations
)

# Create a grid for the contour plot
x <- seq(-2, 2, length.out = 100)  # Increasing sequence for x
y <- seq(-2, 2, length.out = 100)  # Increasing sequence for y
z <- outer(x, y, FUN = f)          # Compute z values
contour_data <- expand.grid(x = x, y = y)
contour_data$z <- as.vector(z)

# Extract gradient descent steps
arrow_data <- data.frame(
  x = grad_descent_data$x,
  y = grad_descent_data$y,
  xend = c(grad_descent_data$x[-1], NA),
  yend = c(grad_descent_data$y[-1], NA)
)

# Remove rows where xend is NA
arrow_data <- subset(arrow_data, !is.na(xend))
# Extract objective function values
objective_data <- data.frame(
  iteration = seq_along(grad_descent_data$z),
  value = grad_descent_data$z
)

contour_plot <- ggplot() +
  geom_contour(
    data = contour_data,
    aes(x = x, y = y, z = z),
    color = "black",
    bins = 20
  ) +
  geom_segment(
    data = arrow_data,
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.2, "cm")),
    color = "blue",
    size = 1
  ) +
  geom_point(
    data = arrow_data,
    aes(x = x, y = y),
    color = "red",
    size = 3
  ) +
  labs(
    title = "Gradient Descent Steps",
    x = "x",
    y = "y"
  ) +
  theme_minimal()

objective_plot <- ggplot(objective_data, aes(x = iteration, y = value)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3, showSelected = "iteration") +
  geom_tallrect(
    aes(xmin = iteration - 0.5, xmax = iteration + 0.5),
    alpha = 0.2,
    clickSelects = "iteration"
  ) +
  labs(
    title = "Objective Function Value Over Iterations",
    x = "Iteration",
    y = "Objective Value"
  ) +
  theme_minimal()

# Combine the plots into a list
plots <- list(
  contour = contour_plot,
  objective = objective_plot,
  time = list(variable = "iteration", ms = 200)
)

# Save the animation to a directory
animint2dir(plots, out.dir = "gradient_descent_animation")