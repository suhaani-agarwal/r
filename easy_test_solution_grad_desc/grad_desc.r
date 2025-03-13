# Load required libraries
library(ggplot2)
library(animint2)
library(dplyr)

# Define the objective function and its gradient
f <- function(x, y) x^2 + 2 * y^2  # Objective function
grad <- function(x, y) c(2 * x, 4 * y)  # Gradient of the function

# Gradient descent function
gradient_descent <- function(f, grad, init, gamma, tol, max_iter) {
  x <- init[1]
  y <- init[2]
  trajectory <- data.frame(x = x, y = y, z = f(x, y), iteration = 1)
  
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
    trajectory <- rbind(trajectory, data.frame(x = x, y = y, z = f(x, y), iteration = i + 1))
  }
  
  return(trajectory)
}

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
x_vals <- seq(-2, 2, length.out = 100)  # Increasing sequence for x
y_vals <- seq(-2, 2, length.out = 100)  # Increasing sequence for y
z_vals <- outer(x_vals, y_vals, FUN = f)  # Compute z values over the grid
contour_data <- expand.grid(x = x_vals, y = y_vals)
contour_data$z <- as.vector(z_vals)  # Flatten z_vals to match the grid

# Prepare the animation
viz.one <- ggplot() +
  geom_contour(
    data = contour_data, aes(x = x, y = y, z = z, color = ..level..),
    bins = 20, size = 0.5
  ) +
  geom_path(
    data = grad_descent_data, aes(x = x, y = y, group = 1),
    color = "red", size = 1, arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_point(
    data = grad_descent_data, aes(x = x, y = y, color = iteration),
    size = 3
  ) +
  labs(
    title = "Gradient Descent Steps",
    x = "x",
    y = "y"
  ) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal()

# Add interactivity to the main plot
viz.one <- viz.one +
  animint2::clickSelects("iteration") +  # Use `clickSelects` as a parameter
  animint2::showSelected("iteration")   # Use `showSelected` as a parameter

# Create the objective function plot
viz.two <- ggplot(grad_descent_data, aes(x = iteration, y = z)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  geom_tallrect(
    aes(xmin = iteration - 0.5, xmax = iteration + 0.5),
    alpha = 0.2
  ) +
  labs(
    title = "Objective Function Value Over Iterations",
    x = "Iteration",
    y = "Objective Value"
  ) +
  theme_minimal()

# Add interactivity to the objective function plot
viz.two <- viz.two +
  animint2::clickSelects("iteration") +  # Use `clickSelects` as a parameter
  animint2::showSelected("iteration")   # Use `showSelected` as a parameter

# Combine the plots into an interactive visualization
viz <- list(
  contour = viz.one,
  objective = viz.two,
  time = list(variable = "iteration", ms = 1000)  # Animate over iterations
)

# Save the animation to a directory
animint2dir(viz, out.dir = "gradient_descent_animation")