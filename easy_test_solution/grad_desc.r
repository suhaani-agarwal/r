# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))
# Install required packages
# install.packages("ggplot2")
# install.packages("animint2")
# install.packages("animint")
# install.packages("animation")
# install.packages("dplyr")
library(ggplot2)
library(animint2)
library(dplyr)  # Ensure dplyr is loaded for data manipulation

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

# Define the objective function and its gradient
f <- function(x, y) x^2 + 2 * y^2  # Modified objective function
grad <- function(x, y) c(2 * x, 4 * y)  # Gradient of the modified function

# Run gradient descent
grad_descent_data <- gradient_descent(
  f = f,
  grad = grad,
  init = c(-1.5, 1.5),  # Initial point
  gamma = 0.05,         # Learning rate
  tol = 0.001,          # Tolerance for convergence
  max_iter = 50         # Maximum number of iterations
)


print(length(grad_descent_data$step))
print(head(grad_descent_data))

# Create a grid for the contour plot
x_vals <- seq(-2, 2, length.out = 100)  # Increasing sequence for x
y_vals <- seq(-2, 2, length.out = 100)  # Increasing sequence for y
z_vals <- outer(x_vals, y_vals, FUN = function(x, y) f(x, y))  # Compute z values over the grid
contour_data <- expand.grid(x = x_vals, y = y_vals)
contour_data$z <- as.vector(z_vals)  # Flatten z_vals to match the grid

# Ensure data is valid for gradient descent
grad_descent_data$step <- grad_descent_data$iteration  # Add step variable for time

# Prepare the animation
viz.one <- ggplot(grad_descent_data, aes(x = x, y = y, key = step)) +
  geom_point(aes(color = iteration), size = 4) +
  # geom_segment(aes(x = x, y = y, xend = lead(x, default = first(x)), yend = lead(y, default = first(y))), 
  #              arrow = arrow(length = unit(0.2, "cm")), color = "blue", size = 1) +
  # geom_contour(data = contour_data, aes(x = x, y = y, z = z), color = "black", bins = 20) +
  labs(title = "Gradient Descent Steps", x = "x", y = "y") +
  theme_bw() +
  scale_color_gradient(low = "red", high = "blue") +
  theme(legend.position = "none")

viz.two <- ggplot(grad_descent_data, aes(x = iteration, y = z, key = step)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  geom_tallrect(aes(xmin = iteration - 0.5, xmax = iteration + 0.5), alpha = 0.2) +
  labs(title = "Objective Function Value Over Iterations", x = "Iteration", y = "Objective Value") +
  theme_bw()

viz.three <- ggplot(grad_descent_data, aes(x = step, y = z, key = step)) +
  geom_line(color = "red") +
  geom_tallrect(aes(xmin = step - 0.5, xmax = step + 0.5, ymin = 0, ymax = z), alpha = 0.2) +
  labs(title = "Objective Function Value Over Steps", x = "Iteration", y = "Objective Value") +
  theme_bw()

# Combine the plots into an interactive visualization with named list
viz.publish <- animint2dir(
  list(
    "GradientDescentSteps" = viz.one,
    "ObjectiveFunctionValueOverIterations" = viz.two,
    "ObjectiveFunctionValueOverSteps" = viz.three
  ),
  out.dir = "./output"
)

# Return the animation object
viz.publish
