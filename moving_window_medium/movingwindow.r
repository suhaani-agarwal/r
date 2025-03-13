# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))
#  Load required libraries
library(ggplot2)
library(animint2)

# Simulate Brownian motion
set.seed(123)
n <- 300  # Total number of points
brownian_motion <- cumsum(rnorm(n))
data <- data.frame(x = 1:n, y = brownian_motion)

# Define window size
window_size <- 50

# Create subsets of data for each iteration/frame
subsets <- lapply(1:(n - window_size + 1), function(i) {
  subset <- data[(i):(i + window_size - 1), ]
  subset$iteration <- i  # Add iteration number to each subset
  return(subset)
})

# Combine all subsets into a single data frame
animation_df <- do.call(rbind, subsets)

# Create the plot
p <- ggplot(animation_df, aes(x = x, y = y)) +
  # Use group and showSelected for animation
  geom_line(aes(group = iteration), 
            color = "blue", 
            size = 1,
            showSelected = "iteration") +
  geom_point(aes(group = iteration), 
             color = "red", 
             size = 3,
             showSelected = "iteration") +
  labs(
    title = "Brownian Motion - Moving Window",
    subtitle = paste("Window size:", window_size),
    x = "Time",
    y = "Value"
  ) +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90")
  ) +
  # Fix the x-axis range to always show window_size points
  scale_x_continuous(
    breaks = function(limits) pretty(limits, n = 10)
  )

# Create the animation
viz <- list(
  brownian = p,
  time = list(variable = "iteration", ms = 200)
)

# Save the animation
animint2dir(viz, out.dir = "moving_window_animation")

