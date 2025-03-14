# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

library(ggplot2)
library(animint2)
library(stats)

# Set parameters
set.seed(123)
n_max <- 150  # Maximum sample size
n_samples <- 1000  # Number of sample means to calculate for each n
interval_step <- 5  # Increment for sample size n

# Function to generate sample means for a given sample size
generate_sample_means <- function(dist_type = "exp", sample_size, n_samples) {
  # Generate sample means based on the specified distribution
  if (dist_type == "exp") {
    # Exponential distribution (rate=1)
    sample_means <- sapply(1:n_samples, function(i) {
      mean(rexp(sample_size, rate = 1))
    })
  } else if (dist_type == "unif") {
    # Uniform distribution
    sample_means <- sapply(1:n_samples, function(i) {
      mean(runif(sample_size))
    })
  } else {
    # Default to normal distribution
    sample_means <- sapply(1:n_samples, function(i) {
      mean(rnorm(sample_size))
    })
  }
  
  return(sample_means)
}

# Function to calculate p-value from Shapiro-Wilk test
calculate_p_value <- function(sample_means) {
  test_result <- shapiro.test(sample_means)
  return(test_result$p.value)
}

# Generate data for all sample sizes
sample_sizes <- seq(5, n_max, by = interval_step)
dist_type <- "exp"  # Using exponential distribution to show CLT effect

# Calculate theoretical parameters for the given distribution type
if (dist_type == "exp") {
  true_mean <- 1
  true_sd <- 1
} else if (dist_type == "unif") {
  true_mean <- 0.5
  true_sd <- 1/sqrt(12)
} else {  # normal
  true_mean <- 0
  true_sd <- 1
}

# Initialize dataframes
density_df <- data.frame()
p_values_df <- data.frame()

# Generate data for each sample size
for (n in sample_sizes) {
  # Generate sample means
  sample_means <- generate_sample_means(dist_type, n, n_samples)
  
  # Calculate p-value
  p_value <- calculate_p_value(sample_means)
  
  # Store p-value data
  p_values_df <- rbind(p_values_df, data.frame(
    n = n,
    p_value = p_value
  ))
  
  # Create dataframe for density plot
  temp_df <- data.frame(
    n = n,
    mean = sample_means,
    fill_var = "Sample Means"  # Add a variable for fill aesthetic
  )
  
  density_df <- rbind(density_df, temp_df)
}

# Calculate theoretical normal distribution for each n
normal_curve_df <- data.frame()
for (n in sample_sizes) {
  # Theoretical standard error for the sample mean
  se <- true_sd / sqrt(n)
  
  # Create x-values for the normal curve
  x_range <- seq(
    min(density_df$mean[density_df$n == n]),
    max(density_df$mean[density_df$n == n]),
    length.out = 200
  )
  
  # Calculate normal density
  y_values <- dnorm(x_range, mean = true_mean, sd = se)
  
  # Create dataframe for normal curve
  temp_df <- data.frame(
    n = n,
    x = x_range,
    y = y_values,
    color_var = "Normal Approx."  # Add a variable for color aesthetic
  )
  
  normal_curve_df <- rbind(normal_curve_df, temp_df)
}

# Update the density plot to include p-value annotation
density_plot <- ggplot() +
  geom_density(
    data = density_df,
    aes(x = mean, group = n, fill = fill_var),
    alpha = 0.5,
    showSelected = "n"
  ) +
  geom_line(
    data = normal_curve_df,
    aes(x = x, y = y, group = n, color = color_var),
    linetype = "dashed",
    size = 1,
    showSelected = "n"
  ) +
  scale_fill_manual(values = c("Sample Means" = "steelblue"), name = "") +
  scale_color_manual(values = c("Normal Approx." = "red"), name = "") +
  labs(
    title = "Central Limit Theorem Animation",
    x = "Sample Mean",
    y = "Density"
  ) +
  scale_x_continuous(breaks = seq(0, 2.5, by = 0.5)) +  # Set x-axis breaks
  scale_y_continuous(breaks = seq(0, max(y_values), by = 0.5)) +  # Set y-axis breaks  
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(size = 16, face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(size = 16, face = "bold"),  # Bold y-axis title
    axis.text.x = element_text(size = 14),  # Larger x-axis labels
    axis.text.y = element_text(size = 14)   # Larger y-axis labels
  ) +
  coord_cartesian(xlim = c(0, 2.5))   # Extend x-axis to make space for p-value
  

p_value_plot <- ggplot(p_values_df, aes(x = n, y = log10(p_value))) +  
  geom_line(color = "red", size = 1) +
  geom_point(size = 2, color = "blue", clickSelects = "n") +
  geom_tallrect(aes(xmin = n - interval_step/2, xmax = n + interval_step/2),
                clickSelects = "n",
                alpha = 0.2) +
  geom_hline(yintercept = log10(0.05), linetype = "dashed", color = "black", size = 1) +  
  geom_text(
    data = p_values_df,
    aes(x = n, y = log10(p_value) + 0.2,  
        label = paste("p-val=", round(p_value, 4))),
    size = 17, 
    color = "darkblue",
    fontface = "bold",
    hjust = 0,  # Slightly shift text to the right for visibility
    showSelected = "n"
  ) +
  labs(
    title = "Shapiro-Wilk Test P-values",
    x = "Sample Size (n)",
    y = "log10(P-Value)"
  ) +
  scale_x_continuous(breaks = seq(0, n_max, by = 25)) +  
  scale_y_continuous(breaks = seq(-100, 0, by = 1)) +  
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),  
    axis.title.y = element_text(size = 16, face = "bold"),  
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14)  
  )+
  coord_cartesian(xlim = c(0, n_max + 20))


# Create interactive animint visualization
clt_viz <- list(
  density = density_plot,
  pvalues = p_value_plot,
  time = list(variable = "n", ms = 1000)
)

# Save animation to directory
animint2dir(clt_viz, out.dir = "clt_animation_exponential_fn")
