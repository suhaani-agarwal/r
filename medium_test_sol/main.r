# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))
# # Install required packages
# install.packages("ggplot2")
# install.packages("animint2")
# install.packages("animation")

# library(ggplot2)
# library(animint2)
# library(dplyr)

# # Step 1: Define parameters
# set.seed(123)  # For reproducibility
# nmax <- 150  # Maximum sample size
# num_obs <- 1000  # Number of observations per sample

# # Step 2: Simulate data from an arbitrary non-normal distribution (e.g., exponential)
# clt_data <- expand.grid(n = 1:nmax, sample_id = 1:num_obs) %>%
#   group_by(n) %>%
#   mutate(sample = rexp(num_obs, rate = 1),  # Exponential distribution
#          mean = cumsum(sample) / seq_along(sample)) %>%
#   ungroup()

# # Step 3: Compute theoretical normal distribution for each n
# normal_density <- lapply(1:nmax, function(n) {
#   mean_theoretical <- 1  # Mean of the exponential distribution
#   sd_theoretical <- 1 / sqrt(n)  # Standard deviation of the sample mean
#   x_vals <- seq(0.5, 1.5, length.out = 200)  # Zoomed-in x-axis range
#   data.frame(
#     n = n,
#     x = x_vals,
#     y = dnorm(x_vals, mean = mean_theoretical, sd = sd_theoretical)
#   )
# }) %>% bind_rows()

# # Step 4: Perform Shapiro-Wilk normality test for each n
# p_values <- clt_data %>%
#   group_by(n) %>%
#   summarise(p_value = shapiro.test(mean)$p.value)

# # Step 5: Create density plot animation (zoomed-in x-axis)
# density_plot <- ggplot() +
#   geom_density(
#     data = clt_data,
#     aes(x = mean, group = n),
#     fill = "blue",
#     alpha = 0.5,
#     showSelected = "n"
#   ) +
#   geom_line(
#     data = normal_density,
#     aes(x = x, y = y, group = n),
#     linetype = "dashed",
#     color = "red",
#     showSelected = "n"
#   ) +
#   labs(
#     title = "Central Limit Theorem Animation",
#     x = "Sample Mean",
#     y = "Density"
#   ) +
#   coord_cartesian(xlim = c(0.5, 1.5))  # Zoom in on the x-axis

# # Step 6: Create p-value vs sample size plot
# pvalue_plot <- ggplot(p_values, aes(x = n, y = p_value)) +
#   geom_line(color = "green", size = 1) +
#   geom_point(size = 2, color = "blue", clickSelects = "n") +
#   geom_tallrect(aes(xmin = n - 0.5, xmax = n + 0.5), alpha = 0.2, clickSelects = "n") +
#   labs(
#     title = "Shapiro-Wilk Test P-values",
#     x = "Sample Size (n)",
#     y = "P-Value"
#   )

# # Step 7: Compile the plots into a list for animint2
# plots <- list(
#   density = density_plot,
#   pvalue = pvalue_plot,
#   time = list(variable = "n", ms = 200)
# )

# # Step 8: Save animation to directory
# animint2dir(plots, out.dir = "clt_animation")


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

# Update the density plot to improve axis visibility
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
  scale_x_continuous(breaks = seq(-1, 3, by = 0.5)) +  # Set x-axis breaks
  scale_y_continuous(breaks = seq(0, 2, by = 0.5)) +  # Set y-axis breaks
  theme_minimal(base_size = 14) +  # Increase base font size
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(size = 16, face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(size = 16, face = "bold"),  # Bold y-axis title
    axis.text.x = element_text(size = 14),  # Larger x-axis labels
    axis.text.y = element_text(size = 14)   # Larger y-axis labels
  ) +
  coord_cartesian(xlim = c(-1, 3))  # Adjust x-axis range as needed

# Update the p-value plot for better axis visibility
p_value_plot <- ggplot(p_values_df, aes(x = n, y = p_value)) +
  geom_line(color = "green", size = 1) +
  geom_point(size = 2, color = "blue", clickSelects = "n") +
  geom_tallrect(aes(xmin = n - interval_step/2, xmax = n + interval_step/2),
                clickSelects = "n",
                alpha = 0.2) +
  labs(
    title = "Shapiro-Wilk Test P-values",
    x = "Sample Size (n)",
    y = "P-Value"
  ) +
  scale_x_continuous(breaks = seq(0, n_max, by = 25)) +  # Set x-axis breaks
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +  # Set y-axis breaks
  theme_minimal(base_size = 14) +  # Increase base font size
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(size = 16, face = "bold"),  # Bold y-axis title
    axis.text.x = element_text(size = 14),  # Larger x-axis labels
    axis.text.y = element_text(size = 14)   # Larger y-axis labels
  )

# Create interactive animint visualization
clt_viz <- list(
  density = density_plot,
  pvalues = p_value_plot,
  time = list(variable = "n", ms = 1000)
)

# Save animation to directory
animint2dir(clt_viz, out.dir = "clt_animation")
