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
generate_sample_means <- function(dist_type = "norm", sample_size, n_samples) {
  # Generate sample means based on the specified distribution
  if (dist_type == "exp") {
    sample_means <- sapply(1:n_samples, function(i) {
      mean(rexp(sample_size, rate = 1))
    })
  } else if (dist_type == "unif") {
    sample_means <- sapply(1:n_samples, function(i) {
      mean(runif(sample_size))
    })
  } else {
    sample_means <- sapply(1:n_samples, function(i) {
      mean(rnorm(sample_size, mean = 0, sd = 1))
    })
  }
  
  return(sample_means)
}

calculate_p_value <- function(sample_means) {
  test_result <- shapiro.test(sample_means)
  return(test_result$p.value)
}

# Generate data for all sample sizes
sample_sizes <- seq(5, n_max, by = interval_step)
dist_type <- "norm"  # Using normal distribution


# Define parameters for normal distribution
true_mean <- 0
true_sd <- 1

density_df <- data.frame()
p_values_df <- data.frame()

# Generate data for each sample size
for (n in sample_sizes) {
  sample_means <- generate_sample_means(dist_type, n, n_samples)
  p_value <- calculate_p_value(sample_means)
  
  p_values_df <- rbind(p_values_df, data.frame(n = n, p_value = p_value))
  
  temp_df <- data.frame(n = n, mean = sample_means, fill_var = "Sample Means")
  density_df <- rbind(density_df, temp_df)
}

normal_curve_df <- data.frame()
for (n in sample_sizes) {
  se <- true_sd / sqrt(n)
  x_range <- seq(
    min(density_df$mean[density_df$n == n]),
    max(density_df$mean[density_df$n == n]),
    length.out = 200
  )
  y_values <- dnorm(x_range, mean = true_mean, sd = se)
  temp_df <- data.frame(
    n = n,
    x = x_range,
    y = y_values,
    color_var = "Normal Approx."
  )
  normal_curve_df <- rbind(normal_curve_df, temp_df)
}

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
  scale_fill_manual(values = c("Sample Means" = "steelblue")) +
  scale_color_manual(values = c("Normal Approx." = "red")) +
  labs(title = "Central Limit Theorem - Normal Distribution", x = "Sample Mean", y = "Density") +
  theme(legend.position = "bottom")+
  coord_cartesian(xlim = c(-1, x_range + 0.5))

p_value_plot <- ggplot(p_values_df, aes(x = n, y = p_value)) +  
  geom_line(color = "red", size = 1) +
  geom_point(size = 2, color = "blue", clickSelects = "n") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "black", size = 1) +  
  geom_text(
    aes(x = max(n) + 30, y = 0.05 + 0.02,  # Position above the line
        label = "p > 0.05"),
    size = 10, 
    color = "black",
    fontface = "bold",
    hjust = 1  # Align text to the right
  ) +
  # Add text below the line (p < 0.05)
  geom_text(
    aes(x = max(n) + 30, y = 0.05 - 0.03,  # Position below the line
        label = "p < 0.05"),
    size = 10, 
    color = "black",
    fontface = "bold",
    hjust = 1  # Align text to the right
  ) +
  geom_tallrect(aes(xmin = n - interval_step/2, xmax = n + interval_step/2),
                clickSelects = "n",
                alpha = 0.2) +
  labs(title = "Shapiro-Wilk Test P-values", x = "Sample Size (n)", y = "P-Value") +
  geom_text(
    data = p_values_df,
    aes(x = n, y = p_value + 0.02,  
        label = paste("p-val=", round(p_value, 4))),
    size = 15, 
    color = "darkblue",
    fontface = "bold",
    hjust = 0,  # Slightly shift text to the right for visibility
    showSelected = "n"
  ) +
  coord_cartesian(xlim = c(0, n_max + 40))


clt_viz <- list(density = density_plot, pvalues = p_value_plot, time = list(variable = "n", ms = 1000))

animint2dir(clt_viz, out.dir = "clt_animation")
