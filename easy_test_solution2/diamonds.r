# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Load required libraries
library(ggplot2)
library(animint2)

# Load the diamonds dataset
data("diamonds")

# Define consistent colors for diamond cuts
cut.colors <- c(
  Fair = "#E41A1C", 
  Good = "#377EB8", 
  `Very Good` = "#4DAF4A", 
  Premium = "#984EA3", 
  Ideal = "#FF7F00"
)

# Create the scatterplot
scatter <- ggplot() +
  scale_color_manual(values = cut.colors) +
  geom_point(
    mapping = aes(
      x = carat, 
      y = price, 
      color = cut
    ),
    data = diamonds
  ) +
  labs(
    x = "Carat",
    y = "Price",
    title = "Carat vs. Price of Diamonds"
  )

# Create the histogram for carat with consistent colors
histCarat <- ggplot() +
  scale_fill_manual(values = cut.colors) +  # Ensure matching colors
  geom_histogram(
    mapping = aes(
      x = carat,
      fill = cut
    ),
    data = diamonds,
    bins = 30
  ) +
  labs(
    x = "Carat",
    y = "Count",
    title = "Distribution of Carat"
  )

# Create the histogram for price with consistent colors
histPrice <- ggplot() +
  scale_fill_manual(values = cut.colors) +  # Ensure matching colors
  geom_histogram(
    mapping = aes(
      x = price,
      fill = cut
    ),
    data = diamonds,
    bins = 30
  ) +
  labs(
    x = "Price",
    y = "Count",
    title = "Distribution of Price"
  )

# Combine the plots into an interactive visualization
viz <- animint(
  scatter = scatter,
  histCarat = histCarat,
  histPrice = histPrice,
  out.dir = "diamonds_visualization",
  title = "Interactive Visualization of Diamonds Dataset"
)

# Render and view the visualization
viz
