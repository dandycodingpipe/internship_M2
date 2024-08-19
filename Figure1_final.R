library(tidyverse)
library(ggplot2)
library(gridExtra)

# Read the data
source <- read.csv("averaged_data2.csv", fileEncoding = "ISO-8859-1")

# Filter the data for 'aorta' and 'X2' equal to 1
filtered_data <- source %>%
  filter(X4 == "aorta" & X2 == 1)

# Check the filtered data
print(filtered_data)
fills <- c("lightblue", "gold")

# Define text format function
text_format <- function(size, margin_left = 0, margin_right = 0) {
  element_text(size = size, color = "black", face = "bold", margin = margin(t = 0, r = margin_right, b = 0, l = margin_left))
}

# Create the first boxplot with M values
plot1 <- ggplot(filtered_data, aes(x = X3, fill = X3)) +
  geom_boxplot(
    aes(
      ymin = M2,
      lower = M4,
      middle = M1,
      upper = M5,
      ymax = M3
    ),
    stat = "identity",
    size = 2,
    fill = fills
  ) +
  ylim(c(-1000, 1000)) +  # Manually set y-axis limits for plot1
  labs(y = "Hounsfield Units (HU)",
       x = NULL) +  # Remove x-axis title
  theme_minimal() +
  theme(
    axis.title.y = text_format(40, margin_right = 30),
    axis.text = text_format(30),
    axis.line = element_line(color = "black", size = 2),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = "none",
    axis.ticks = element_line(color = "black", size = 1),  # Add ticks on the y-axis
    axis.ticks.length = unit(0.3, "cm")
  )

# Create the second boxplot with N values
plot2 <- ggplot(filtered_data, aes(x = X3, fill = X3)) +
  geom_boxplot(
    aes(
      ymin = N2,
      lower = N4,
      middle = N1,
      upper = N5,
      ymax = N3
    ),
    stat = "identity",
    size = 2,
    fill = fills
  ) +
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, by = 5)) +  # Adjust y-axis range and increments
  labs(
       y = "[Gd] mg/mL",
       x = NULL) +  # Remove x-axis title
  theme_minimal() +
  theme(
    axis.title.y = text_format(40, margin_right = 30),
    axis.text = text_format(30),
    axis.line = element_line(color = "black", size = 2),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = "none",
    axis.ticks = element_line(color = "black", size = 1),  # Add ticks on the y-axis
    axis.ticks.length = unit(0.3, "cm")
  )

# Arrange the plots side by side
combined_plot <- grid.arrange(plot1, plot2, ncol = 2)

# Save the combined plot to a file with larger dimensions
ggsave("combined_plot.png", plot = combined_plot, width = 7250, height = 3000, units = "px", dpi = 300)

