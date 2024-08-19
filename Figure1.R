first_pass <- mean_data %>%
  filter(X4 %in% "aorta") %>%
  filter(X2 %in% 1)

# Create data frames for each plot
data_X1 <- first_pass %>%
  select(X3, mean_X1, sd_X1) %>%
  rename(value = mean_X1, error = sd_X1)

data_y <- first_pass %>%
  select(X3, mean_y, sd_y) %>%
  rename(value = mean_y, error = sd_y)

# Define custom fills for textures
fills <- c("lightblue", "gold")

# Function to create patterns based on treatment (X3)
pattern_function <- function(trt) {
  if (trt == "AGUIX") {
    return("none")  # No pattern for AGUIX
  } else if (trt == "GBCA") {
    return("none")
  } else {
    return("none")
  }
}
library(ggplot2)
library(ggpattern)
library(gridExtra)

# Assuming data_X1, data_y, fills, and pattern_function are defined as before
# Function for text formatting with margin adjustment
# Function for text formatting with margin adjustment
text_format <- function(size, margin_left = 0, margin_right = 0) {
  element_text(size = size, color = "black", face = "bold", margin = margin(t = 0, r = margin_right, b = 0, l = margin_left))
}

# Adjust plot_X1
plot_X1 <- ggplot(data_X1, aes(x = X3, y = value, fill = X3, pattern = X3)) +
  geom_col_pattern(
    position = position_dodge(width = 0.5), width = 0.4,
    pattern_fill = "white",  # Set pattern fill color (background color)
    pattern_color = NA  # Set pattern color to NA for no outline
  ) +
  geom_col(
    position = position_dodge(width = 0.5), width = 0.4, color = "black", fill = NA,  # Narrower bars
    size = 1  # Increase bar outline thickness
  ) +
  geom_errorbar(aes(ymin = value - error, ymax = value + error), 
                position = position_dodge(width = 0.5), width = 0.2, size = 1) +  # Increase error bar thickness
  labs(y = "Hounsfield Units (HU)", x = NULL) +
  scale_fill_manual(values = fills) +
  scale_pattern_manual(values = c("AGUIX" = "none", "GBCA" = "none")) +  # Adjust scale_pattern_manual
  scale_y_continuous(expand = expansion(mult = c(0.001, 0.1))) +  # Adjust space between axis line and labels
  theme_minimal() +
  theme(
    axis.title.y = text_format(30, margin_right = 30),  # Displace y-axis title to the right
    axis.text = text_format(24),  # Larger axis text
    axis.line = element_line(color = "black", size = 2),  # Thicker axis lines
    axis.text.x = element_text(margin = margin(t = 10)),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank(),  # Remove panel border
    legend.position = "none"
  )

# Adjust plot_y similarly
plot_y <- ggplot(data_y, aes(x = X3, y = value, fill = X3, pattern = X3)) +
  geom_col_pattern(
    position = position_dodge(width = 0.5), width = 0.4,
    pattern_fill = "white",  # Set pattern fill color (background color)
    pattern_color = NA  # Set pattern color to NA for no outline
  ) +
  geom_col(
    position = position_dodge(width = 0.5), width = 0.4, color = "black", fill = NA,  # Narrower bars
    size = 1  # Increase bar outline thickness
  ) +
  geom_errorbar(aes(ymin = value - error, ymax = value + error), 
                position = position_dodge(width = 0.5), width = 0.2, size = 1) +  # Increase error bar thickness
  labs(y = "[Gd] (mg/mL)", x = NULL) +
  scale_fill_manual(values = fills) +
  scale_pattern_manual(values = c("AGUIX" = "none", "GBCA" = "none")) +  # Adjust scale_pattern_manual
  scale_y_continuous(expand = expansion(mult = c(0.001, 0.1))) +  # Adjust space between axis line and labels
  theme_minimal() +
  theme(
    axis.title.y = text_format(30, margin_right = 30),  # Displace y-axis title to the right
    axis.text = text_format(24),  # Larger axis text
    axis.line = element_line(color = "black", size = 2),  # Thicker axis lines
    axis.text.x = element_text(margin = margin(t = 10)),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank(),  # Remove panel border
    legend.position = "none"
  )

# Combine the two plots
combined_plot <- grid.arrange(plot_X1, plot_y, ncol = 2)

# Save the combined plot to a file with larger dimensions
ggsave("combined_plot.png", plot = combined_plot, width = 7250, height = 2000, units = "px", dpi = 300)