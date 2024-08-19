library(dplyr)
library(ggplot2)
library(ggpattern)
library(gridExtra)

mean_data <- read.csv("averaged_data2.csv", fileEncoding = "ISO-8859-1")

# Filter and average left and right pelvis into one pelvis group
averaged_data <- mean_data %>%
  mutate(
    segment = case_when(
      X4 %in% c("l pelvis", "r pelvis") ~ "pelvis",
      TRUE ~ X4
    )
  ) %>%
  group_by(X2, X3, segment) %>%
  summarize(across(c(X1:N5), mean, na.rm = TRUE), .groups = 'drop')

averaged_data <- averaged_data %>%
  filter(segment %in% "pelvis")

# Define custom fills and patterns
fills <- c("lightblue", "gold")
patterns <- c("none", "none")  # none for aorta, stripe for vena cava

# Create data frames for X1 and y
data_X1 <- averaged_data %>%
  filter(X3 %in% c("AGuIX® ", "Dotarem® ")) %>%
  select(X2, segment, X3, X1, X11) %>%
  mutate(X3 = factor(X3, levels = c("AGuIX® ", "Dotarem® ")),
         segment = factor(segment)) %>%
  rename(value = X1, error = X11) %>%
  drop_na()

data_y <- averaged_data %>%
  filter(X3 %in% c("AGuIX® ", "Dotarem® ")) %>%
  select(X2, segment, X3, y, y1) %>%
  mutate(Contrast = factor(X3, levels = c("AGuIX® ", "Dotarem® ")),
         Region = factor(segment)) %>%
  rename(value = y, error = y1) %>%
  drop_na()

# Debugging: print data to check for any issues
print(data_X1)
print(data_y)

# Plot for X1 values
plot_X1 <- ggplot(data_X1, aes(x = factor(X2), y = value, fill = X3)) +
  geom_col_pattern(
    position = position_dodge2(width = 0.8, preserve = "single"), width = 0.8,
    pattern_fill = "white", pattern_color = NA, color = "black", size = 0.7  # Black border
  ) +
  geom_errorbar(aes(ymin = value - error, ymax = value + error),
                position = position_dodge2(width = 0.8, preserve = "single"), width = 0.8, size = 0.5) +
  labs(y = "Hounsfield Units (HU)", x = "Time (min)") +
  scale_fill_manual(values = fills, guide = "none") +  # Remove legend for fills
  scale_pattern_manual(values = patterns) +
  scale_x_discrete(labels = c("1" = "0.11","2" = "0.5", "3" = "1", "4" = "3", "5" = "10")) +
  scale_y_continuous(expand = expansion(mult = c(0.001, 0.05)), limits = c(0, NA)) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 40, margin = margin(r = 30), color = "black"),
    axis.title.x = element_text(size = 40, margin = margin(r = 30), color = "black"),
    axis.text = element_text(size = 30, color = "black"),
    axis.line = element_line(color = "black", size = 2),
    axis.text.x = element_text(size = 30, margin = margin(t = 10), color = "black"),
    panel.background = element_rect(fill = "white", color = NA),  # White background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank(),  # Remove panel border
    legend.position = "none",
    legend.text = element_text(size = 100),  # Adjust this for legend text size
    legend.title = element_text(size = 100),  # Adjust this for legend title size
    legend.key.size = unit(100, "lines"),  # Adjust this for the legend key size
    legend.spacing.y = unit(100, 'lines')
  )

# Plot for y values
plot_y <- ggplot(data_y, aes(x = factor(X2), y = value, fill = Contrast)) +
  geom_col_pattern(
    position = position_dodge2(width = 0.8, preserve = "single"), width = 0.8,
    pattern_fill = "white", pattern_color = NA, color = "black", size = 0.7  # Black border
  ) +
  geom_errorbar(aes(ymin = pmax(value - error,0), ymax = value + error),
                position = position_dodge2(width = 0.8, preserve = "single"), width = 0.8, size = 0.5) +
  labs(y = "[Gd] (mg/mL)", x = "Time (min)") +
  scale_fill_manual(values = fills, labels = c("AGuIX® ", "Dotarem® ")) +
  scale_pattern_manual(values = patterns, labels = c("pelvis" = "Pelvis")) +
  scale_x_discrete(labels = c("1" = "0.11","2" = "0.5", "3" = "1", "4" = "3", "5" = "10")) +
  scale_y_continuous(expand = expansion(mult = c(0.001, 0.05)), limits = c(0, NA)) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 40, margin = margin(r = 30), color = "black"),
    axis.title.x = element_text(size = 40, margin = margin(r = 30), color = "black"),
    axis.text = element_text(size = 30, color = "black"),
    axis.line = element_line(color = "black", size = 2),
    axis.text.x = element_text(size = 30, margin = margin(t = 10), color = "black"),
    panel.background = element_rect(fill = "white", color = NA),  # White background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank()  # Remove panel border
  )

# Combine the plots
combined_plot <- grid.arrange(plot_X1, plot_y, ncol = 2)

# Print the combined plot
print(combined_plot)

# Save the combined plot
ggsave("combined_plot.png", plot = combined_plot, width = 7250, height = 2000, units = "px", dpi = 300)
