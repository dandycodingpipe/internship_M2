# Load required library
library(dplyr)

# Read the CSV file
source <- read.csv("raw_data2.csv")

# Calculate mean for M1 to N5, grouped by X2 (timepoints) and X3 (AGUIX) and mutate the cortex and medulla
mean_source <- source %>%
  group_by(X2, X3, X4) %>%
  summarize(
    X1 = mean(X1, na.rm = TRUE),
    X11 = mean(X11, na.rm = TRUE),
    y = mean(y, na.rm = TRUE),
    y1 = mean(y1, na.rm = TRUE),
    M1 = mean(M1, na.rm = TRUE),
    M2 = mean(M2, na.rm = TRUE),
    M3 = mean(M3, na.rm = TRUE),
    M4 = mean(M4, na.rm = TRUE),
    M5 = mean(M5, na.rm = TRUE),
    N1 = mean(N1, na.rm = TRUE),
    N2 = mean(N2, na.rm = TRUE),
    N3 = mean(N3, na.rm = TRUE),
    N4 = mean(N4, na.rm = TRUE),
    N5 = mean(N5, na.rm = TRUE)
  )

# Average cortex and medulla, preserving timepoints
averaged_data <- mean_source %>%
  filter(X3 == "AGUIX") %>%
  mutate(
    segment = case_when(
      X4 %in% c("l cortex", "r cortex") ~ "cortex",
      X4 %in% c("l medulla", "r medulla") ~ "medulla",
      TRUE ~ X4
    )
  ) %>%
  group_by(X2,segment) %>%
  summarize(across(c(X1:N5), mean, na.rm = TRUE))

# Separate data frames for cortex, medulla, spleen, and liver
cortex <- averaged_data %>% filter(segment == "cortex")
medulla <- averaged_data %>% filter(segment == "medulla")
spleen <- averaged_data %>% filter(segment == "spleen")
liver <- averaged_data %>% filter(segment == "liver")

library(dplyr)
library(ggplot2)
library(ggpattern)
library(gridExtra)

# Filter data for relevant organs and time points
organ_data <- averaged_data %>%
  filter(segment %in% c("cortex", "medulla", "spleen", "liver"),
         X2 %in% c(1, 2, 3, 4, 5)) %>%
  drop_na()

# Define custom fills and patterns
fills <- c("#2a9df4","#d0efff" , "#1167b1", "#03254c")
patterns <- c("none", "none", "none", "none")  # Custom patterns for organs

data_X1 <- organ_data %>%
  select(X2, segment, X1, X11) %>%
  mutate(Region = factor(segment, levels = c("spleen", "liver","cortex", "medulla"))) %>%
  rename(value = X1, error = X11) %>%
  drop_na()

data_y <- organ_data %>%
  select(X2, segment, y, y1) %>%
  mutate(Region = factor(segment, levels = c("spleen", "liver","cortex", "medulla"))) %>%
  rename(value = y, error = y1) %>%
  drop_na()

# Debugging: print data to check for any issues
print(data_X1)
print(data_y)

plot_X1 <- ggplot(data_X1, aes(x = factor(X2), y = value, fill = Region, pattern = Region)) +
  geom_col_pattern(
    position = position_dodge2(width = 0.8, preserve = "single"), width = 0.8,
    pattern_fill = "white", pattern_color = NA, color = "black", size = 0.7  # Black border
  ) +
  geom_errorbar(aes(ymin = pmax(value - error,0), ymax = value + error),
                position = position_dodge2(width = 0.8, preserve = "single"), width = 0.8, size = 0.5) +
  labs(y = "Hounsfield Units", x = "Time (min)") +
  scale_fill_manual(values = fills) +
  scale_pattern_manual(values = patterns) +
  scale_x_discrete(labels = c("1" = "0.11","2" = "0.5", "3" = "1", "4" = "3", "5" = "10")) +
  scale_y_continuous(expand = expansion(mult = c(0.001, 0.05)), limits = c(0, NA)) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 30, margin = margin(r = 30), color = "black"),
    axis.title.x = element_text(size = 30, margin = margin(r = 30), color = "black"),
    axis.text = element_text(size = 24, color = "black"),
    axis.line = element_line(color = "black", size = 2),
    axis.text.x = element_text(size = 24, margin = margin(t = 10), color = "black"),
    panel.background = element_rect(fill = "white", color = NA),  # White background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "None",
    panel.border = element_blank()  # Remove panel border
    
  )

# Plot for y values
plot_y <- ggplot(data_y, aes(x = factor(X2), y = value, fill = Region, pattern = Region)) +
  geom_col_pattern(
    position = position_dodge2(width = 0.8, preserve = "single"), width = 0.8,
    pattern_fill = "white", pattern_color = NA, color = "black", size = 0.7  # Black border
  ) +
  geom_errorbar(aes(ymin = pmax(value - error,0), ymax = value + error),
                position = position_dodge2(width = 0.8, preserve = "single"), width = 0.8, size = 0.5) +
  labs(y = "[Gd] (mg/mL)", x = "Time (min)") +
  scale_fill_manual(values = fills) +
  scale_pattern_manual(values = patterns) +
  scale_x_discrete(labels = c("1" = "0.11","2" = "0.5", "3" = "1", "4" = "3", "5" = "10")) +
  scale_y_continuous(expand = expansion(mult = c(0.001, 0.05)), limits = c(0, NA)) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 30, margin = margin(r = 30), color = "black"),
    axis.title.x = element_text(size = 30, margin = margin(r = 30), color = "black"),
    axis.text = element_text(size = 24, color = "black"),
    axis.line = element_line(color = "black", size = 2),
    axis.text.x = element_text(size = 24, margin = margin(t = 10), color = "black"),
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


