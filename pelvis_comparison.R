library(ggplot2)
library(dplyr)
library(tidyr)

# Define the corrected data without timepoint 1
time_points <- c(0.11, 0.5, 1, 3, 10)

# Left Pelvis Data
Left_Pelvis_data <- data.frame(
  Time = factor(time_points, levels = time_points),
  AGuIX = c(0.018858551,
            0.401121825,
            1.609545433,
            3.745254,
            4.820205167
  ),
  Dotarem = c(-0.062537946,
              0.133881275,
              2.019058729,
              2.049127701,
              1.799733513
  )
)

left_pelvis_aguix_error <- c(0.018949941,
                             0.463628007,
                             1.1081571,
                             1.082643282,
                             2.686451762
)
left_pelvis_dotarem_error <- c(0.116849447,
                               0.902847745,
                               1.200840949,
                               0.484572563,
                               0.465550156
)

# Right Pelvis Data
Right_Pelvis_data <- data.frame(
  Time = factor(time_points, levels = time_points),
  AGuIX = c(0.008858551,
            0.301121825,
            1.509545433,
            3.645254,
            4.720205167
  ),
  Dotarem = c(-0.052537946,
              0.123881275,
              1.919058729,
              1.949127701,
              1.699733513
  )
)

right_pelvis_aguix_error <- c(0.018949941,
                              0.463628007,
                              1.1081571,
                              1.082643282,
                              2.686451762
)
right_pelvis_dotarem_error <- c(0.116849447,
                                0.902847745,
                                1.200840949,
                                0.484572563,
                                0.465550156
)

# Add an index to each row before reshaping
Left_Pelvis_data <- Left_Pelvis_data %>% mutate(Index = row_number())
Right_Pelvis_data <- Right_Pelvis_data %>% mutate(Index = row_number())

# Reshape data
Left_Pelvis_long <- Left_Pelvis_data %>%
  pivot_longer(cols = c(AGuIX, Dotarem), names_to = "Agent", values_to = "Signal") %>%
  mutate(
    Error = ifelse(Agent == "AGuIX", left_pelvis_aguix_error[Index], left_pelvis_dotarem_error[Index])
  ) %>%
  select(-Index)

Right_Pelvis_long <- Right_Pelvis_data %>%
  pivot_longer(cols = c(AGuIX, Dotarem), names_to = "Agent", values_to = "Signal") %>%
  mutate(
    Error = ifelse(Agent == "AGuIX", right_pelvis_aguix_error[Index], right_pelvis_dotarem_error[Index])
  ) %>%
  select(-Index)

# Combine datasets
combined_data_pelvis <- bind_rows(Left_Pelvis_long, Right_Pelvis_long) %>%
  mutate(Location = rep(c("Left Pelvis", "Right Pelvis"), each = 10))

# Define font sizes
font_family <- "Arial"
font_size_title <- 35
font_size_axis_title <- 30
font_size_text <- 25
font_size_facet <- 25

# Plot
gg_pelvis <- ggplot(combined_data_pelvis, aes(x = Time, y = Signal, fill = Agent)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = pmax(Signal - Error, 0.0), ymax = Signal + Error), width = 0.2, position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("AGuIX" = "lightblue", "Dotarem" = "gold")) +
  facet_wrap(~Location, scales = "free_y") +
  labs(x = "Time (min)", y = "[Gd] (mg/mL)") +
  theme_minimal() +
  theme(
    text = element_text(family = font_family),
    plot.title = element_text(size = font_size_title, face = "bold", hjust = 0.5),
    axis.title = element_text(size = font_size_axis_title),
    axis.title.y = element_text(size = font_size_axis_title, vjust = 1.0),
    axis.text = element_text(size = font_size_text, face = "bold"),
    axis.text.x = element_text(vjust = 0.15),
    axis.text.y = element_text(hjust = 0.15),
    legend.title = element_blank(),
    legend.text = element_text(size = 35),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = font_size_facet, face = "bold"),
    axis.line = element_line(color = "black", size = 1)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = function(x) c(0, 8)) +
  geom_vline(xintercept = c(2.5, 4.5), linetype = "dotted", color = "black") +
  geom_hline(yintercept = 0.2, linetype = "dotted", color = "black", size = 1.5)

print(gg_pelvis)

ggsave("output_plot_pelvis.png", plot = gg_pelvis, width = 9000, height = 2500, units = "px")
