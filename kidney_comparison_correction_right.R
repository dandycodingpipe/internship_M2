library(ggplot2)
library(dplyr)
library(tidyr)

# Define the corrected data without timepoint 1
time_points <- c(0.11, 0.5, 1, 3, 10)

# Right Cortex Data
Right_Cortex_data <- data.frame(
  Time = factor(time_points, levels = time_points),
  AGuIX = c(0.66319916,
            0.792184033,
            0.806397367,
            0.88836115,
            0.617782017
  ),
  Dotarem = c(0.253690995,
              0.864664098,
              0.670505579,
              0.338659516,
              0.070268034
  )
)

right_cortex_aguix_error <- c(0.233523453,
                              0.368114096,
                              0.357977912,
                              0.318598489,
                              0.472015344
)
right_cortex_dotarem_error <- c(0.666290501,
                                0.37588782,
                                0.297004923,
                                0.123411967,
                                0.079327684
)

# Right Medulla Data
Right_Medulla_data <- data.frame(
  Time = factor(time_points, levels = time_points),
  AGuIX = c(0.09899994,
            0.799509567,
            1.4743117,
            1.841082667,
            1.318230183
  ),
  Dotarem = c(-0.043306187,
              0.784826417,
              1.407206803,
              0.822386309,
              0.596176736
  )
)

right_medulla_aguix_error <- c(0.092490269,
                               0.493119386,
                               0.576624869,
                               0.624847362,
                               0.978550689
)
right_medulla_dotarem_error <- c(0.300871183,
                                 0.574242152,
                                 0.428026287,
                                 0.159262949,
                                 0.066558448
)

# Add an index to each row before reshaping
Right_Cortex_data <- Right_Cortex_data %>% mutate(Index = row_number())
Right_Medulla_data <- Right_Medulla_data %>% mutate(Index = row_number())

# Reshape data
Right_Cortex_long <- Right_Cortex_data %>%
  pivot_longer(cols = c(AGuIX, Dotarem), names_to = "Agent", values_to = "Signal") %>%
  mutate(
    Error = ifelse(Agent == "AGuIX", right_cortex_aguix_error[Index], right_cortex_dotarem_error[Index])
  ) %>%
  select(-Index)

Right_Medulla_long <- Right_Medulla_data %>%
  pivot_longer(cols = c(AGuIX, Dotarem), names_to = "Agent", values_to = "Signal") %>%
  mutate(
    Error = ifelse(Agent == "AGuIX", right_medulla_aguix_error[Index], right_medulla_dotarem_error[Index])
  ) %>%
  select(-Index)

# Combine datasets
combined_data <- bind_rows(Right_Cortex_long, Right_Medulla_long) %>%
  mutate(Location = rep(c("Right Cortex", "Right Medulla"), each = 10))

# Define font sizes
font_family <- "Free sans"
font_size_title <- 35
font_size_axis_title <- 30
font_size_text <- 25
font_size_facet <- 25

# Plot
gg <- ggplot(combined_data, aes(x = Time, y = Signal, fill = Agent)) +
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
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = function(x) c(0,  3)) +
  geom_vline(xintercept = c(2.5, 4.5), linetype = "dotted", color = "black") +
  geom_hline(yintercept = 0.2, linetype = "dotted", color = "black", size = 1.5)

print(gg)

ggsave("output_plot.png", plot = gg, width = 9000, height = 2500, units = "px")
