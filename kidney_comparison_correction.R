<<<<<<< HEAD
library(ggplot2)
library(dplyr)
library(tidyr)

# Define the corrected data without timepoint 1
time_points <- c(0.11, 0.5, 1, 3, 10)

# Left Cortex Data
Left_Cortex_data <- data.frame(
  Time = factor(time_points, levels = time_points),
  AGuIX = c(0.86936692,
            0.912187167,
            0.898268667,
            1.0927848,
            0.85624795
  ),
  Dotarem = c(0.19163794,
              0.509623251,
              0.709636628,
              0.464234582,
              0.168374808
  )
)

left_cortex_aguix_error <- c(0.302536129,
                             0.505737857,
                             0.315585301,
                             0.344668928,
                             0.477905776
)
left_cortex_dotarem_error <- c(1.058221973,
                               0.82760845,
                               0.339280716,
                               0.173873897,
                               0.083399462
)

# Left Medulla Data
Left_Medulla_data <- data.frame(
  Time = factor(time_points, levels = time_points),
  AGuIX = c(0.26647186,
            0.901580012,
            1.643271833,
            2.22928,
            1.792824433
  ),
  Dotarem = c(0.011172301,
              0.601878747,
              1.420305233,
              0.898271725,
              0.571721262
  )
)

left_medulla_aguix_error <- c(0.16721566,
                              0.747545311,
                              0.324227098,
                              0.473546056,
                              0.763614168
)
left_medulla_dotarem_error <- c(0.212532326,
                                0.774829515,
                                0.358336606,
                                0.143986256,
                                0.207530372
)

# Add an index to each row before reshaping
Left_Cortex_data <- Left_Cortex_data %>% mutate(Index = row_number())
Left_Medulla_data <- Left_Medulla_data %>% mutate(Index = row_number())

# Reshape data
Left_Cortex_long <- Left_Cortex_data %>%
  pivot_longer(cols = c(AGuIX, Dotarem), names_to = "Agent", values_to = "Signal") %>%
  mutate(
    Error = ifelse(Agent == "AGuIX", left_cortex_aguix_error[Index], left_cortex_dotarem_error[Index])
  ) %>%
  select(-Index)

Left_Medulla_long <- Left_Medulla_data %>%
  pivot_longer(cols = c(AGuIX, Dotarem), names_to = "Agent", values_to = "Signal") %>%
  mutate(
    Error = ifelse(Agent == "AGuIX", left_medulla_aguix_error[Index], left_medulla_dotarem_error[Index])
  ) %>%
  select(-Index)

# Combine datasets
combined_data_left <- bind_rows(Left_Cortex_long, Left_Medulla_long) %>%
  mutate(Location = rep(c("Left Cortex", "Left Medulla"), each = 10))

# Define font sizes
font_family <- "Arial"
font_size_title <- 35
font_size_axis_title <- 30
font_size_text <- 25
font_size_facet <- 25

# Plot
gg_left <- ggplot(combined_data_left, aes(x = Time, y = Signal, fill = Agent)) +
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
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = function(x) c(0, 3)) +
  geom_vline(xintercept = c(2.5, 4.5), linetype = "dotted", color = "black") +
  geom_hline(yintercept = 0.2, linetype = "dotted", color = "black", size = 1.5)

print(gg_left)

ggsave("output_plot_left.png", plot = gg_left, width = 9000, height = 2500, units = "px")
=======
library(ggplot2)
library(dplyr)
library(tidyr)

# Define the corrected data without timepoint 1
time_points <- c(0.11, 0.5, 1, 3, 10)

# Left Cortex Data
Left_Cortex_data <- data.frame(
  Time = factor(time_points, levels = time_points),
  AGuIX = c(0.86936692,
            0.912187167,
            0.898268667,
            1.0927848,
            0.85624795
  ),
  Dotarem = c(0.19163794,
              0.509623251,
              0.709636628,
              0.464234582,
              0.168374808
  )
)

left_cortex_aguix_error <- c(0.302536129,
                             0.505737857,
                             0.315585301,
                             0.344668928,
                             0.477905776
)
left_cortex_dotarem_error <- c(1.058221973,
                               0.82760845,
                               0.339280716,
                               0.173873897,
                               0.083399462
)

# Left Medulla Data
Left_Medulla_data <- data.frame(
  Time = factor(time_points, levels = time_points),
  AGuIX = c(0.26647186,
            0.901580012,
            1.643271833,
            2.22928,
            1.792824433
  ),
  Dotarem = c(0.011172301,
              0.601878747,
              1.420305233,
              0.898271725,
              0.571721262
  )
)

left_medulla_aguix_error <- c(0.16721566,
                              0.747545311,
                              0.324227098,
                              0.473546056,
                              0.763614168
)
left_medulla_dotarem_error <- c(0.212532326,
                                0.774829515,
                                0.358336606,
                                0.143986256,
                                0.207530372
)

# Add an index to each row before reshaping
Left_Cortex_data <- Left_Cortex_data %>% mutate(Index = row_number())
Left_Medulla_data <- Left_Medulla_data %>% mutate(Index = row_number())

# Reshape data
Left_Cortex_long <- Left_Cortex_data %>%
  pivot_longer(cols = c(AGuIX, Dotarem), names_to = "Agent", values_to = "Signal") %>%
  mutate(
    Error = ifelse(Agent == "AGuIX", left_cortex_aguix_error[Index], left_cortex_dotarem_error[Index])
  ) %>%
  select(-Index)

Left_Medulla_long <- Left_Medulla_data %>%
  pivot_longer(cols = c(AGuIX, Dotarem), names_to = "Agent", values_to = "Signal") %>%
  mutate(
    Error = ifelse(Agent == "AGuIX", left_medulla_aguix_error[Index], left_medulla_dotarem_error[Index])
  ) %>%
  select(-Index)

# Combine datasets
combined_data_left <- bind_rows(Left_Cortex_long, Left_Medulla_long) %>%
  mutate(Location = rep(c("Left Cortex", "Left Medulla"), each = 10))

# Define font sizes
font_family <- "Arial"
font_size_title <- 35
font_size_axis_title <- 30
font_size_text <- 25
font_size_facet <- 25

# Plot
gg_left <- ggplot(combined_data_left, aes(x = Time, y = Signal, fill = Agent)) +
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
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = function(x) c(0, 3)) +
  geom_vline(xintercept = c(2.5, 4.5), linetype = "dotted", color = "black") +
  geom_hline(yintercept = 0.2, linetype = "dotted", color = "black", size = 1.5)

print(gg_left)

ggsave("output_plot_left.png", plot = gg_left, width = 9000, height = 2500, units = "px")
>>>>>>> 77a3982bf0902d3f28a8584daad61788e60cc341
