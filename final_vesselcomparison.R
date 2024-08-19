library(ggplot2)
library(dplyr)
library(tidyr)

# Define the custom transformation function for the new scale with larger break spacing
trans <- function(x) {
  low_break <- 1
  high_break <- 4
  lower_scale <- 8  # Further increase the visual space for the lower range
  compression_factor <- 0.05  # Compress values between 1.75 and 4 more tightly
  break_space <- 2  # Additional space for the break
  continuation_point <- lower_scale + break_space + (high_break - low_break) * compression_factor  # Adjust for the continuation
  
  ifelse(x <= low_break,
         (x / low_break) * lower_scale,  # Scale normally up to low_break, expanding its visual space
         ifelse(x <= high_break,
                lower_scale + break_space + (x - low_break) * compression_factor,  # Compress between low_break and high_break
                continuation_point + (x - high_break)))  # Continue normally above high_break
}

# Define your time points
time_points <- c(0.11, 0.5, 1, 3, 10)

# Integrate error values directly into the IVC data frame
Aorta_data <- data.frame(
  Time = factor(time_points, levels = time_points),
  AGuIX_Signal = c(5.548945201,
                   0.652886655,
                   0.559673933,
                   0.361039895,
                   0.202980942
                   
  ),
  AGuIX_Error = c(2.829433055,
                  0.165313597,
                  0.174700487,
                  0.114263344,
                  0.112892213
                  
                  
                  
                  
  ),
  Dotarem_Signal = c(5.282955373,
                     0.31028977,
                     0.339183584,
                     0.109310959,
                     -0.051615153
                     
                     
                     
                     
  ),
  Dotarem_Error = c(2.139503369,
                    0.230013523,
                    0.226093185,
                    0.179134654,
                    0.149362489
                    
                    
                    
                    
                    
  ),
  Sig1 = c( 0.2802, 
            0.01074, 0.02637, 0.02916, 0.02726)
)




# Integrate error values directly into the Aorta data frame
IVC_data <- data.frame(
  Time = factor(time_points, levels = time_points),
  AGuIX_Signal = c(
    0.270504064,
    0.649750898,
    0.551024334,
    0.400949266,
    0.201428167
    
    
    
    
  ),
  AGuIX_Error = c(
    0.158932649,
    0.161287274,
    0.204228597,
    0.134932782,
    0.120971906
    
  ),
  Dotarem_Signal = c(
    -0.042488332,
    0.3559385,
    0.302371488,
    0.186987547,
    0.033254641
    
  ),
  Dotarem_Error = c(
    0.228135204,
    0.181330086,
    0.168189553,
    0.178972259,
    0.124047507
    
  )
  ,
  Sig2 = c( 0.9875, 
            0.007302, 0.0387, 0.02073, 0.06486)
)



# Reshape the Aorta data into a long format for plotting
long_data_aorta <- Aorta_data %>%
  pivot_longer(cols = starts_with("AGuIX_"), names_to = c("Agent", ".value"), names_pattern = "(.*)_(.*)") %>%
  bind_rows(
    Aorta_data %>%
      pivot_longer(cols = starts_with("Dotarem_"), names_to = c("Agent", ".value"), names_pattern = "(.*)_(.*)")
  ) %>%
  mutate(
    min = Signal - Error,
    max = Signal + Error,
    sd = Error,
    AAP = Time,
    Location = "Abdominal aorta"
  )

# Reshape the IVC data into a long format for plotting
long_data_ivc <- IVC_data %>%
  pivot_longer(cols = starts_with("AGuIX_"), names_to = c("Agent", ".value"), names_pattern = "(.*)_(.*)") %>%
  bind_rows(
    IVC_data %>%
      pivot_longer(cols = starts_with("Dotarem_"), names_to = c("Agent", ".value"), names_pattern = "(.*)_(.*)")
  ) %>%
  mutate(
    min = Signal - Error,
    max = Signal + Error,
    sd = Error,
    AAP = Time,
    Location = "Inferior vena cava"
  )

# Combine both datasets
combined_data <- bind_rows(long_data_aorta, long_data_ivc)

# Apply the transformation to your combined data
combined_data <- combined_data %>%
  mutate(
    mean_t = trans(Signal),
    sd_up_t = trans(Signal + Error),
    sd_low_t = trans(min)
  )
  
  # Define the breakpoints and transformation limits for visualization
  low_break <- 1
  high_break <- 4
  lower_scale <- 12
  compression_factor <- 0.05
  break_space <- 1
  continuation_point <- lower_scale + break_space + (high_break - low_break) * compression_factor
  
  # Define the y-ticks for better visibility
  yticks <- c(0, 0.2, 0.4, 0.6, 4, 6, 8)
  transformed_yticks <- trans(yticks)
  
  # Create the combined plot with the adjusted transformation
  gg <- ggplot(data = combined_data, aes(x = AAP, y = mean_t, group = Agent, fill = Agent)) +
    geom_errorbar(aes(ymin = pmax(sd_low_t, 0), ymax = sd_up_t), position = position_dodge(width = 0.7), width = 0.2) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = trans(low_break), ymax = trans(low_break) + break_space), fill = "white") +
    scale_fill_manual(values = c("AGuIX" = "lightblue", "Dotarem" = "gold")) +
    scale_y_continuous(limits = c(0, 17), breaks = transformed_yticks, labels = yticks) +
    labs(y = "[Gd] mg/mL", x = "Time (min)") +
    theme_minimal() +
    facet_wrap(~Location, scales = "free_y") +
    theme(
      text = element_text(family = "Helvetica"),
      plot.title = element_text(size = 45, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 35),
      axis.title.y = element_text(size = 35, vjust = 1.0),
      axis.text = element_text(size = 35, face = "bold"),
      axis.text.x = element_text(vjust = 1),
      axis.text.y = element_text(hjust = 0.15),
      legend.title = element_blank(),
      legend.text = element_text(size = 35),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 45, face = "bold"),
      axis.line.y = element_line(color = "black", size = 1),
      axis.ticks.y = element_line(color = "black"),
      axis.ticks.length.y = unit(0.3, "cm"),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      strip.background = element_blank(),
      strip.placement = "outside"
    ) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +
    geom_hline(yintercept = trans(0.2), linetype = "dotted", color = "black", size = 1.5)+
    geom_vline(xintercept = c(2.5, 4.5), linetype = "dotted", color = "black") 
    #geom_text(data = filter(combined_data, Location == "Inferior vena cava"), aes(x = Inf, y = Inf, label = "Global p < 0.01**"), vjust = 1.45, hjust = 2.1, color = "red", size = 12) +
    #geom_text(data = filter(combined_data, Location == "Abdominal aorta"), aes(x = Inf, y = Inf, label = "Global p < 0.001***"), vjust = 1.45, hjust = 2.1, color = "red", size = 12) 
  
  print(gg)
  
  ggsave("output_plot.png", plot = gg, width = 9000, height = 2500, units = "px")
