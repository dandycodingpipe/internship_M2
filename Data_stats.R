<<<<<<< HEAD
mean_data <- read.csv("averaged_data2.csv", fileEncoding = "ISO-8859-1")
# Average cortex and medulla, preserving timepoints
mean_data1 <- mean_data %>%
  filter(X3 == "AGUIX") %>%
  mutate(
    X4 = case_when(
      X4 %in% c("l cortex", "r cortex") ~ "cortex",
      X4 %in% c("l medulla", "r medulla") ~ "medulla",
      X4 %in% c("l pelvis", "r pelvis") ~ "pelvis",
      TRUE ~ X4
    )
  ) %>%
  group_by(X2,X4) %>%
  summarize(across(c(X1:N5), mean, na.rm = TRUE)) %>%
  mutate(X3 = "AGUIX")

mean_data2 <- mean_data %>%
  filter(X3 == "GBCA") %>%
  mutate(
    X4 = case_when(
      X4 %in% c("l cortex", "r cortex") ~ "cortex",
      X4 %in% c("l medulla", "r medulla") ~ "medulla",
      X4 %in% c("l pelvis", "r pelvis") ~ "pelvis",
      TRUE ~ X4
    )
  ) %>%
  group_by(X2,X4) %>%
  summarize(across(c(X1:N5), mean, na.rm = TRUE)) %>%
  mutate(X3 = "GBCA")

# Combine both data frames
mean_data <- bind_rows(mean_data1, mean_data2)

# Curve stats 

stats <- data.frame(
  region = character(),
  statistic = numeric(),
  p_value = numeric(),
  mean_diff = numeric(),
  stringsAsFactors = FALSE
)

organ = "pelvis"
#####

# Filter data for the specified organ
dotarem <- mean_data %>%
  filter(X3 %in% "GBCA") %>%
  filter(X4 %in% organ)

aguix <- mean_data %>%
  filter(X3 %in% "AGUIX") %>%
  filter(X4 %in% organ)

# Perform the Wilcoxon test
wilcox <- wilcox.test(aguix$N1, dotarem$N1, paired = TRUE)

# Calculate mean difference
mean_diff <- mean(aguix$N1) - mean(dotarem$N1)

# Append the results to the data frame
stats <- rbind(stats, data.frame(
  region = organ,
  statistic = wilcox$statistic,
  p_value = wilcox$p.value,
  mean_diff = mean_diff
))

# Print the updated data frame
print(stats)



#Individual Stats





#####
comparable_data <- read.csv("imputated_data2.csv", fileEncoding = "ISO-8859-1")
# Process data for AGUIX
comparable_data1 <- comparable_data %>%
  filter(X3 == "AGuIX® ") %>%
  mutate(
    X4 = case_when(
      X4 %in% c("l cortex", "r cortex") ~ "cortex",
      X4 %in% c("l medulla", "r medulla") ~ "medulla",
      X4 %in% c("l pelvis", "r pelvis") ~ "pelvis",
      TRUE ~ X4
    )
  ) %>%
  group_by(X2, X4,X5) %>%
  summarize(across(c(X1:M5, y:N5), mean, na.rm = TRUE)) %>%
  mutate(X3 = "AGuIX® ")

# Process data for GBCA
comparable_data2 <- comparable_data %>%
  filter(X3 == "Dotarem® ") %>%
  mutate(
    X4 = case_when(
      X4 %in% c("l cortex", "r cortex") ~ "cortex",
      X4 %in% c("l medulla", "r medulla") ~ "medulla",
      X4 %in% c("l pelvis", "r pelvis") ~ "pelvis",
      TRUE ~ X4
    )
  ) %>%
  group_by(X2, X4, X5) %>%
  summarize(across(c(X1:M5, y:N5), mean, na.rm = TRUE)) %>%
  mutate(X3 = "Dotarem® ")

# Combine both data frames
comparable_data <- bind_rows(comparable_data1, comparable_data2)

stats2 <- data.frame(
  timepoint = numeric(),
  region = character(),
  t_statistic = numeric(),
  t_p_value = numeric(),
  mean_diff = numeric(),
  shapiro_w_statistic_aguix = numeric(),
  shapiro_w_p_value_aguix = numeric(),
  shapiro_w_statistic_dotarem = numeric(),
  shapiro_w_p_value_dotarem = numeric(),
  t_degrees_freedom = numeric(),  # Adding degrees of freedom column
  stringsAsFactors = FALSE
)

organ = "pelvis"
t = 4
  #####
# Filter data for the specified organ and timepoint
dotarem <- comparable_data %>%
  filter(X3 == "Dotarem® ", X4 == organ, X2 == t)
  #filter(X3 == "GBCA", X4 == "vena cava", X2 == t)

aguix <- comparable_data %>%
  filter(X3 == "AGuIX® ", X4 == organ, X2 == t)

# Find common X5 values between dotarem and aguix
common_X5 <- intersect(dotarem$X5, aguix$X5)

# Filter dotarem and aguix to only include common X5 values
dotarem <- dotarem %>% filter(X5 %in% common_X5)
aguix <- aguix %>% filter(X5 %in% common_X5)
print(mean(aguix$X1 - aguix$M1))
print(mean(aguix$y - aguix$N1))
print(mean(dotarem$X1 - dotarem$M1))
print(mean(dotarem$y - dotarem$N1))
print(mean(aguix$X1 - dotarem$X1))
print( mean(aguix$X11 - dotarem$X11))
mean(aguix$y - dotarem$y)
mean(aguix$y1 - dotarem$y1)

# Perform the paired t-test
ttest <- t.test(aguix$N1, dotarem$N1, paired = TRUE)

# Calculate mean difference
mean_diff <- mean(aguix$N1) - mean(dotarem$N1)

# Perform Shapiro-Wilk test for normality
shapiro_aguix <- shapiro.test(aguix$N1)
shapiro_dotarem <- shapiro.test(dotarem$N1)

# Append the results to the data frame
stats2 <- rbind(stats2, data.frame(
  timepoint = t,
  region = organ,
  t_statistic = ttest$statistic,
  t_p_value = ttest$p.value,
  mean_diff = mean_diff,
  shapiro_w_statistic_aguix = shapiro_aguix$statistic,
  shapiro_w_p_value_aguix = shapiro_aguix$p.value,
  shapiro_w_statistic_dotarem = shapiro_dotarem$statistic,
  shapiro_w_p_value_dotarem = shapiro_dotarem$p.value,
  t_degrees_freedom = ttest$parameter  # Add degrees of freedom
))








#####
comparable_data <- read.csv("imputated_data2.csv", fileEncoding = "ISO-8859-1")
stats2 <- data.frame(
  timepoint = numeric(),
  region = character(),
  t_statistic = numeric(),
  t_p_value = numeric(),
  mean_diff = numeric(),
  shapiro_w_statistic_aguix = numeric(),
  shapiro_w_p_value_aguix = numeric(),
  shapiro_w_statistic_dotarem = numeric(),
  shapiro_w_p_value_dotarem = numeric(),
  t_degrees_freedom = numeric(),  # Adding degrees of freedom column
  stringsAsFactors = FALSE
)


t = 5
organ = "aguix_pelvis"
#####
# Filter data for the specified organ and timepoint
dotarem <- comparable_data %>%
  filter(X3 == "AGuIX® ", X4 == "r pelvis", X2 == t)

aguix <- comparable_data %>%
  filter(X3 == "AGuIX® ", X4 == "l pelvis", X2 == t)

# Find common X5 values between dotarem and aguix
common_X5 <- intersect(dotarem$X5, aguix$X5)

# Filter dotarem and aguix to only include common X5 values
dotarem <- dotarem %>% filter(X5 %in% common_X5)
aguix <- aguix %>% filter(X5 %in% common_X5)

# Perform the paired t-test
ttest <- t.test(aguix$X1, dotarem$X1, paired = TRUE)

# Calculate mean difference
mean_diff <- mean(aguix$X1) - mean(dotarem$X1)

# Perform Shapiro-Wilk test for normality
shapiro_aguix <- shapiro.test(aguix$X1)
shapiro_dotarem <- shapiro.test(dotarem$X1)

# Append the results to the data frame
stats2 <- rbind(stats2, data.frame(
  timepoint = t,
  region = organ,
  t_statistic = ttest$statistic,
  t_p_value = ttest$p.value,
  mean_diff = mean_diff,
  shapiro_w_statistic_left = shapiro_aguix$statistic,
  shapiro_w_p_value_left= shapiro_aguix$p.value,
  shapiro_w_statistic_right= shapiro_dotarem$statistic,
  shapiro_w_p_value_right= shapiro_dotarem$p.value,
  t_degrees_freedom = ttest$parameter  # Add degrees of freedom
))

# Print the updated data frame
print(stats2)



stats2_first_half <- stats2[1:10, ]
stats2_second_half <- stats2[(10 + 1):20, ]
=======
mean_data <- read.csv("averaged_data2.csv", fileEncoding = "ISO-8859-1")
# Average cortex and medulla, preserving timepoints
mean_data1 <- mean_data %>%
  filter(X3 == "AGUIX") %>%
  mutate(
    X4 = case_when(
      X4 %in% c("l cortex", "r cortex") ~ "cortex",
      X4 %in% c("l medulla", "r medulla") ~ "medulla",
      X4 %in% c("l pelvis", "r pelvis") ~ "pelvis",
      TRUE ~ X4
    )
  ) %>%
  group_by(X2,X4) %>%
  summarize(across(c(X1:N5), mean, na.rm = TRUE)) %>%
  mutate(X3 = "AGUIX")

mean_data2 <- mean_data %>%
  filter(X3 == "GBCA") %>%
  mutate(
    X4 = case_when(
      X4 %in% c("l cortex", "r cortex") ~ "cortex",
      X4 %in% c("l medulla", "r medulla") ~ "medulla",
      X4 %in% c("l pelvis", "r pelvis") ~ "pelvis",
      TRUE ~ X4
    )
  ) %>%
  group_by(X2,X4) %>%
  summarize(across(c(X1:N5), mean, na.rm = TRUE)) %>%
  mutate(X3 = "GBCA")

# Combine both data frames
mean_data <- bind_rows(mean_data1, mean_data2)

# Curve stats 

stats <- data.frame(
  region = character(),
  statistic = numeric(),
  p_value = numeric(),
  mean_diff = numeric(),
  stringsAsFactors = FALSE
)

organ = "pelvis"
#####

# Filter data for the specified organ
dotarem <- mean_data %>%
  filter(X3 %in% "GBCA") %>%
  filter(X4 %in% organ)

aguix <- mean_data %>%
  filter(X3 %in% "AGUIX") %>%
  filter(X4 %in% organ)

# Perform the Wilcoxon test
wilcox <- wilcox.test(aguix$N1, dotarem$N1, paired = TRUE)

# Calculate mean difference
mean_diff <- mean(aguix$N1) - mean(dotarem$N1)

# Append the results to the data frame
stats <- rbind(stats, data.frame(
  region = organ,
  statistic = wilcox$statistic,
  p_value = wilcox$p.value,
  mean_diff = mean_diff
))

# Print the updated data frame
print(stats)



#Individual Stats





#####
comparable_data <- read.csv("imputated_data2.csv", fileEncoding = "ISO-8859-1")
# Process data for AGUIX
comparable_data1 <- comparable_data %>%
  filter(X3 == "AGuIX® ") %>%
  mutate(
    X4 = case_when(
      X4 %in% c("l cortex", "r cortex") ~ "cortex",
      X4 %in% c("l medulla", "r medulla") ~ "medulla",
      X4 %in% c("l pelvis", "r pelvis") ~ "pelvis",
      TRUE ~ X4
    )
  ) %>%
  group_by(X2, X4,X5) %>%
  summarize(across(c(X1:M5, y:N5), mean, na.rm = TRUE)) %>%
  mutate(X3 = "AGuIX® ")

# Process data for GBCA
comparable_data2 <- comparable_data %>%
  filter(X3 == "Dotarem® ") %>%
  mutate(
    X4 = case_when(
      X4 %in% c("l cortex", "r cortex") ~ "cortex",
      X4 %in% c("l medulla", "r medulla") ~ "medulla",
      X4 %in% c("l pelvis", "r pelvis") ~ "pelvis",
      TRUE ~ X4
    )
  ) %>%
  group_by(X2, X4, X5) %>%
  summarize(across(c(X1:M5, y:N5), mean, na.rm = TRUE)) %>%
  mutate(X3 = "Dotarem® ")

# Combine both data frames
comparable_data <- bind_rows(comparable_data1, comparable_data2)

stats2 <- data.frame(
  timepoint = numeric(),
  region = character(),
  t_statistic = numeric(),
  t_p_value = numeric(),
  mean_diff = numeric(),
  shapiro_w_statistic_aguix = numeric(),
  shapiro_w_p_value_aguix = numeric(),
  shapiro_w_statistic_dotarem = numeric(),
  shapiro_w_p_value_dotarem = numeric(),
  t_degrees_freedom = numeric(),  # Adding degrees of freedom column
  stringsAsFactors = FALSE
)

organ = "pelvis"
t = 4
  #####
# Filter data for the specified organ and timepoint
dotarem <- comparable_data %>%
  filter(X3 == "Dotarem® ", X4 == organ, X2 == t)
  #filter(X3 == "GBCA", X4 == "vena cava", X2 == t)

aguix <- comparable_data %>%
  filter(X3 == "AGuIX® ", X4 == organ, X2 == t)

# Find common X5 values between dotarem and aguix
common_X5 <- intersect(dotarem$X5, aguix$X5)

# Filter dotarem and aguix to only include common X5 values
dotarem <- dotarem %>% filter(X5 %in% common_X5)
aguix <- aguix %>% filter(X5 %in% common_X5)
print(mean(aguix$X1 - aguix$M1))
print(mean(aguix$y - aguix$N1))
print(mean(dotarem$X1 - dotarem$M1))
print(mean(dotarem$y - dotarem$N1))
print(mean(aguix$X1 - dotarem$X1))
print( mean(aguix$X11 - dotarem$X11))
mean(aguix$y - dotarem$y)
mean(aguix$y1 - dotarem$y1)

# Perform the paired t-test
ttest <- t.test(aguix$N1, dotarem$N1, paired = TRUE)

# Calculate mean difference
mean_diff <- mean(aguix$N1) - mean(dotarem$N1)

# Perform Shapiro-Wilk test for normality
shapiro_aguix <- shapiro.test(aguix$N1)
shapiro_dotarem <- shapiro.test(dotarem$N1)

# Append the results to the data frame
stats2 <- rbind(stats2, data.frame(
  timepoint = t,
  region = organ,
  t_statistic = ttest$statistic,
  t_p_value = ttest$p.value,
  mean_diff = mean_diff,
  shapiro_w_statistic_aguix = shapiro_aguix$statistic,
  shapiro_w_p_value_aguix = shapiro_aguix$p.value,
  shapiro_w_statistic_dotarem = shapiro_dotarem$statistic,
  shapiro_w_p_value_dotarem = shapiro_dotarem$p.value,
  t_degrees_freedom = ttest$parameter  # Add degrees of freedom
))








#####
comparable_data <- read.csv("imputated_data2.csv", fileEncoding = "ISO-8859-1")
stats2 <- data.frame(
  timepoint = numeric(),
  region = character(),
  t_statistic = numeric(),
  t_p_value = numeric(),
  mean_diff = numeric(),
  shapiro_w_statistic_aguix = numeric(),
  shapiro_w_p_value_aguix = numeric(),
  shapiro_w_statistic_dotarem = numeric(),
  shapiro_w_p_value_dotarem = numeric(),
  t_degrees_freedom = numeric(),  # Adding degrees of freedom column
  stringsAsFactors = FALSE
)


t = 5
organ = "aguix_pelvis"
#####
# Filter data for the specified organ and timepoint
dotarem <- comparable_data %>%
  filter(X3 == "AGuIX® ", X4 == "r pelvis", X2 == t)

aguix <- comparable_data %>%
  filter(X3 == "AGuIX® ", X4 == "l pelvis", X2 == t)

# Find common X5 values between dotarem and aguix
common_X5 <- intersect(dotarem$X5, aguix$X5)

# Filter dotarem and aguix to only include common X5 values
dotarem <- dotarem %>% filter(X5 %in% common_X5)
aguix <- aguix %>% filter(X5 %in% common_X5)

# Perform the paired t-test
ttest <- t.test(aguix$X1, dotarem$X1, paired = TRUE)

# Calculate mean difference
mean_diff <- mean(aguix$X1) - mean(dotarem$X1)

# Perform Shapiro-Wilk test for normality
shapiro_aguix <- shapiro.test(aguix$X1)
shapiro_dotarem <- shapiro.test(dotarem$X1)

# Append the results to the data frame
stats2 <- rbind(stats2, data.frame(
  timepoint = t,
  region = organ,
  t_statistic = ttest$statistic,
  t_p_value = ttest$p.value,
  mean_diff = mean_diff,
  shapiro_w_statistic_left = shapiro_aguix$statistic,
  shapiro_w_p_value_left= shapiro_aguix$p.value,
  shapiro_w_statistic_right= shapiro_dotarem$statistic,
  shapiro_w_p_value_right= shapiro_dotarem$p.value,
  t_degrees_freedom = ttest$parameter  # Add degrees of freedom
))

# Print the updated data frame
print(stats2)



stats2_first_half <- stats2[1:10, ]
stats2_second_half <- stats2[(10 + 1):20, ]
>>>>>>> 77a3982bf0902d3f28a8584daad61788e60cc341
