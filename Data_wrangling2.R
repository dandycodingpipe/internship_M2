<<<<<<< HEAD
library(readr)
library(glmnet)
library(dplyr)
library(tidyverse)

#imputated data 2 is only the vessel data with improved feature extraction 
source <- read.csv("raw_data2.csv", fileEncoding = "UTF-8")

#remove NAs present in the CT number data. This data should be discarded for analyses (Data cleaning)
valid_data <- source %>%
  filter(!M1 %in% NA)

valid_data$M1 = as.numeric(valid_data$M1)

comparable_data <- valid_data

#selecting a specific organ for comparisons and performing the Si-component removal (Data preprocessing)
organ <- "aorta"
df <- comparable_data %>% 
  filter(X4 %in% organ) %>%
  filter(X3 %in% "AGUIX") %>%
  filter(!X5 %in% c(10674, 10308, 26328, 26361)) %>% # I need to remove non-paired samples s7-s10 since these may introduce bias
  mutate(M1 = as.numeric(M1)*(1 - 0.0569))

#df <- df %>%
#  filter(!X2 %in% 1)



### Machine learning
# Split the data into training and test sets
set.seed(123)  # For reproducibility

regression <- lm(N1 ~ M1, data = df)
print(summary(regression))

mean_df <- df %>%
  group_by(X2) %>%
  summarize(M1 = mean(M1), N1 = mean(N1))

#means
regression <- lm(N1 ~ M1, data = mean_df)
print(summary(regression))
# Fit the linear model

#####
### Plot df$y versus df$X1 with custom axes and title
plot(df$M1, df$N1, xlab = "CT number", ylab = "[Gd] (mg/mL)", main = paste0("OLS-fit (", organ,")"))
# Add red dots representing the mean
points(mean_df$M1, mean_df$N1, col = "red", pch = 19)
# Add a regression line in blue
abline(regression, col = "blue")
# Get summary of the regression model
summary_regression <- summary(regression)
# Extract R-squared value
rsquared <- summary_regression$r.squared
rse <- sqrt(mean(residuals(regression)^2))

# Format R-squared and RSE for legend
rsq_label <- paste("R² =", round(rsquared, 3))
rse_label <- paste("RSE =", round(rse, 3))

# Add legend with abbreviated labels
legend("bottomright", legend = c(rsq_label, rse_label), 
       bg = "white", cex = 0.8)
#####

coefficients <- coef(regression)  # Extract coefficients
intercept <- coefficients[1]      # Intercept
slopes <- coefficients[-1]  

regression_sd <- lm(y1 ~ X11, data = df)
summary(regression_sd)
coefficients_sd <- coef(regression_sd)  # Extract coefficients
intercept_sd <- coefficients_sd[1]      # Intercept
slopes_sd <- coefficients_sd[-1]  

comparable_data <- comparable_data %>%
  mutate(
    y1 = case_when(
      X3 == "GBCA" & X4 == organ & is.na(y1) ~ intercept_sd + X11 * slopes_sd[1],
      TRUE ~ y1),  # Keep original y1 values unchanged otherwise
    y = case_when(
      X3 == "GBCA" & X4 == organ & is.na(y) ~ intercept + X1 * slopes[1],
      TRUE ~ y  # Keep original N1 values unchanged otherwise
    ),
    N1 = case_when(
      X3 == "GBCA" & X4 == organ & is.na(N1) ~ intercept + M1 * slopes[1],
      TRUE ~ N1  # Keep original N1 values unchanged otherwise
    ),
    N2 = case_when(
      X3 == "GBCA" & X4 == organ & is.na(N2) ~ intercept + M2 * slopes[1],
      TRUE ~ N2  # Keep original N2 values unchanged otherwise
    ),
    N3 = case_when(
      X3 == "GBCA" & X4 == organ & is.na(N3) ~ intercept + M3 * slopes[1],
      TRUE ~ N3  # Keep original N3 values unchanged otherwise
    ),
    N4 = case_when(
      X3 == "GBCA" & X4 == organ & is.na(N4) ~ intercept + M4 * slopes[1],
      TRUE ~ N4  # Keep original N4 values unchanged otherwise
    ),
    N5 = case_when(
      X3 == "GBCA" & X4 == organ & is.na(N5) ~ intercept + M5 * slopes[1],
      TRUE ~ N5  # Keep original N5 values unchanged otherwise
    )
  )

# Calculate mean and standard deviation for M1 and N1, N2, N3, N4, N5
mean_data <- comparable_data %>%
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

# Order the data by X4
mean_data <- mean_data[order(mean_data$X4), ]

print(mean_data)
=======
library(readr)
library(glmnet)
library(dplyr)
library(tidyverse)

#imputated data 2 is only the vessel data with improved feature extraction 
source <- read.csv("raw_data2.csv", fileEncoding = "UTF-8")

#remove NAs present in the CT number data. This data should be discarded for analyses (Data cleaning)
valid_data <- source %>%
  filter(!M1 %in% NA)

valid_data$M1 = as.numeric(valid_data$M1)

comparable_data <- valid_data

#selecting a specific organ for comparisons and performing the Si-component removal (Data preprocessing)
organ <- "aorta"
df <- comparable_data %>% 
  filter(X4 %in% organ) %>%
  filter(X3 %in% "AGUIX") %>%
  filter(!X5 %in% c(10674, 10308, 26328, 26361)) %>% # I need to remove non-paired samples s7-s10 since these may introduce bias
  mutate(M1 = as.numeric(M1)*(1 - 0.0569))

#df <- df %>%
#  filter(!X2 %in% 1)



### Machine learning
# Split the data into training and test sets
set.seed(123)  # For reproducibility

regression <- lm(N1 ~ M1, data = df)
print(summary(regression))

mean_df <- df %>%
  group_by(X2) %>%
  summarize(M1 = mean(M1), N1 = mean(N1))

#means
regression <- lm(N1 ~ M1, data = mean_df)
print(summary(regression))
# Fit the linear model

#####
### Plot df$y versus df$X1 with custom axes and title
plot(df$M1, df$N1, xlab = "CT number", ylab = "[Gd] (mg/mL)", main = paste0("OLS-fit (", organ,")"))
# Add red dots representing the mean
points(mean_df$M1, mean_df$N1, col = "red", pch = 19)
# Add a regression line in blue
abline(regression, col = "blue")
# Get summary of the regression model
summary_regression <- summary(regression)
# Extract R-squared value
rsquared <- summary_regression$r.squared
rse <- sqrt(mean(residuals(regression)^2))

# Format R-squared and RSE for legend
rsq_label <- paste("R² =", round(rsquared, 3))
rse_label <- paste("RSE =", round(rse, 3))

# Add legend with abbreviated labels
legend("bottomright", legend = c(rsq_label, rse_label), 
       bg = "white", cex = 0.8)
#####

coefficients <- coef(regression)  # Extract coefficients
intercept <- coefficients[1]      # Intercept
slopes <- coefficients[-1]  

regression_sd <- lm(y1 ~ X11, data = df)
summary(regression_sd)
coefficients_sd <- coef(regression_sd)  # Extract coefficients
intercept_sd <- coefficients_sd[1]      # Intercept
slopes_sd <- coefficients_sd[-1]  

comparable_data <- comparable_data %>%
  mutate(
    y1 = case_when(
      X3 == "GBCA" & X4 == organ & is.na(y1) ~ intercept_sd + X11 * slopes_sd[1],
      TRUE ~ y1),  # Keep original y1 values unchanged otherwise
    y = case_when(
      X3 == "GBCA" & X4 == organ & is.na(y) ~ intercept + X1 * slopes[1],
      TRUE ~ y  # Keep original N1 values unchanged otherwise
    ),
    N1 = case_when(
      X3 == "GBCA" & X4 == organ & is.na(N1) ~ intercept + M1 * slopes[1],
      TRUE ~ N1  # Keep original N1 values unchanged otherwise
    ),
    N2 = case_when(
      X3 == "GBCA" & X4 == organ & is.na(N2) ~ intercept + M2 * slopes[1],
      TRUE ~ N2  # Keep original N2 values unchanged otherwise
    ),
    N3 = case_when(
      X3 == "GBCA" & X4 == organ & is.na(N3) ~ intercept + M3 * slopes[1],
      TRUE ~ N3  # Keep original N3 values unchanged otherwise
    ),
    N4 = case_when(
      X3 == "GBCA" & X4 == organ & is.na(N4) ~ intercept + M4 * slopes[1],
      TRUE ~ N4  # Keep original N4 values unchanged otherwise
    ),
    N5 = case_when(
      X3 == "GBCA" & X4 == organ & is.na(N5) ~ intercept + M5 * slopes[1],
      TRUE ~ N5  # Keep original N5 values unchanged otherwise
    )
  )

# Calculate mean and standard deviation for M1 and N1, N2, N3, N4, N5
mean_data <- comparable_data %>%
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

# Order the data by X4
mean_data <- mean_data[order(mean_data$X4), ]

print(mean_data)
>>>>>>> 77a3982bf0902d3f28a8584daad61788e60cc341
