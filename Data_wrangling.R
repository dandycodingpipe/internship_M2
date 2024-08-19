library(readr)
library(glmnet)
library(dplyr)

source <- read.csv("Data/rawdata.csv")

#remove NAs present in the CT number data. This data should be discarded for analyses (Data cleaning)
valid_data <- source %>%
  filter(!X1 %in% NA)

valid_data$X1 = as.numeric(valid_data$X1)

#remove non-comparative organs and study IDs using pipe filtering this will be the output dataframe (Data cleaning) 
comparable_data <- valid_data %>% 
  filter(!X4 %in% c("spleen", "liver")) %>%
  filter(!X5 %in% c(10674, 10308, 26328, 26361))

#selecting a specific organ for comparisons and performing the Si-component removal (Data preprocessing)
organ <- "right medulla"
df <- comparable_data %>% 
  filter(X4 %in% organ) %>%
  filter(X3 %in% "AGUIX") %>%
  mutate(X1 = as.numeric(X1)*(1 - 0.0569))

#df <- df %>%
 # filter(!X2 %in% 1)



### Machine learning
# Split the data into training and test sets
set.seed(123)  # For reproducibility

regression <- lm(y ~ X1, data = df)
print(summary(regression))

mean_df <- df %>%
  group_by(X2) %>%
  summarize(X1 = mean(X1), y = mean(y))
#means
regression <- lm(y ~ X1, data = mean_df)
print(summary(regression))
# Fit the linear model

#####
### Plot df$y versus df$X1 with custom axes and title
plot(df$X1, df$y, xlab = "CT number", ylab = "[Gd] (mg/mL)", main = paste0("OLS-fit (", organ,")"))

# Add red dots representing the mean
#points(mean_df$mean_X1, mean_df$mean_y, col = "red", pch = 19)
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

# Predict y values for rows where X3 is "GBCA", X4 is "organ", and y is NA, then update the original dataframe
comparable_data <- comparable_data %>%
  mutate(y = ifelse(X3 == "GBCA" & X4 == organ & is.na(y), 
                    predict(regression, newdata = comparable_data[X3 == "GBCA" & X4 == organ & is.na(y), ]), 
                    y))


mean_data <- comparable_data %>%
  group_by(X2, X3, X4) %>%
  summarize(mean_X1 = mean(X1), sd_X1 = sd(X1), mean_y = mean(y), sd_y = sd(y))

mean_data <- mean_data[order(mean_data$X4), ]
