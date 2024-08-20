<<<<<<< HEAD
y_1 <-c(12.13303333,
        7.964676462,
        4.054808105,
        1.265254219,
        0.980784917,
        0.656716209,
        0.372968354,
        0.207138889,
        0.057983339,
        0.04992563
        
)
y_2 <-c(12.12622737,
        8.044724609,
        3.906147355,
        1.432275195,
        0.936587553,
        0.670379596,
        0.316624473,
        0.151206078,
        0.000947997,
        0.008595931
        
)

x <- c(15,10,5,2,1.5,1,0.5, 0.2, 0.1 ,0)

dotarem <- data.frame(x, y_1)
aguix <- data.frame(x, y_2)
both <- data.frame(y_1, y_2)

model <- lm(x~y_1, data = dotarem)
summary(model)

model <- lm(x~y_2, data = aguix)
summary(model)

model <- lm(y_1~y_2, data = both)
summary(model)

y_3 <- c(594.1410256,
         396.0779221,
         197.0512821,
         85.33333333,
         48.40506329,
         48.59493671,
         33.81012658,
         15.08974359,
         -1.376623377,
         -0.654320988
)

y_4 <- c(597.8860759,
         418.45,
         217.1038961,
         80.87179487,
         59.82278481,
         35.59493671,
         21.14102564,
         0.08974359,
         -3.296296296,
         -6.291139241)

both2 <- data.frame(y_3,y_4)

model <- lm(y_3~y_4, data = both2)
summary(model)



library(tidyverse)
library(dplyr)

# Load
df = read.csv("prelim_phantom.csv")

# Reshape data
wrangled_data <- df %>%
  pivot_longer(cols = -1., names_to = "variable", values_to = "value") 
  
#Plot raw
library(ggplot2)
gg <-ggplot(wrangled_data, aes(x = density_mgml, y = value, color = variable)) + 
      geom_point() + 
      theme_minimal()
print(gg)

simplified_plot <- function(wrangled_data, density_col, value_col, variable_col) {
  ggplot(wrangled_data, aes_string(x = density_col, y = value_col, color = variable_col)) +
    #geom_point() +  # Adds data points
    stat_smooth(method = "lm", se = FALSE) +  # Adds a linear regression line
    theme_minimal() +  # Uses a minimal theme for a clean look
    theme(legend.position = "bottom",  # Positions the legend at the bottom
          panel.grid.major = element_blank(),  # Removes major grid lines
          panel.grid.minor = element_blank(),  # Removes minor grid lines
          plot.title = element_text(hjust = 0.5),  # Centers the plot title
          axis.line = element_line(color = "black"),  # Adds axis lines in black
          axis.ticks = element_line(color = "black")) +  # Adds tick marks in black
    labs(x = "Prepared concentration (mg Gd/mL)", y = "Hounsfield Units (HU)",
         title = "SPCCT relative attenuation vs. absolute quantification") +
    scale_color_manual(values = c("Blue", "Red", "LightBlue", "Pink")) +
    scale_x_continuous(breaks = pretty(wrangled_data[[density_col]], n = 10),  # Pretty breaks for x-axis
                       labels = scales::comma) +
    scale_y_continuous(breaks = pretty(wrangled_data[[value_col]], n = 10),  # Pretty breaks for y-axis
                       labels = scales::comma)
}

# Usage
simplified_plot(wrangled_data, "density_mgml", "value", "variable")




#Model
model = lm(density_mgml~df$ex_AGIX, df)
summary(model)

mean(df$pred_AGIX-df$pred_GBCA)
mean(df$ex_AGIX-df$ex_GBCA)
df
print(wilcox.test(df$pred_AGIX, df$pred_GBCA, paired = TRUE))

#Model
model = lm(x~df$ex_AGIX, df)
summary(model)


# Install ggplot2 if not already installed
if(!require(ggplot2)) install.packages("ggplot2")

# Load the ggplot2 package
library(ggplot2)

# Your data
x <- c(15,10,5,2,1.5,1,0.5, 0.2, 0.1, 0)
y_1 <- c(5.548945201,
         0.619190801,
         0.536298914,
         0.353784247,
         0.203349585
)
y_2 <- c(4.523664836,
         0.091851469,
         0.117597732,
         -0.087270262,
         -0.230691209
)

# Create a data frame
data <- data.frame(x, y_1, y_2)

# Plotting y_1 and y_2 as a function of x using ggplot
p <- ggplot(data, aes(x=x)) +
  geom_line(aes(y=y_1, colour="y_1"), size=1.5) +  # Line for y_1
  geom_point(aes(y=y_1, colour="y_1")) +           # Points for y_1
  geom_line(aes(y=y_2, colour="y_2"), size=1.5) +  # Line for y_2
  geom_point(aes(y=y_2, colour="y_2")) +           # Points for y_2
  labs(title="Plot of y_1 and y_2 as Functions of x", x="x", y="y") +
  scale_colour_manual(name="Series", values=c("y_1"="blue", "y_2"="red"))

# Display the plot
print(p)

model = lm(y_1~y_2, data = data)
summary(model)

=======
y_1 <-c(12.13303333,
        7.964676462,
        4.054808105,
        1.265254219,
        0.980784917,
        0.656716209,
        0.372968354,
        0.207138889,
        0.057983339,
        0.04992563
        
)
y_2 <-c(12.12622737,
        8.044724609,
        3.906147355,
        1.432275195,
        0.936587553,
        0.670379596,
        0.316624473,
        0.151206078,
        0.000947997,
        0.008595931
        
)

x <- c(15,10,5,2,1.5,1,0.5, 0.2, 0.1 ,0)

dotarem <- data.frame(x, y_1)
aguix <- data.frame(x, y_2)
both <- data.frame(y_1, y_2)

model <- lm(x~y_1, data = dotarem)
summary(model)

model <- lm(x~y_2, data = aguix)
summary(model)

model <- lm(y_1~y_2, data = both)
summary(model)

y_3 <- c(594.1410256,
         396.0779221,
         197.0512821,
         85.33333333,
         48.40506329,
         48.59493671,
         33.81012658,
         15.08974359,
         -1.376623377,
         -0.654320988
)

y_4 <- c(597.8860759,
         418.45,
         217.1038961,
         80.87179487,
         59.82278481,
         35.59493671,
         21.14102564,
         0.08974359,
         -3.296296296,
         -6.291139241)

both2 <- data.frame(y_3,y_4)

model <- lm(y_3~y_4, data = both2)
summary(model)



library(tidyverse)
library(dplyr)

# Load
df = read.csv("prelim_phantom.csv")

# Reshape data
wrangled_data <- df %>%
  pivot_longer(cols = -1., names_to = "variable", values_to = "value") 
  
#Plot raw
library(ggplot2)
gg <-ggplot(wrangled_data, aes(x = density_mgml, y = value, color = variable)) + 
      geom_point() + 
      theme_minimal()
print(gg)

simplified_plot <- function(wrangled_data, density_col, value_col, variable_col) {
  ggplot(wrangled_data, aes_string(x = density_col, y = value_col, color = variable_col)) +
    #geom_point() +  # Adds data points
    stat_smooth(method = "lm", se = FALSE) +  # Adds a linear regression line
    theme_minimal() +  # Uses a minimal theme for a clean look
    theme(legend.position = "bottom",  # Positions the legend at the bottom
          panel.grid.major = element_blank(),  # Removes major grid lines
          panel.grid.minor = element_blank(),  # Removes minor grid lines
          plot.title = element_text(hjust = 0.5),  # Centers the plot title
          axis.line = element_line(color = "black"),  # Adds axis lines in black
          axis.ticks = element_line(color = "black")) +  # Adds tick marks in black
    labs(x = "Prepared concentration (mg Gd/mL)", y = "Hounsfield Units (HU)",
         title = "SPCCT relative attenuation vs. absolute quantification") +
    scale_color_manual(values = c("Blue", "Red", "LightBlue", "Pink")) +
    scale_x_continuous(breaks = pretty(wrangled_data[[density_col]], n = 10),  # Pretty breaks for x-axis
                       labels = scales::comma) +
    scale_y_continuous(breaks = pretty(wrangled_data[[value_col]], n = 10),  # Pretty breaks for y-axis
                       labels = scales::comma)
}

# Usage
simplified_plot(wrangled_data, "density_mgml", "value", "variable")




#Model
model = lm(density_mgml~df$ex_AGIX, df)
summary(model)

mean(df$pred_AGIX-df$pred_GBCA)
mean(df$ex_AGIX-df$ex_GBCA)
df
print(wilcox.test(df$pred_AGIX, df$pred_GBCA, paired = TRUE))

#Model
model = lm(x~df$ex_AGIX, df)
summary(model)


# Install ggplot2 if not already installed
if(!require(ggplot2)) install.packages("ggplot2")

# Load the ggplot2 package
library(ggplot2)

# Your data
x <- c(15,10,5,2,1.5,1,0.5, 0.2, 0.1, 0)
y_1 <- c(5.548945201,
         0.619190801,
         0.536298914,
         0.353784247,
         0.203349585
)
y_2 <- c(4.523664836,
         0.091851469,
         0.117597732,
         -0.087270262,
         -0.230691209
)

# Create a data frame
data <- data.frame(x, y_1, y_2)

# Plotting y_1 and y_2 as a function of x using ggplot
p <- ggplot(data, aes(x=x)) +
  geom_line(aes(y=y_1, colour="y_1"), size=1.5) +  # Line for y_1
  geom_point(aes(y=y_1, colour="y_1")) +           # Points for y_1
  geom_line(aes(y=y_2, colour="y_2"), size=1.5) +  # Line for y_2
  geom_point(aes(y=y_2, colour="y_2")) +           # Points for y_2
  labs(title="Plot of y_1 and y_2 as Functions of x", x="x", y="y") +
  scale_colour_manual(name="Series", values=c("y_1"="blue", "y_2"="red"))

# Display the plot
print(p)

model = lm(y_1~y_2, data = data)
summary(model)

>>>>>>> 77a3982bf0902d3f28a8584daad61788e60cc341
