# Loading necessary libraries
library(ggplot2)
library(dplyr)
library(reshape2)
# Loading the RColorBrewer package
library(RColorBrewer)
install.packages("corrplot")
library(corrplot)

# Loading the dataset
crimeData <- read.csv("crimebystatecombinedwithunemployment.csv", stringsAsFactors = FALSE)

# Printing the first few rows of the data
head(crimeData)

# Printing the last few rows of the data
tail(crimeData)

# Printing the structure of the data
str(crimeData)

# Checking the null values in the data
sum(is.na(crimeData))

# Removing the null values from the data
crimeData <- na.omit(crimeData)

# Printing the first few rows of the cleaned data
head(crimeData)

# Printing the summary statistics of the data
summary(crimeData)

# Printing the column names of the data
colnames(crimeData)

# Calculating average violent crime rate by state
AvgViolentCrime <- crimeData %>%
  group_by(state) %>%
  summarise(Average_Violent_Crime = mean(`violent.total`, na.rm = TRUE))

# Printing the summary of the Average violent crime rate
summary(AvgViolentCrime)

# Descriptive Statistics for the Crime Data

# Summary statistics for numerical columns
numerical_summary <- crimeData %>% 
  select(unemployment, Population, `violent.total`, Murder, rape, Robbery, `Aggravated.assault`, `property.total`, Burglary, `Larceny.theft`, `vehicle.theft`) %>%
  summarise_all(list(
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    var = ~var(., na.rm = TRUE)
  ))

print("Descriptive Statistics for Numerical Variables:")
print(round(numerical_summary, 2))

# Check for missing values in Unemployment and Violent Crime Total
cleaned_crime_data <- crimeData %>%
  filter(!is.na(unemployment) & !is.na(`violent.total`))

# Compute Spearman Correlation Coefficient between Unemployment Rate and Violent Crime Total
spearman_correlation <- cor(cleaned_crime_data$unemployment, cleaned_crime_data$`violent.total`, method = "spearman")
print(paste("Spearman Correlation Coefficient:", round(spearman_correlation, 2)))

# Perform Hypothesis Test for Spearman Correlation
spearman_test <- cor.test(cleaned_crime_data$unemployment, cleaned_crime_data$`violent.total`, method = "spearman")

# Print the test results
print(spearman_test)

# Interpretation based on p-value
if (spearman_test$p.value <= 0.05) {
  print("Reject the null hypothesis: There is a significant between Unemployment Rate and Violent Crime Rate.")
} else {
  print("Fail to reject the null hypothesis: There is no significant between Unemployment Rate and Violent Crime Rate.")
}

# Select only numeric columns for correlation matrix
numeric_data_crime <- cleaned_crime_data %>% select(where(is.numeric))

# Compute the correlation matrix for all numeric columns
cor_matrix <- cor(numeric_data_crime, use = "complete.obs")

# Plot the heatmap of the correlation matrix (Changed Colors)
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.cex = 1,  # Adjusted label size and kept text color black
         title = "Correlation Heatmap of Crime Data",
         mar = c(0, 0, 1, 0),  # Adjusted margins
         col = colorRampPalette(c("green", "yellow", "red"))(200))  # Changed color palette to green-yellow-red

# Build Linear Regression Model for Property Crime Prediction
model <- lm(`property.total` ~ unemployment + `violent.total` + Murder + rape + Robbery + `Aggravated.assault`, data = crimeData)
print(summary(model)) # Display model summary

# Plot Regression Line for Unemployment vs Property Total Crime
ggplot(crimeData, aes(x = unemployment, y = `property.total`)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Regression: Property Total Crime vs Unemployment",
    x = "Unemployment Rate",
    y = "Property Crime Total"
  ) +
  theme_minimal()

# Scatter Plot for Unemployment vs Violent Crime Total with a regression line (Changed Colors)
ggplot(cleaned_crime_data, aes(x = unemployment, y = `violent.total`)) +
  geom_point(color = "purple", alpha = 0.7) +  # Changed points color to purple
  geom_smooth(method = "lm", color = "darkorange", se = FALSE) +  # Changed line color to dark orange
  labs(title = "Scatter Plot: Unemployment Rate vs Violent Crime Total",
       x = "Unemployment Rate",
       y = "Violent Crime Total") +
  theme_minimal(base_size = 15) +  # Adjusted font size for clarity
  theme(
    plot.title = element_text(color = "darkred"),  # Title color changed to dark red
    axis.title.x = element_text(color = "darkblue"),  # X-axis label color changed to dark blue
    axis.title.y = element_text(color = "darkblue")   # Y-axis label color changed to dark blue
  )


# Plotting a Boxplot of Violent Crime by State
ggplot(crimeData, aes(x = state, y = `violent.total`)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +  # Changed colors to green shades
  labs(
    title = "Violent Crime by State",
    x = "State",
    y = "Violent Crime Total"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Plotting a Bar chart for the distribution of Average Violent Crime by State with a predefined color palette
ggplot(AvgViolentCrime, aes(x = state, y = Average_Violent_Crime, fill = state)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +  # Use the Set3 color palette
  labs(
    title = "Average Violent Crime by State",
    x = "State",
    y = "Average Violent Crime Total"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Histogram for Unemployment Rate
ggplot(crimeData, aes(x = unemployment)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Distribution of Unemployment Rates with Density Curve",
    x = "Unemployment Rate",
    y = "Density"
  ) +
  theme_minimal()

# Histogram for Violent Crime Total
ggplot(crimeData, aes(x = `violent.total`)) +
  geom_histogram(aes(y = ..density..), binwidth = 50, fill = "green", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(
    title = "Distribution of Violent Crime Total with Density Curve",
    x = "Violent Crime Total",
    y = "Density"
  ) +
  theme_minimal()
