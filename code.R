# Load necessary library
library(ggplot2)

# Read the data from the CSV file
data <- read.csv("C:\\Users\\SourceCode\\Downloads\\Car Sales (3).csv")

# View the first few rows of the data
head(data)

# Visualization 1: Histogram of Buyer Age
ggplot(data, aes(x = Buyer.Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  ggtitle("Distribution of Buyer Age") +
  xlab("Buyer Age") +
  ylab("Frequency") +
  theme_minimal()


# Visualization 3: Boxplot of Sale Price by Buyer Gender
ggplot(data, aes(x = Buyer.Gender, y = Sale.Price, fill = Buyer.Gender)) +
  geom_boxplot() +
  ggtitle("Sale Price by Buyer Gender") +
  xlab("Buyer Gender") +
  ylab("Sale Price") +
  theme_minimal() +
  theme(legend.position = "none")

# Statistical Analysis

# Fit a linear regression model
model <- lm(Sale.Price ~ Buyer.Age + Discount, data = data)

# Summarize the model to view coefficients and p-values
summary(model)

# Extract p-values for Buyer Age and Discount
coefficients <- summary(model)$coefficients
p_values <- coefficients[, 4]

# Check significance of Buyer Age
if (p_values["Buyer.Age"] < 0.05) {
  print("Buyer Age is a significant predictor of Sale Price.")
} else {
  print("Buyer Age is not a significant predictor of Sale Price.")
}

# Check significance of Discount
if (p_values["Discount"] < 0.05) {
  print("Discount is a significant predictor of Sale Price.")
} else {
  print("Discount is not a significant predictor of Sale Price.")
}

# Conclusion based on p-values
if (p_values["Buyer.Age"] < 0.05 & p_values["Discount"] < 0.05) {
  print("Both Buyer Age and Discount significantly impact the Sale Price of cars.")
} else if (p_values["Buyer.Age"] < 0.05) {
  print("Only Buyer Age significantly impacts the Sale Price of cars.")
} else if (p_values["Discount"] < 0.05) {
  print("Only Discount significantly impacts the Sale Price of cars.")
} else {
  print("Neither Buyer Age nor Discount significantly impact the Sale Price of cars.")
}
