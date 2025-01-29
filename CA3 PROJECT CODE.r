
library(dplyr)
library(tidyr)

df <- read.csv("D:/lpu/SEM 5/RESEARCH PAPERS/Delhi house data.csv")


head(df)

df_cleaned <- df %>% drop_na()

df_cleaned <- df_cleaned %>% distinct()

# View the cleaned dataset
head(df_cleaned)

library(ggplot2)



# Create a histogram of property prices
ggplot(df, aes(x = Price)) +
  geom_histogram(aes(y = ..density..), binwidth = 500000, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", linetype = "dashed") +
  labs(title = "Distribution of Property Prices with Density Curve",
       x = "Property Price (INR)",
       y = "Density") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma)



top_localities <- df %>% 
  group_by(Locality) %>%
  summarise(Listing_Count = n()) %>%
  top_n(10, wt = Listing_Count)

df_top_localities <- df %>% filter(Locality %in% top_localities$Locality)

# Create the box plot for top localities
ggplot(df_top_localities, aes(x = Locality, y = Price)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) + 
  labs(title = "Price Variation Across Top Localities", 
       x = "Locality", 
       y = "Price (in currency)") +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels for readability


ggplot(df, aes(x = Area, y = Price)) +
  geom_point(color = "blue", alpha = 0.6, size = 3) +  # Plot the points with some transparency
  labs(title = "Scatter Plot of Area vs. Price", 
       x = "Area (in square feet)", 
       y = "Price (in currency)") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()  # Use a clean theme for better visualization



# Bar plot for BHK (Number of Bedrooms)
# Load necessary library
library(ggplot2)

# Bar plot for BHK (Number of Bedrooms)
ggplot(df, aes(x = as.factor(BHK))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Frequency of BHK", x = "Number of Bedrooms", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

# Bar plot for Furnishing Status
ggplot(df, aes(x = Furnishing)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Frequency of Furnishing Status", x = "Furnishing Status", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

# Bar plot for Transaction Type
ggplot(df, aes(x = Transaction)) +
  geom_bar(fill = "orange") +
  labs(title = "Frequency of Transaction Type", x = "Transaction Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))


library(reshape2)  # for melting the correlation matrix
library(ggcorrplot) # for easy heatmap plotting

# Sample dataframe with continuous variables
df_subset <- df[, c("Area", "Price", "BHK", "Bathroom")]

# Calculate the Pearson correlation matrix
cor_matrix <- cor(df_subset, use = "complete.obs")

# Create a heatmap using ggcorrplot
ggcorrplot(cor_matrix, 
           method = "square",      # Heatmap with squares
           type = "lower",         # Show only the lower triangle
           lab = TRUE,             # Display the correlation coefficients on the heatmap
           title = "Correlation Heatmap",
           colors = c("red", "white", "blue")) # Color gradient (red for negative, blue for positive)




price_data <- df$Price

# Calculate Measures of Central Tendency
mean_price <- mean(price_data, na.rm = TRUE)  # Mean
median_price <- median(price_data, na.rm = TRUE)  # Median

# Mode Function
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

mode_price <- get_mode(price_data)  # Mode

# Calculate Measures of Dispersion
std_dev_price <- sd(price_data, na.rm = TRUE)  # Standard Deviation
variance_price <- var(price_data, na.rm = TRUE)  # Variance
iqr_price <- IQR(price_data, na.rm = TRUE)  # Interquartile Range

# Create a summary table
summary_table <- data.frame(
  Measure = c("Mean", "Median", "Mode", "Standard Deviation", "Variance", "Interquartile Range"),
  Value = c(mean_price, median_price, mode_price, std_dev_price, variance_price, iqr_price)
)

# Print the summary table
print(summary_table)


mean_area <- mean(df$Area, na.rm = TRUE)  # Mean area
median_area <- median(df$Area, na.rm = TRUE)  # Median area

# Print the average property area
cat("Average Property Area (Mean):", mean_area, "\n")
cat("Average Property Area (Median):", median_area, "\n")

min_price <- min(df$Price, na.rm = TRUE)  # Minimum price
max_price <- max(df$Price, na.rm = TRUE)  # Maximum price
iqr_price <- IQR(df$Price, na.rm = TRUE)  # Interquartile range
std_dev_price <- sd(df$Price, na.rm = TRUE)  # Standard deviation

# Print price distribution
cat("Price Range: [", min_price, "-", max_price, "]\n")
cat("Interquartile Range (Price):", iqr_price, "\n")
cat("Standard Deviation of Prices:", std_dev_price, "\n")

bhk_count <- table(df$BHK)  # BHK column contains the number of bedrooms

# Bathroom Count Frequency
bathroom_count <- table(df$Bathroom)  # Bathroom column contains the number of bathrooms

# Print BHK and bathroom count
cat("BHK Count Frequency:\n")
print(bhk_count)
cat("\nBathroom Count Frequency:\n")
print(bathroom_count)





df$Furnishing_Grouped <- ifelse(df$Furnishing %in% c("Furnished", "Semi-Furnished"), "Furnished", "Unfurnished")

# Summarize the new grouped Furnishing variable
furnishing_grouped_table <- table(df$Furnishing_Grouped)

# Display the summary
furnishing_grouped_table

# Calculate probabilities for the grouped Furnishing
total_furnishing_grouped <- sum(furnishing_grouped_table)
p_furnished_grouped <- furnishing_grouped_table["Furnished"] / total_furnishing_grouped  # Probability of being furnished (including semi-furnished)
p_unfurnished <- furnishing_grouped_table["Unfurnished"] / total_furnishing_grouped  # Probability of being unfurnished

# Applying Binomial Distribution for the grouped Furnishing
n_properties <- total_furnishing_grouped  # Number of trials (total properties)
# Probability of having exactly 50 furnished (Furnished + Semi-Furnished) properties
dbinom(50, size = n_properties, prob = p_furnished_grouped)

# Cumulative probability of 50 or fewer furnished (Furnished + Semi-Furnished) properties
pbinom(50, size = n_properties, prob = p_furnished_grouped)

# Simulate 1000 outcomes for the grouped furnished properties
set.seed(123)
binom_furnishing_grouped_simulation <- rbinom(1000, size = n_properties, prob = p_furnished_grouped)

# Plot a histogram of the simulation for the grouped Furnishing
hist(binom_furnishing_grouped_simulation, 
     main = "Binomial Distribution: Number of Furnished (incl. Semi-Furnished) Properties", 
     xlab = "Number of Furnished (incl. Semi-Furnished) Properties", 
     col = "skyblue", 
     breaks = 20)

# Display the probability values
p_furnished_grouped
p_unfurnished





transaction_table <- table(df$Transaction)

# Display the summary
transaction_table

# Calculate probabilities for Transaction
total_transaction <- sum(transaction_table)
p_new <- transaction_table["New"] / total_transaction     # Probability of a new transaction
p_resale <- transaction_table["Resale"] / total_transaction  # Probability of a resale transaction

# Applying Binomial Distribution for Transaction
n_properties_transaction <- total_transaction  # Number of trials (total properties)
# Probability of having exactly 30 new transactions
dbinom(30, size = n_properties_transaction, prob = p_new)

# Cumulative probability of 30 or fewer new transactions
pbinom(30, size = n_properties_transaction, prob = p_new)

# Simulate 1000 outcomes for new transactions
set.seed(123)
binom_transaction_simulation <- rbinom(1000, size = n_properties_transaction, prob = p_new)

# Plot a histogram of the simulation for Transactions
hist(binom_transaction_simulation, 
     main = "Binomial Distribution: Number of New Transactions", 
     xlab = "Number of New Transactions", 
     col = "orange", 
     breaks = 20)

# Display the probability values
p_new
p_resale




# Check for missing values in Bathroom and Parking columns
summary(df$Bathroom)
summary(df$Parking)

# Check the counts for properties with 2 bathrooms and 1 parking
two_bathroom_count <- sum(df$Bathroom == 2, na.rm = TRUE)  # Properties with 2 bathrooms
one_parking_count <- sum(df$Parking == 1, na.rm = TRUE)    # Properties with 1 parking
total_properties <- nrow(df)                               # Total number of properties

# Print counts to ensure they are valid
print(paste("Properties with 2 bathrooms:", two_bathroom_count))
print(paste("Properties with 1 parking:", one_parking_count))
print(paste("Total number of properties:", total_properties))

# Ensure that the counts make sense for the hypergeometric distribution
if (two_bathroom_count > 0 & one_parking_count > 0) {
  # Set the number of samples to a valid number, say 10
  n_sample <- 10
  
  # Simulate Hypergeometric Distribution for 2 bathrooms
  set.seed(123)
  sim_bathroom <- rhyper(1000, two_bathroom_count, total_properties - two_bathroom_count, n_sample)
  
  # Plot histogram of the simulation for 2 bathrooms
  hist(sim_bathroom, 
       main = "Hypergeometric Distribution: Properties with 2 Bathrooms", 
       xlab = "Number of Properties with 2 Bathrooms in Sample", 
       col = "skyblue", 
       breaks = 10)
  
  # Simulate Hypergeometric Distribution for 1 parking
  set.seed(123)
  sim_parking <- rhyper(1000, one_parking_count, total_properties - one_parking_count, n_sample)
  
  # Plot histogram of the simulation for 1 parking
  hist(sim_parking, 
       main = "Hypergeometric Distribution: Properties with 1 Parking", 
       xlab = "Number of Properties with 1 Parking in Sample", 
       col = "orange", 
       breaks = 10)
} else {
  print("Invalid counts for 2 bathrooms or 1 parking. Please check your data.")
}








df_clean <- df[!is.na(df$Price) & !is.na(df$Area), ]

# Histogram and density plot for the Price variable
ggplot(df_clean, aes(x = Price)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Price Distribution (with Density Plot)", x = "Price", y = "Density") +
  theme_minimal()

# Histogram and density plot for the Area variable
ggplot(df_clean, aes(x = Area)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", color = "black") +
  geom_density(color = "blue", size = 1) +
  labs(title = "Area Distribution (with Density Plot)", x = "Area (in square feet)", y = "Density") +
  theme_minimal()
