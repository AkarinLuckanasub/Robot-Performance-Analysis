# Load necessary library
library(lubridate)
library(ggplot2)

# Set locale to English (United States)
Sys.setlocale("LC_TIME", "C")

# Define the data collection period and parameters for 10 days
start_time_before <- as.POSIXct("2024-06-01 08:00")
end_time_before <- start_time_before + days(6) - hours(1)
start_time_after <- as.POSIXct("2024-06-07 08:00")
end_time_after <- start_time_after + days(6) - hours(1)

# Generate hourly timestamps within the defined periods
timestamps_before <- seq(from = start_time_before, to = end_time_before, by = "hour")
timestamps_after <- seq(from = start_time_after, to = end_time_after, by = "hour")

# Sample data for illustration purposes
set.seed(44) # Setting seed for reproducibility
workpieces_before <- sample(40:60, length(timestamps_before), replace = TRUE)
workpieces_after <- sample(50:70, length(timestamps_after), replace = TRUE)

# Combine data into a data frame
data_before <- data.frame(Time = timestamps_before, Workpieces = workpieces_before, Period = "Before")
data_after <- data.frame(Time = timestamps_after, Workpieces = workpieces_after, Period = "After")

# Combine both periods into one data frame
data <- rbind(data_before, data_after)

# View the first few rows of the data
head(data)

# Summary statistics
summary(data[data$Period == "Before", "Workpieces"])
summary(data[data$Period == "After", "Workpieces"])

# Visualize the data
ggplot(data, aes(x = as.POSIXct(Time), y = Workpieces, color = Period)) +
  geom_line() +
  labs(title = "Robot Performance Analysis: Pre and Post Upgrade", x = "Day (1-10)", y = "Number of Workpieces Processed Per Hour During 8-Hour Shifts") +
  scale_color_manual(name = "Period", values = c("Before" = "gold", "After" = "purple")) +
  theme_minimal()

# Statistical analysis
before_data <- data[data$Period == "Before", "Workpieces"]
after_data <- data[data$Period == "After", "Workpieces"]

t_test_result <- t.test(before_data, after_data, paired = TRUE)
print(t_test_result)

# Calculate mean workpieces processed per hour
mean_before <- mean(before_data)
mean_after <- mean(after_data)

# Print the results
cat("Average number of workpieces processed per hour before the upgrade:", mean_before, "\n")
cat("Average number of workpieces processed per hour after the upgrade:", mean_after, "\n")

# Interpretation
if (t_test_result$p.value < 0.05) {
  cat("The increase in the number of workpieces processed per hour after the upgrade is statistically significant.\n")
} else {
  cat("There is no statistically significant difference in the number of workpieces processed per hour before and after the upgrade.\n")
}
