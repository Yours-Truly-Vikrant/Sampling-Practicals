#The following data gives the geographical area (in acres) under paddy for 58 villages. Draw sample of eight villages by using SRSWOR. Based on sample drawn find the estimate of ‘average area per village under paddy’ and estimate of its variance and 95% confidence interval for ‘average area per village under paddy’.
# 98, 19, 47, 137, 155, 270, 64, 69, 127, 292, 79, 81, 144, 104, 240, 273, 141, 56, 117, 201, 130, 58, 102, 170, 261, 158, 29, 102, 210, 189, 116, 46, 187, 101, 194, 93, 161, 222, 41, 127, 179, 223, 33, 114, 76, 96, 78, 88, 137, 114, 56, 108, 179, 318, 58, 58, 76, 272 
# Step 1: Create the data vector
paddy_area <- c(98, 19, 47, 137, 155, 270, 64, 69, 127, 292, 79, 81, 144, 104, 240, 273, 141, 56, 117, 201, 130, 58, 
                102, 170, 261, 158, 29, 102, 210, 189, 116, 46, 187, 101, 194, 93, 161, 222, 41, 127, 179, 223, 33, 
                114, 76, 96, 78, 88, 137, 114, 56, 108, 179, 318, 58, 58, 76, 272)

# Step 2: Draw a random sample of 8 villages without replacement
sample_villages <- sample(paddy_area, size = 8, replace = FALSE)

# Display the sample
sample_villages
# Calculate the estimates and confidence interval
sample_mean <- mean(sample_villages)
sample_variance <- var(sample_villages)
sample_size <- length(sample_villages)
alpha <- 0.05  # 95% confidence level

# Calculate the critical t-value
critical_t <- qt(1 - alpha/2, df = sample_size - 1)

# Calculate the standard error
standard_error <- sqrt(sample_variance / sample_size)

# Calculate the lower and upper limits of the confidence interval
lower_limit <- sample_mean - (critical_t * (standard_error / sqrt(sample_size)))
upper_limit <- sample_mean + (critical_t * (standard_error / sqrt(sample_size)))

# Display the results
cat("Estimate of average area per village under paddy:", sample_mean, "\n")
cat("Estimate of variance of area per village under paddy:", sample_variance, "\n")
cat("95% Confidence Interval for average area per village under paddy: [", lower_limit, ", ", upper_limit, "]\n")



#Q.2


#The following are the monthly expenditures of 25 households selected randomly by using SRSWOR from a village having N = 400 households.
#1698, 1889, 1910, 1768, 1852, 1837, 1811, 1762, 1722, 1851, 1717, 1715,1791, 1908, 1801, 1800, 1893, 1771, 1709, 1772, 1667, 1690, 1811, 1816, 1731.
#Obtain an unbiased estimate of monthly average expenditure per household in the village. Also, provide an estimate of 98% C.I. for the monthly average expenditure per household.









# Step 1: Enter the data
monthly_expenditures <- c(1698, 1889, 1910, 1768, 1852, 1837, 1811, 1762, 1722, 1851, 1717,
                          1715, 1791, 1908, 1801, 1800, 1893, 1771, 1709, 1772, 1667,
                          1690, 1811, 1816, 1731)

# Step 2: Calculate the sample mean
sample_mean <- mean(monthly_expenditures)

# Step 3: Calculate the standard error of the mean
n <- length(monthly_expenditures)
standard_error <- sd(monthly_expenditures) / sqrt(n)

# Step 4: Calculate the critical t-value for a 98% confidence level
confidence_level <- 0.98
df <- n - 1
critical_t <- qt(1 - (1 - confidence_level) / 2, df)

# Step 5: Calculate the margin of error and the confidence interval
margin_of_error <- critical_t * standard_error
lower_limit <- sample_mean - margin_of_error
upper_limit <- sample_mean + margin_of_error

# Display the results
cat("Unbiased estimate of monthly average expenditure per household:", sample_mean, "\n")
cat("98% Confidence Interval for monthly average expenditure per household: [", lower_limit, ", ", upper_limit, "]\n")





#3
#A SRS of 30 households was drawn from a city area containing 14,848 households. The number of persons per household in sample was as follows
#5, 6, 3, 3, 2, 3, 3, 3, 4, 4, 3, 2, 7, 4, 3,5, 4, 4, 3, 3, 4, 3, 3, 1, 2, 4, 3, 4, 2, 4
#Estimate the total number of people in the area and compute the probability that this estimate is within ± 10 % of the true value.


# Given data
sample_size <- 30
population_size <- 14848
persons_per_household <- c(5, 6, 3, 3, 2, 3, 3, 3, 4, 4, 3, 2, 7, 4, 3, 5, 4, 4, 3, 3, 4, 3, 3, 1, 2, 4, 3, 4, 2, 4)

# Calculate the total estimate using Horvitz-Thompson Estimator
total_estimate <- population_size * (1/sample_size) * sum(persons_per_household)

# Calculate the lower and upper bounds for ±10% of the true value
lower_bound <- 0.9 * total_estimate
upper_bound <- 1.1 * total_estimate

# Calculate the probability that the estimate is within ±10% of the true value
# Assuming that the total number of people follows a normal distribution
# with mean = total_estimate and standard deviation = sqrt(N * (1 - n/N) * (1/n) * Σ((y_i)^2 / p_i - y_i))
# where Σ denotes summation over the sample units.
estimated_std_dev <- sqrt(population_size * (1 - sample_size/population_size) * (1/sample_size) * sum((persons_per_household^2) / (population_size/sample_size)))
prob_within_10_percent <- pnorm(upper_bound, mean = total_estimate, sd = estimated_std_dev) - pnorm(lower_bound, mean = total_estimate, sd = estimated_std_dev)

# Display the results
cat("Estimated total number of people in the area:", total_estimate, "\n")
cat("Probability that the estimate is within ±10% of the true value:", prob_within_10_percent, "\n")



#Q.4
#The following table shows the weekly family income (X ) and weekly family expenditure on food (Y) in SRS of 30 low-income families. From this sample estimate the mean weeklyexpenditure on food per family and the percentage of income spent on food.
# Given data
income <- c(620, 620, 870, 650, 580, 920, 880, 790, 830, 620, 630, 620, 600, 750, 900, 750, 690, 830, 850, 730, 660, 580, 770, 690, 650, 770, 690, 950, 770, 690)
food_cost <- c(143, 208, 227, 305, 412, 282, 242, 300, 242, 444, 134, 198, 294, 271, 222, 377, 226, 360, 206, 277, 259, 233, 398, 168, 378, 348, 287, 630, 195, 216)

# Step 1: Calculate the sample mean of weekly expenditure on food
mean_expenditure_on_food <- mean(food_cost)

# Step 2: Calculate the sample mean of weekly family income
mean_income <- mean(income)

# Step 3: Compute the percentage of income spent on food
percentage_spent_on_food <- (mean_expenditure_on_food / mean_income) * 100

# Display the results
cat("Mean weekly expenditure on food per family:", mean_expenditure_on_food, "\n")
cat("Percentage of income spent on food:", percentage_spent_on_food, "%\n")

