# Given data
population_size <- 2000
eps <- 20
confidence_level <- 0.95
estimate_of_variance <- 10000

# Calculate the critical z-value for the desired confidence level
z <- qnorm(1 - (1 - confidence_level) / 2)

# Calculate the sample size
sample_size <- ceiling((z * sqrt(estimate_of_variance) / eps)^2)

# Display the result
cat("The sample size needed is:", sample_size, "\n")
