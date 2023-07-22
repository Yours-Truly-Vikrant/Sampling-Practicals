#Generate random sample from the finite population using i)SRSWR ii)SRSWOR iii) Unequal probabilities for population elements.

#SRSWR
population=c(1:100) #finite population
sample_size <- 100 #sample size
sample_srs_wr <- sample(population, sample_size, replace = TRUE);#sample drawing 
sample_srs_wr

#SRSWOR
sample_size <- 100
sample_srs_wor <- sample(population, sample_size, replace = FALSE)
sample_srs_wor

#With unequal probabilities.
sample_size <- 100
probabilities <- seq(0.01,1,0.01)
sample_unequal_prob <- sample(population, sample_size, replace = TRUE, prob = probabilities)
sample_unequal_prob



#Q.2 Generate random sample of size 10,000 from Poisson(𝜆=3). Treat this generated sample data as characteristic values of study variables of the population of the size 10,000. Now, draw 5 random samples of size 100 each from this population using SRSWOR, obtain estimate of population mean based on each sample data by using sample mean as an estimator for population mean. Also find the amount of sampling error from your estimates.
#Generate the initial random sample of size 10,000 from Poisson distribution
set.seed(123)  # For reproducibility
population_size <- 10000
population_lambda <- 3
population <- rpois(population_size, lambda = population_lambda)

#Draw 5 random samples of size 100 each from the population (SRSWOR)
sample_size <- 100
num_samples <- 5
sample_means <- vector("numeric", num_samples)

for (i in 1:num_samples) {
  sample <- sample(population, size = sample_size, replace = FALSE)
  sample_mean <- mean(sample)
  sample_means[i] <- sample_mean
}

#Calculate the estimates of the population mean using the sample means
population_mean_estimate <- mean(sample_means)

#Find the amount of sampling error for each estimate
sampling_errors <- population_mean_estimate - sample_means

# Print the results
cat("True Population Mean:", mean(population), "\n")
cat("Estimated Population Mean:", population_mean_estimate, "\n")
cat("Sampling Errors for Each Estimate:", sampling_errors, "\n")


