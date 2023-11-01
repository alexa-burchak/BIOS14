# Summary Statistics 
log(1.1/1)
-log(1.1/1)

# Simulating Data from Statistical Distributions 
x = rnorm(n=100, mean=5, sd=1)
mean(x)
sd(x)
hist(x,las=1,main="")

# Bootstrapping
set.seed(1) # setting seed ensures reproducibility
x = rnorm(50, 10, 2)
se_x = sqrt(var(x)/length(x)) # calculate standard error of the mean 
out = NULL # store results of each loop iteration 
for(i in 1:1000){ # nonparametric bootstrap, resample the data many times 
  sample = sample(x, replace=TRUE)
  out[i] = mean(sample)
}
hist(out, las=1, main="")
sd(out)
se_x

# 95% confidence interval
quantile(out, c(0.025,0.975)) 
mean(x) - 1.96*se_x
mean(x) + 1.96*se_x

# Exercise 
set.seed(1) 
x = rnorm(5000, 120, 2)
se_x = sqrt(var(x)/length(x)) 
out = NULL 
for(i in 1:10000){ 
  sample = sample(x, replace=TRUE)
  meansamp = mean(sample)
  sdsamp = sd(sample)
  out[i] = sdsamp/meansamp
}
quantile(out, c(0.025,0.975)) # calculate 95% confidence interval

# Optional Exercise 
set.seed(1)
# Store results
sd_log <- NULL
cv <- NULL

for (i in 1:1000) {
  x <- rnorm(1000,120,2) # Simulate data
  cv[i] <- sd(x) / mean(x) # Calculate CV
  sd_log[i] <- sd(log(x)) # Calculate SD of log-transformed data
}

# Create a scatterplot to show the relationship
plot(cv, sd_log, xlab = "CV(x)", ylab = "SD(log[x])")

# Add a trendline
abline(lm(sd_log ~ cv), col = "red")