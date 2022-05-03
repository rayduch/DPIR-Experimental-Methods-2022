
##### I. SIMULATED T-TESTS OF MEAN DIFFERENCES AND POWER

set.seed(1234)
group1 <- rnorm(30, 1, 2)
group2 <- rnorm(30, 0, 2)


hist(group1, col = "#addd8e", breaks = 10, main = "Histogram of both groups", xlab = "")
hist(group2, add = TRUE, breaks = 10, col= "#31a354")


t.test(group1, group2, paired = FALSE, var.equal = TRUE, conf.level = 0.9)


set.seed(1)
n_sims <- 1000 # we want 1000 simulations
p_vals <- c()
for(i in 1:n_sims){
  group1 <- rnorm(30,1,2) # simulate group 1
  group2 <- rnorm(30,0,2) # simulate group 2
  p_vals[i] <- t.test(group1, group2, paired = FALSE, var.equal = TRUE, conf.level = 0.90)$p.value # run t-test and extract the p-value
}
mean(p_vals < .10) # check power (i.e. proportion of p-values that are smaller than alpha-level of .10)



###### II. SIMULATING NECESSARY N FOR A REQUIRED POWER LEVEL DIFFERENT IN MEANS

set.seed(1)
n_sims <- 1000 # we want 1000 simulations
p_vals <- c()
power_at_n <- c(0) # this vector will contain the power for each sample-size (it needs the initial 0 for the while-loop to work)
cohens_ds <- c()
cohens_ds_at_n <- c() 
n <- 30 # sample-size 
i <- 2
while(power_at_n[i-1] < .95){
  for(sim in 1:n_sims){
    group1 <- rnorm(n,1,2) # simulate group 1
    group2 <- rnorm(n,0,2) # simulate group 2
    p_vals[sim] <- t.test(group1, group2, paired = FALSE, var.equal = TRUE, conf.level = 0.9)$p.value # run t-test and extract the p-value
    cohens_ds[sim] <- abs((mean(group1)-mean(group2))/(sqrt((sd(group1)^2+sd(group2)^2)/2))) # we also save the cohens ds that we observed in each simulation
  }
  power_at_n[i] <- mean(p_vals < .10) # check power (i.e. proportion of p-values that are smaller than alpha-level of .10)
  cohens_ds_at_n[i] <- mean(cohens_ds) # calculate means of cohens ds for each sample-size
  n <- n+1 # increase sample-size by 1
  i <- i+1 # increase index of the while-loop by 1 to save power and cohens d to vector
}
power_at_n <- power_at_n[-1] # delete first 0 from the vector
cohens_ds_at_n <- cohens_ds_at_n[-1] # delete first NA from the vector

plot(30:(n-1), power_at_n, xlab = "Number of participants per group", ylab = "Power", ylim = c(0,1), axes = TRUE)
abline(h = .95, col = "red")


###### III. SIMULATING NECESSARY N FOR A REQUIRED POWER LEVEL BI-VARIARTE REGRESSION


possible.ns <- seq(from=100, to=2000, by=50) # The sample sizes we'll be considering
powers <- rep(NA, length(possible.ns)) # Empty object to collect simulation estimates
alpha <- 0.05 # Standard significance level
sims <- 500 # Number of simulations to conduct for each N 
#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j] # Pick the jth value for N
  significant.experiments <- rep(NA, sims) # Empty object to count significant experiments
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <- rnorm(n=N, mean=60, sd=20) # control potential outcome
    tau <- 5 # Hypothesize treatment effect
    Y1 <- Y0 + tau # treatment potential outcome
    Z.sim <- rbinom(n=N, size=1, prob=.5) # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim) # Reveal outcomes according to assignment
    fit.sim <- lm(Y.sim ~ Z.sim) # Do analysis (Simple regression)
    p.value <- summary(fit.sim)$coefficients[2,4] # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
  }
  powers[j] <- mean(significant.experiments) # store average success rate (power) for each N
}
plot(possible.ns, powers, ylim=c(0,1))



