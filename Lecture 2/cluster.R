treatment_effect     <- 1
# Define the individual ids (i)
person               <- 1:10
# Define the cluster indicator (j)
hair_color           <- c(rep("black",5),rep("brown",5))
# Define the control outcome (Y0)
outcome_if_untreated <- rnorm(n = 10)
# Define the treatment outcome (Y1)
outcome_if_treated   <- outcome_if_untreated + treatment_effect

# Version 1 - Not cluster randomized
# Generate all possible non-clustered assignments of treatment (Z)
non_clustered_assignments <- combn(x = unique(person),m = 5)
# Estimate the treatment effect
treatment_effects_V1 <-
  apply(
    X = non_clustered_assignments,
    MARGIN = 2,
    FUN = function(assignment) {
      treated_outcomes   <- outcome_if_treated[person %in% assignment]
      untreated_outcomes <- outcome_if_untreated[!person %in% assignment]
      mean(treated_outcomes) - mean(untreated_outcomes)
    }
  )
# Estimate the true standard error
standard_error_V1 <- sd(treatment_effects_V1)
# Plot the histogram of all possible estimates of the treatment effect
hist(treatment_effects_V1,xlim = c(-1,2.5),breaks = 20)


# Version 2 - Cluster randomized
# Generate all possible assignments of treatment when clustering by hair color (Z)
clustered_assignments     <- combn(x = unique(hair_color),m = 1)
# Estimate the treatment effect
treatment_effects_V2 <-
  sapply(
    X = clustered_assignments,
    FUN = function(assignment) {
      treated_outcomes   <- outcome_if_treated[person %in% person[hair_color==assignment]]
      untreated_outcomes <- outcome_if_untreated[person %in% person[!hair_color==assignment]]
      mean(treated_outcomes) - mean(untreated_outcomes)
    }
  )
# Estimate the true standard error
standard_error_V2 <- sd(treatment_effects_V2)
# Plot the histogram of all possible estimates of the treatment effect
hist(treatment_effects_V2,xlim = c(-1,2.5),breaks = 20)


