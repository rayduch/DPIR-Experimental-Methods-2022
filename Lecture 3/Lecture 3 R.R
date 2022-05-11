
require(ggplot2)

# Define two helper functions
complete_ra <- function(N,m){
  assign <- ifelse(1:N %in% sample(1:N,m),1,0)
  return(assign)
}

get_condition <- function(assign, adjmat){
  exposure <-  adjmat %*% assign
  condition <- rep("00", length(assign))
  condition[assign==1 & exposure==0] <- "10"
  condition[assign==0 & exposure>0] <- "01"
  condition[assign==1 & exposure>0] <- "11"
  return(condition)
}


N <- 50  # total units
m <- 10  # Number to be treated

# Generate adjacency matrix
set.seed(343)
coords <- matrix(rnorm(N*2)*10, ncol = 2)
distmat <- as.matrix(dist(coords))
true_adjmat <- 1 * (distmat<=5) # true radius = 5
diag(true_adjmat) <- 0

# Run simulation 10000 times
Z_mat <- replicate(10000, complete_ra(N = N, m = m))
cond_mat <- apply(Z_mat, 2, get_condition, adjmat=true_adjmat)

# Calculate assignment probabilities
prob00 <- rowMeans(cond_mat=="00")
prob01 <- rowMeans(cond_mat=="01")
prob10 <- rowMeans(cond_mat=="10")
prob11 <- rowMeans(cond_mat=="11")

# calculate number of villages
vil_sum = rowSums(true_adjmat)

probs = cbind(prob00, prob01, prob10, prob11,vil_sum)
probs2 = as.data.frame(probs)
probs2 = as.matrix(probs2)
probs3 = rbind(probs2[,c(1,5)], probs2[,c(2,5)],
               probs2[,c(3,5)], probs2[,c(4,5)])
probs3 = as.data.frame(probs3)
probs3$ProbCat = c(rep("prob00",50), rep("prob01",50), rep("prob10",50), rep("prob11",50))
probs3$ProbCat = factor(probs3$ProbCat)

p = ggplot(probs3, aes(y = prob00, x=vil_sum))
p
p = p + geom_point()
p
p = p + geom_point(aes(colour = ProbCat))
p
p = p + labs(x = "Number of Nearby Villages within Radius")
p 
p = p + labs(y = "Probability of Treatment Condition")
p

  











