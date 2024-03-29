setwd("~/Documents/Data analysis projects/Github repos/bayesian-football-match-prediction")
library("rjags")
library('coda')
library(ggplot2)

####### LOADING DATA 
X_train <- read.csv('data/X_train.csv')
y_train <- read.csv('data/y_train.csv')

X_test <- read.csv('data/X_test.csv')
y_test <- read.csv('data/y_test.csv')

X_future <- read.csv('data/X_future.csv')

# Adding a constant term
X_train <- cbind(1, X_train)
X_test <- cbind(1, X_test)
X_future <- cbind(1, X_future)

print(length(X_train))
print(length(X_test))
print(length(X_future))
###### MODEL TRAINING
model_string <- 'model{
  for(i in 1:N){
    y[i] ~ dbern(p[i])
    logit(p[i]) <- eta[i]
    eta[i] <- inprod(X[i,], beta[]) # linear predictors using inner product notation.
  }
  
  # Weakly informative priors for coefficients
  for(j in 1:P){
    beta[j] ~ dnorm(0, 0.001)
  }
}'


writeLines(model_string , con="model.txt" )

data_list <- list(
  N = nrow(X_train),
  P = ncol(X_train),  
  y = as.numeric(y_train[,1]),
  X = as.matrix(X_train)
)

jags_model <- jags.model(file = "model.txt",
                        data = data_list,
                        n.chains = 2,
                        n.adapt = 1000)

samples <- coda.samples(jags_model,
                        variable.names = c("beta"),
                        n.iter = 10000,
                        thin = 5
                        )

save(samples, file = "posterior_samples.RData")

gelman_result <- gelman.diag(samples)
print(gelman_result)

###### SUMMARY 
summary(samples)
plot(samples)

###### TEST SET PREDICTIONS
#load("posterior_samples.RData")

posteria_means <- colMeans(as.matrix(samples))

eta <- as.matrix(X_test) %*% posteria_means
p_pred <- plogis(eta) # same as 1 / (1 + exp(-eta)) or exp(eta) / (1 + exp(eta))
predictions <- ifelse(p_pred > 0.5, 1, 0)

accuracy <- sum(predictions == y_test) / length(y_test)
precision <- sum(predictions & y_test) / sum(predictions)
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))

##### FUTURE MATCHDAY 30 PREDICTIONS

eta_future <- as.matrix(X_future) %*% posteria_means
p_pred_future <- plogis(eta_future) # same as 1 / (1 + exp(-eta)) or exp(eta) / (1 + exp(eta))
predictions_future <- ifelse(p_pred_future > 0.5, 1, 0)

### By team
a <- as.matrix(X_future[7,]) %*% t(as.matrix(samples))
b <- as.matrix(X_future[8,]) %*% t(as.matrix(samples))

p_pred_a <- plogis(a)
p_pred_b <- plogis(b)

team_a <- 'Liverpool'
team_b <- 'Brighton'
predictions_df <- data.frame(
  PredictedProbability = c(p_pred_a, p_pred_b),
  Team = factor(rep(c(team_a, team_b), each = length(p_pred_a)))
)

title <- paste("Predicted Probabilities of Winning")

ggplot(predictions_df, aes(x = PredictedProbability, fill = Team)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = title,
       x = "Predicted Probability",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.title = element_blank())

