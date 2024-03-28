library("rjags")
library('coda')

####### LOADING DATA 
X_train <- read.csv('data/X_train.csv')
y_train <- read.csv('data/y_train.csv')

X_test <- read.csv('data/X_test.csv')
y_test <- read.csv('data/y_test.csv')

# Adding a constant term
X_train <- cbind(1, X_train)
X_test <- cbind(1, X_test)

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
                        thin = 10
                        )

save(samples, file = "posterior_samples.RData")
posterior_df <- as.data.frame(as.mcmc.list(samples))
write.csv(posterior_df, "posterior_samples.csv", row.names = FALSE)

gelman_result <- gelman.diag(samples)
print(gelman_result)

###### SUMMARY 
summary(samples)
plot(samples)


###### TEST SET PREDICTIONS
#load("posterior_samples.RData")

predict_proba <- function(beta_samples, X_row) {
  if (is.vector(X_row)) {
    X_row <- matrix(X_row, nrow = 1)
  }
  eta_samples <- as.matrix(beta_samples) %*% t(X_row) # calc linear predictors.
  predicted_probabilities <- 1 / (1 + exp(-eta_samples))
  return(predicted_probabilities)
}

probabilities_row1 <- predict_proba(beta_samples, X_test[1, ])
probabilities_row2 <- predict_proba(beta_samples, X_test[2, ])



y_test <- filtered_test_data$target
TP_encoded_test <- filtered_test_data$TP_encoded
OP_encoded_test <- filtered_test_data$OP_encoded
is_home_test <- filtered_test_data$is_home
FT_TP_G_rolling_mean_test <- filtered_test_data$FT_TP_G_rolling_mean
FT_OP_G_rolling_mean_test <- filtered_test_data$FT_OP_G_rolling_mean
target_rolling_mean_test <- filtered_test_data$target_rolling_mean


logit_p_1 <- samples[[1]][,1] + samples[[1]][,2]*TP_encoded_test[2]+samples[[1]][,3]*OP_encoded_test[2]+samples[[1]][,4]*is_home_test[2]+samples[[1]][,5]*FT_OP_G_rolling_mean_test[2]+samples[[1]][,6]*target_rolling_mean_test[2]
logit_p_2 <- samples[[1]][,1] + samples[[1]][,2]*TP_encoded_test[3]+samples[[1]][,3]*OP_encoded_test[3]+samples[[1]][,4]*is_home_test[3]+samples[[1]][,5]*FT_OP_G_rolling_mean_test[3]+samples[[1]][,6]*target_rolling_mean_test[3]

pred_1 = exp(logit_p_1)/(1+exp(logit_p_1))
pred_2 = exp(logit_p_2)/(1+exp(logit_p_2))

library(ggplot2)

pred_df = data.frame(pred_1, pred_2)
names(pred_df) = c('home', 'away')


ggplot(pred_df) +
  geom_histogram(aes(x = pred_1, y = ..density.., color = 'home_team', fill='home_team'),
                 bins = 30, alpha=0.5) +
  geom_histogram(aes(x = pred_2, y = ..density.., color = 'away_team', fill='away_team' ),
                 bins = 30, alpha=0.5)

##### FUTURE MATCHDAY 30 PREDICTIONS

