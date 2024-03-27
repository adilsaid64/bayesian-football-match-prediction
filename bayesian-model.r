library("rjags")

X_train <- read.csv('data/X_train.csv')
y_train <- read.csv('data/y_train.csv')

X_test <- read.csv('data/X_test.csv')
y_test <- read.csv('data/y_test.csv')

# JAGS model
model_string <- 'model{
  for(i in 1:N){
    y[i] ~ dbern(p[i])
    logit(p[i]) <- eta[i]
    eta[i] <- inprod(X[i,], beta[])
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
                        n.adapt = 300)

#update(jags_model, n.iter=1000)

samples <- coda.samples(jags_model,
                        variable.names = c("beta"),
                        n.iter = 5000,
                        thin = 10)
summary(samples)

plot(samples)


# making predictions

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

