library("rjags")

train_data <- read.csv('data/train_encoded.csv')
test_data <- read.csv('data/test_encoded.csv')

# Feature that will be used and what they represent: 
# TP_encoded - The team playing  id
# OP_encoded - The opposition team id
# is_home - If the team playing is home or not.
# FT_TP_G_rolling_mean - Last 3 games average of goals scored for the team playing.
# FT_OP_G_rolling_mean - Last 3 games average if goals conceded for the team playing.
# target_rolling_mean - Last 3 games average results for the team playing.

features = c("TP_encoded", "OP_encoded", "is_home", "FT_TP_G_rolling_mean", "FT_OP_G_rolling_mean", "target_rolling_mean", "target") 
filtered_train_data <- train_data[features]
filtered_test_data <- test_data[features]

# JAGS model
model_string = "
model{
  for(i in 1:N){
    y[i] ~ dbern(p[i])
    logit(p[i]) <- beta[1] + beta[2]*TP_encoded[i] + beta[3]*OP_encoded[i] + beta[4]*is_home[i] + beta[5]*FT_TP_G_rolling_mean[i] + beta[6]*FT_OP_G_rolling_mean[i] + beta[7]*target_rolling_mean[i]
  }
  
  for(j in 1:P){
    beta[j] ~ dnorm(0, 0.001)
  }
}
"
writeLines(model_string , con="model.txt" )


y <- filtered_train_data$target
TP_encoded <- filtered_train_data$TP_encoded
OP_encoded <- filtered_train_data$OP_encoded
is_home <- filtered_train_data$is_home
FT_TP_G_rolling_mean <- filtered_train_data$FT_TP_G_rolling_mean
FT_OP_G_rolling_mean <- filtered_train_data$FT_OP_G_rolling_mean
target_rolling_mean <- filtered_train_data$target_rolling_mean
N <- length(y)
P = 7

jags_data <- list(
  N = N,
  P = P,
  y = y,
  TP_encoded = TP_encoded,
  OP_encoded = OP_encoded,
  is_home = is_home,
  FT_TP_G_rolling_mean = FT_TP_G_rolling_mean,
  FT_OP_G_rolling_mean = FT_OP_G_rolling_mean,
  target_rolling_mean = target_rolling_mean
)


jags_model <- jags.model(file = "model.txt",
                        data = jags_data,
                        n.chains = 2,
                        n.adapt = 300)

update(jags_model, n.iter=1000)

samples <- coda.samples(jags_model,
                        variable.names = c("beta"),
                        n.iter = 10000,
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

