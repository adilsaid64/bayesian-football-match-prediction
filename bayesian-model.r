setwd("~/Documents/Data analysis projects/Github repos/bayesian-football-match-prediction")
library(rjags)
library(coda)
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
#logit(p[i]) <- eta[i]#beta[1] + beta[2]*x1[i] + beta[3]*x2[i] + beta[4]*x3[i] + beta[5]*x4[i]

###### MODEL TRAINING
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
}
'
writeLines(model_string , con="model.txt" )

N <- nrow(X_train)
P <- ncol(X_train)
data_list <- list(
  N = N,
  P = P,  
  y = as.numeric(y_train[,1]),
  X = as.matrix(X_train)
)


logit_model <- glm(as.numeric(y_train[,1]) ~ ., data = X_train[,-1], family = "binomial")
print(summary(logit_model))

init1 <- list(beta = matrix(logit_model$coefficients, nrow = P)[,1])
init2 <- list(beta = matrix(logit_model$coefficients + 0.01, nrow = P)[,1]) 
inits <- list(init1, init2)


jags_model <- jags.model(file = "model.txt",
                         inits = inits,
                        data = data_list,
                        n.chains = 2,
                        n.adapt = 5000)

jags_model$state()
update(jags_model, n.iter=10000)
samples <- coda.samples(jags_model,
                        variable.names = c("beta"),
                        n.iter = 100000,
                        thin = 20
                        )

save(samples, file = "posterior_samples.RData")

# Model Validation
gelman_result <- gelman.diag(samples)
print(gelman_result)

chain1 <- samples[[1]]
chain2 <- samples[[2]]

plot_convergence <- function(chain1, chain2){
  for(i in 1:7){
    vec1 <- cumsum(chain1[,i])
    vec2<-cumsum(chain2[,i])
    denomi <- 1:length(vec1)
    df <- data.frame(vec1/denomi,vec2/denomi)
    matplot(df, type = "l", col = c("black", "red"), main = paste("beta",(i)), 
            ylab = "value", lty = "solid")
    legend("topright", legend = c("chain 2", "chain 1"),
           lwd = 3, col = c("red", "black"), bg="transparent")
  }
}

plot_convergence(chain1, chain2)

autocorr_results <- autocorr.diag(samples)
print(autocorr_results) # Auto Corr present in some params. Like beta[1].autocorr.plot(samples)

effectiveSize(samples)
###### SUMMARY 
summary(samples)
plot(samples)


posteria_means <- colMeans(as.matrix(samples))
posterior_medians <- apply(as.matrix(samples), 2, median)


###### TEST SET PREDICTIONS
#load("posterior_samples.RData")

eta <- as.matrix(X_test) %*% posteria_means

p_pred <- plogis(eta) # same as 1 / (1 + exp(-eta)) or exp(eta) / (1 + exp(eta))
y_pred <- ifelse(p_pred > 0.5, 1, 0)
accuracy <- sum(y_pred == y_test) / nrow(y_test)
precision <- sum(y_pred & y_test) / sum(y_pred)
recall <- sum(y_pred & y_test) / sum(y_test)
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))


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

title <- paste("Predicted Probabilities for", team_a, "vs.", team_b, "Winning")

ggplot(predictions_df, aes(x = PredictedProbability, fill = Team)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = title,
       x = "Predicted Probability",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.title = element_blank())

