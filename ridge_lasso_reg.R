#import library for lasso regression
library(glmnet)
library(tidyverse)
library(MASS)
set.seed(123)

#import and clean data
train_data <- read.csv("training.csv")
train_data_clean <- dplyr::select(train_data, -X, -performer, -song, -spotify_genre, -spotify_track_album, -date)
x <- as.matrix(train_data_clean[, names(train_data_clean) != "peak_position"])
y <- as.vector(train_data_clean$peak_position)

#50 runs of 5-fold CV
lambdas <- exp(seq(-5, 5, by=0.5))
n <- nrow(x)
k <- 5
ii <- (1:n) %% k + 1
N <- 50
mspe_lasso <- rep(0, N)
mspe_ridge <- rep(0, N)
mspe_full = rep(0, N)
for (i in 1:N) {
  ii <- sample(ii)
  pred_lasso <- rep(0, n)
  pred_ridge <- rep(0, n)
  pred_full = rep(0, n)
  for (j in 1:k) {
    reg_ridge <- cv.glmnet(
      x = x[ii != j, ], y = y[ii != j], lambda = lambdas,
      nfolds = 5, alpha = 0, family = "gaussian"
    )
    reg_lasso <- cv.glmnet(
      x = x[ii != j, ], y = y[ii != j], lambda = lambdas,
      nfolds = 5, alpha = 1, family = "gaussian"
    )
    
    full = lm(peak_position ~ ., data = train_data_clean[ii != j, ])
    pred_ridge[ii == j] <- predict(reg_ridge, s = "lambda.min", newx = x[ii == j, ])
    pred_lasso[ii == j] <- predict(reg_lasso, s = "lambda.min", newx = x[ii == j, ])
    pred_full[ii == j] <- predict(full, newdata = train_data_clean[ii == j, ])
  }
  mspe_ridge[i] <- mean((y - pred_ridge)^2)
  mspe_lasso[i] <- mean((y - pred_lasso)^2)
  mspe_full[i] <- mean((y - pred_full)^2)
}
boxplot(mspe_ridge, mspe_lasso, mspe_full, names = c("Ridge", "Lasso", "Full"))

#plot of regressions
plot(reg_ridge) 
plot(reg_lasso) 
plot(full)
plot(reg_ridge$glmnet.fit, main = 'Ridge')
abline(v = sum(abs(coef(reg_ridge))))
plot(reg_lasso$glmnet.fit, main = 'Lasso')
abline(v = sum(abs(coef(reg_lasso)))) 

#make predictions with regression models
test_data <- read.csv("test.csv")
test_data_clean <- dplyr::select(test_data, -X, -performer, -song, -spotify_genre, -spotify_track_album, -date)
x_test <- as.matrix(test_data_clean[, names(test_data_clean) != "peak_position"])
y_test <- as.vector(test_data_clean$peak_position)

#evaluate model predicitons
ridge_pred <- predict(reg_ridge, newx = x_test, s = "lambda.min")
lasso_pred <- predict(reg_lasso, newx = x_test, s = "lambda.min")
ridge_mse <- mean((y_test - ridge_pred)^2)
lasso_mse <- mean((y_test - lasso_pred)^2)

ridge_mse
lasso_mse