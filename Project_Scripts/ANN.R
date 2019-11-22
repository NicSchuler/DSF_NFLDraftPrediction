library(h2o)
library(tidyverse)

# init ----

# Create sigmoid function
sigmoid <- function(z){
  out <- 1 / (1 + exp(-z))
  return(out)
}

# Create standardization function
standFun <- function(x){
  out <- (x - mean(x))/sd(x)
  return(out)
}

# Load cleaned data
load("../Data/CleanData/CleanClass2007to2014_2.RData")
cleanData <- as_tibble(CleanClass2007to2014_2)

# Define the ANN cost function
ANN_cost <- function(ANN_par, L_i_size, L_h_size, L_o_size, x, y, lambda){

  # Separate the ANN_par matrix back into the two thetas
  theta1 <- matrix(ANN_par[1:(L_h_size * (L_i_size + 1))], nrow = L_h_size)
  theta2 <- matrix(ANN_par[(1 + (L_h_size * (L_i_size + 1))):length(ANN_par)], nrow = L_o_size)
  
  # Prepare gradients to return to optimization function
  J <- 0
  theta1_grad <- matrix(0, nrow(theta1), ncol(theta1))
  theta2_grad <- matrix(0, nrow(theta2), ncol(theta2))
  
  # Create output matrix
  y_new <- matrix(0, n, L_o_size)
  
  # Map output vector to binary vector
  for (i in 1:n) { 
    y_new[i, y[i]] <- 1
  }
  
  # Add column of 1s for bias
  x <- cbind(1, x)
  
  # Compute the output for the current thetas
  H1 <- sigmoid(x %*% t(theta1))
  H2 <- cbind(rep(1,n), H1)
  H <- sigmoid(H2 %*% t(theta2))
  
  # Compute the log-likelihood for optimization
  J <- vector()
  
  for (i in 1:n){
    J[i] <- (sum(-y_new[i,] %*% log(H[i,]) - (1-y_new[i,]) %*% log(1-H[i,]))) +
      (lambda/(2*n)) * (sum(sum(theta1[,2:dim(theta1)[2] ]^2)) +  sum(sum(theta2[,2:dim(theta2)[2]]^2)) ) 
  }
  
  J_new <- sum(J)/n
  J <- J_new
}

# Define the function to calculate the gradient of the cost function

ANN_grad <- function(ANN_par, L_i_size, L_h_size, L_o_size, x, y, lambda){
  
  # Separate the ANN_par matrix back into the thetas
  theta1 <- matrix(ANN_par[1:(L_h_size * (L_i_size + 1))], nrow = L_h_size)
  theta2 <- matrix(ANN_par[(1 + (L_h_size * (L_i_size + 1))):length(ANN_par)], nrow = L_o_size)
  
  # Create placeholder matrices for the gradients
  theta1_grad <- matrix(0, nrow(theta1), ncol(theta1))
  theta2_grad <- matrix(0, nrow(theta2), ncol(theta2))
  
  # Create output matrix
  y_new <- matrix(0, n, L_o_size)
  
  # Map y vector to binary vector
  for (i in 1:n) { 
    y_new[i, y[i]] <- 1
  }
  
  # Add column of 1s to the input matrix for bias
  x <- cbind(1, x)
  
  # Create placeholders for gradient calculation
  a_2 <- matrix(0, L_h_size, 1)
  a_2 <- rbind( 1 , a_2)
  a_3 <- matrix(0, n, L_o_size)
  
  z_2 <- matrix(0, L_i_size, 1)
  z_3 <- matrix(0, L_h_size, 1)
  
  D1 <- matrix(0, L_h_size, L_i_size + 1)
  D2 <- matrix(0, L_o_size, L_h_size + 1)
  
  # Calculate negative gradient for every input
  for (t in 1:n) {
    a_1 <- x[t,]
    z_2 <- theta1 %*% a_1
    a_2 <- rbind(1, sigmoid(z_2))
    
    z_3 <- theta2 %*% a_2
    a_3 <- sigmoid(z_3)
    
    delta_3 <- a_3 - y_new[t,]
    
    tmp <- (t(theta2) %*% delta_3)
    
    delta_2 <- tmp[2:length(tmp)] * sigmoid(z_2) * (1 - sigmoid(z_2))
    
    D1 <- D1 + delta_2 %*% a_1
    D2 <- D2 + delta_3 %*% t(a_2)   
  }
  
  theta1_grad <- D1/n
  theta1_grad[,2:ncol(theta1_grad)] <- theta1_grad[, 2:ncol(theta1_grad)] + (lambda/n) * theta1[, 2:ncol(theta1_grad)]
  theta2_grad <- D2/n
  theta2_grad[,2:ncol(theta2_grad)] <- theta2_grad[, 2:ncol(theta2_grad)] + (lambda/n) * theta2[, 2:ncol(theta2_grad)]
  
  grad <- c(as.vector(theta1_grad), as.vector(theta2_grad))
}

# ANN for QB ----

# Select QBs and remove any players with missing values
cleanData_QB <- cleanData %>% filter(., Position == "QB") %>% drop_na(.)

# Select years 2007 through 2013 as training data
x <- as.matrix(cleanData_QB %>% filter(., Year < 2014) %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted))
y <- pull(cleanData_QB %>% filter(., Year < 2014) %>% select(., Drafted))

# Standardize the training data
for (i in 1:ncol(x)) {
  x[,i] <- standFun(x[,i])
}

# Replace variables that are consistently zero and therefore yield NA when standardized with zero again
x[is.na(x)] <- 0

# Parameters of ANN for QBs
L_i_size <- ncol(x)
L_h_size <- 10
L_o_size <-  1
n <- length(y)
theta1 <- matrix(1, nrow = L_h_size, ncol = L_i_size + 1)
theta2 <- matrix(1, nrow = L_o_size, ncol = L_h_size + 1)
ANN_par <- c(as.vector(theta1), as.vector(theta2))

# Make verbose to print every 5th iteration
options <- list(trace = 5)

# Optimize cost function
BP_pred <- nlminb(start = ANN_par,
                 objective = ANN_cost, 
                 gradient = ANN_grad,
                 hessian = NULL,
                 L_i_size = L_i_size,
                 L_h_size = L_h_size,
                 L_o_size = L_o_size,
                 x = x, y = y,
                 lambda = 1,
                 control = options)

# Retrieve the theta vector found through backpropagation
BP_par <- BP_pred$par

# Separate theta vector from backpropagation into thetas
theta1_train <- matrix(data = BP_par[1:(L_h_size * (L_i_size + 1))], nrow = L_h_size)
theta2_train <- matrix(data = BP_par[(1 + (L_h_size * (L_i_size + 1))):length(BP_par)], nrow = L_o_size)

# Take the year 2014 as testing data
cleanData_QB_test <- cleanData %>% filter(., Position == "QB", Year == 2014) %>% drop_na(.)
x_test <- as.matrix(cleanData_QB_test %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted))
y_test <- pull(cleanData_QB_test %>% select(., Drafted))

# Standardize the testing data and replace NA with zero
for (i in 1:ncol(x_test)) {
  x_test[,i] <- standFun(x_test[,i])
}
x_test[is.na(x_test)] <- 0

# Make predictions with the thetas found in backpropagation
a_1 <- rbind(1, t(x_test))
a_2 <- rbind(1, sigmoid(theta1_train %*% a_1))
a_3 <- sigmoid(theta2_train %*% a_2)

# Summarize the results
pred_QB <- tibble("Code" = cleanData_QB_test$Player.Code, "Name" = cleanData_QB_test$Name, "pred" = as.vector(a_3), "Drafted" = cleanData_QB_test$Drafted)

# ANN for RB ----

# Select RBs and remove any players with missing values
cleanData_RB <- cleanData %>% filter(., Position == "RB") %>% drop_na(.)

# Select years 2007 through 2013 as training data
x <- as.matrix(cleanData_RB %>% filter(., Year < 2014) %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted))
y <- pull(cleanData_RB %>% filter(., Year < 2014) %>% select(., Drafted))

# Standardize the training data
for (i in 1:ncol(x)) {
  x[,i] <- standFun(x[,i])
}

# Replace variables that are consistently zero and therefore yield NA when standardized with zero again
x[is.na(x)] <- 0

# Parameters of ANN for RBs
L_i_size <- ncol(x)
L_h_size <- 10
L_o_size <-  1
n <- length(y)
theta1 <- matrix(1, nrow = L_h_size, ncol = L_i_size + 1)
theta2 <- matrix(1, nrow = L_o_size, ncol = L_h_size + 1)
ANN_par <- c(as.vector(theta1), as.vector(theta2))

# Make verbose to print every 5th iteration
options <- list(trace = 5)

# Optimize cost function
BP_pred <- nlminb(start = ANN_par,
                  objective = ANN_cost, 
                  gradient = ANN_grad,
                  hessian = NULL,
                  L_i_size = L_i_size,
                  L_h_size = L_h_size,
                  L_o_size = L_o_size,
                  x = x, y = y,
                  lambda = 1,
                  control = options)

# Retrieve the theta vector found through backpropagation
BP_par <- BP_pred$par

# Separate theta vector from backpropagation into thetas
theta1_train <- matrix(data = BP_par[1:(L_h_size * (L_i_size + 1))], nrow = L_h_size)
theta2_train <- matrix(data = BP_par[(1 + (L_h_size * (L_i_size + 1))):length(BP_par)], nrow = L_o_size)

# Take the year 2014 as testing data
cleanData_RB_test <- cleanData %>% filter(., Position == "RB", Year == 2014) %>%  drop_na(.)
x_test <- as.matrix(cleanData_RB_test %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted))
y_test <- pull(cleanData_RB_test %>% select(., Drafted))

# Standardize the testing data and replace NA with zero
for (i in 1:ncol(x_test)) {
  x_test[,i] <- standFun(x_test[,i])
}
x_test[is.na(x_test)] <- 0

# Make predictions with the thetas found in backpropagation
a_1 <- rbind(1, t(x_test))
a_2 <- rbind(1, sigmoid(theta1_train %*% a_1))
a_3 <- sigmoid(theta2_train %*% a_2)

# Summarize the results
pred_RB <- tibble("Code" = cleanData_RB_test$Player.Code, "Name" = cleanData_RB_test$Name, "pred" = as.vector(a_3), "Drafted" = cleanData_RB_test$Drafted)

# ANN for WR ----

# Select WRs and remove any players with missing values
cleanData_WR <- cleanData %>% filter(., Position == "WR") %>% drop_na(.)

# Select years 2007 through 2013 as training data
x <- as.matrix(cleanData_WR %>% filter(., Year < 2014) %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted))
y <- pull(cleanData_WR %>% filter(., Year < 2014) %>% select(., Drafted))

# Standardize the training data
for (i in 1:ncol(x)) {
  x[,i] <- standFun(x[,i])
}

# Replace variables that are consistently zero and therefore yield NA when standardized with zero again
x[is.na(x)] <- 0

# Parameters of ANN for WRs
L_i_size <- ncol(x)
L_h_size <- 10
L_o_size <-  1
n <- length(y)
theta1 <- matrix(1, nrow = L_h_size, ncol = L_i_size + 1)
theta2 <- matrix(1, nrow = L_o_size, ncol = L_h_size + 1)
ANN_par <- c(as.vector(theta1), as.vector(theta2))

# Make verbose to print every 5th iteration
options <- list(trace = 5)

# Optimize cost function
BP_pred <- nlminb(start = ANN_par,
                  objective = ANN_cost, 
                  gradient = ANN_grad,
                  hessian = NULL,
                  L_i_size = L_i_size,
                  L_h_size = L_h_size,
                  L_o_size = L_o_size,
                  x = x, y = y,
                  lambda = 1,
                  control = options)

# Retrieve the theta vector found through backpropagation
BP_par <- BP_pred$par

# Separate theta vector from backpropagation into thetas
theta1_train <- matrix(data = BP_par[1:(L_h_size * (L_i_size + 1))], nrow = L_h_size)
theta2_train <- matrix(data = BP_par[(1 + (L_h_size * (L_i_size + 1))):length(BP_par)], nrow = L_o_size)

# Take the year 2014 as testing data
cleanData_WR_test <- cleanData %>% filter(., Position == "WR", Year == 2014) %>% drop_na(.)
x_test <- as.matrix(cleanData_WR_test %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted))
y_test <- pull(cleanData_WR_test %>% select(., Drafted))

# Standardize the testing data and replace NA with zero
for (i in 1:ncol(x_test)) {
  x_test[,i] <- standFun(x_test[,i])
}
x_test[is.na(x_test)] <- 0

# Make predictions with the thetas found in backpropagation
a_1 <- rbind(1, t(x_test))
a_2 <- rbind(1, sigmoid(theta1_train %*% a_1))
a_3 <- sigmoid(theta2_train %*% a_2)

# Summarize the results
pred_WR <- tibble("Code" = cleanData_WR_test$Player.Code, "Name" = cleanData_WR_test$Name, "pred" = as.vector(a_3), "Drafted" = cleanData_WR_test$Drafted)

# ANN for all positions ----

# Remove any players with missing values
cleanData_all <- cleanData %>% drop_na(.)

# Select years 2007 through 2013 as training data
x <- as.matrix(cleanData_all %>% filter(., Year < 2014) %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted))
y <- pull(cleanData_all %>% filter(., Year < 2014) %>% select(., Drafted))

# Standardize the training data
for (i in 1:ncol(x)) {
  x[,i] <- standFun(x[,i])
}

# Replace variables that are consistently zero and therefore yield NA when standardized with zero again
x[is.na(x)] <- 0

# Parameters of ANN
L_i_size <- ncol(x)
L_h_size <- 10
L_o_size <-  1
n <- length(y)
theta1 <- matrix(1, nrow = L_h_size, ncol = L_i_size + 1)
theta2 <- matrix(1, nrow = L_o_size, ncol = L_h_size + 1)
ANN_par <- c(as.vector(theta1), as.vector(theta2))

# Make verbose to print every 5th iteration
options <- list(trace = 5)

# Optimize cost function
BP_pred <- nlminb(start = ANN_par,
                  objective = ANN_cost, 
                  gradient = ANN_grad,
                  hessian = NULL,
                  L_i_size = L_i_size,
                  L_h_size = L_h_size,
                  L_o_size = L_o_size,
                  x = x, y = y,
                  lambda = 1,
                  control = options)

# Retrieve the theta vector found through backpropagation
BP_par <- BP_pred$par

# Separate theta vector from backpropagation into thetas
theta1_train <- matrix(data = BP_par[1:(L_h_size * (L_i_size + 1))], nrow = L_h_size)
theta2_train <- matrix(data = BP_par[(1 + (L_h_size * (L_i_size + 1))):length(BP_par)], nrow = L_o_size)

# Take the year 2014 as testing data
cleanData_all_test <- cleanData %>% filter(., Year == 2014) %>% drop_na(.)
x_test <- as.matrix(cleanData_all_test %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted))
y_test <- pull(cleanData_all_test %>% select(., Drafted))

# Standardize the testing data and replace NA with zero
for (i in 1:ncol(x_test)) {
  x_test[,i] <- standFun(x_test[,i])
}
x_test[is.na(x_test)] <- 0

# Make predictions with the thetas found in backpropagation
a_1 <- rbind(1, t(x_test))
a_2 <- rbind(1, sigmoid(theta1_train %*% a_1))
a_3 <- sigmoid(theta2_train %*% a_2)

# Summarize the results
pred_all <- tibble("Code" = cleanData_all_test$Player.Code, "Name" = cleanData_all_test$Name, "pred" = as.vector(a_3), "Drafted" = cleanData_all_test$Drafted)

# Aggregate Results ----
resultsComb_separate <- tibble("Player.Code" = c(pred_QB$Code, pred_RB$Code, pred_WR$Code),
                          "Name" = c(pred_QB$Name, pred_RB$Name, pred_WR$Name),
                          "P" = c(pred_QB$pred, pred_RB$pred, pred_WR$pred),
                          "Pred" = ifelse(c(pred_QB$pred, pred_RB$pred, pred_WR$pred) >= 0.5, 1, 0),
                          "Drafted" = c(pred_QB$Drafted, pred_RB$Drafted, pred_WR$Drafted))
resultsComb_separate <- resultsComb_separate %>% mutate(., "error" = ifelse(Pred != Drafted, 1, 0),
                                              "TP" = ifelse(Pred == Drafted & Drafted == 1, 1, 0),
                                              "FP" = ifelse(Pred != Drafted & Drafted == 0, 1, 0),
                                              "TN" = ifelse(Pred == Drafted & Drafted == 0, 1, 0),
                                              "FN" = ifelse(Pred != Drafted & Drafted == 1, 1, 0))

resultsComb_all <- tibble("Player.Code" = pred_all$Code,
                          "Name" = pred_all$Name,
                          "P" = pred_all$pred,
                          "Pred" = ifelse(pred_all$pred >= 0.5, 1, 0),
                          "Drafted" = pred_all$Drafted)
resultsComb_all <- resultsComb_all %>% mutate(., "error" = ifelse(Pred != Drafted, 1, 0),
                                              "TP" = ifelse(Pred == Drafted & Drafted == 1, 1, 0),
                                              "FP" = ifelse(Pred != Drafted & Drafted == 0, 1, 0),
                                              "TN" = ifelse(Pred == Drafted & Drafted == 0, 1, 0),
                                              "FN" = ifelse(Pred != Drafted & Drafted == 1, 1, 0))

