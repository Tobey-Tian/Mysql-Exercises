remove(list = ls())
set.seed(1234)
#problem 1
nrows = 1000
ncols = 12
max_units = 5
X_data <- matrix(round(runif(nrows*ncols, 1, 15),digits = 3), nrow = nrows, ncol = ncols)
colnames(X_data) <- paste("X_data_", 1:ncol(X_data), sep = "")
betas <- sample(1:max_units, size = ncols, replace = TRUE)

#problem 2
Noise <- rnorm(nrows, mean = 0, sd = 4)
Y_data = X_data %*% betas + Noise

#problem 3
beta_hat <- solve(crossprod(X_data))%*%crossprod(X_data,Y_data)    #crossprod(X,Y)=t(X)%*%(Y)

#problem 4
estimation_error <- cbind(beta_hat, betas)
colnames(estimation_error) <- c("beta_hat","beta_data")
max(abs(beta_hat - betas))

#problem 5
#5.a
#creat matrix max_error_obtained
max_error_obtained <- matrix(NA, nrow = 20)
rownames(max_error_obtained)<- paste("n_rows_",1:length(max_error_obtained),sep='')

for (i in 1:20){
X_data <- matrix(round(runif((1000*i)*ncols, 1, 15),digits = 3), nrow = 1000*i, ncol = ncols)
betas <- sample(1:max_units, size = ncols, replace = TRUE)
Noise <- rnorm(1000*i, mean = 0, sd = 4)
Y_data = X_data %*% betas + Noise
beta_hat <- solve(crossprod(X_data))%*%crossprod(X_data,Y_data)
max_error_obtained[i,1] = max(abs(beta_hat - betas))
}
max_error_obtained

#5.b
barplot(max_error_obtained, beside = TRUE, xlab ='‘number of rows', main = "max error obtained")

#problem 6
set.seed(123546)
X = matrix(runif(100*10),100,10)
Y = X%*%cbind(1:10) + runif(100)
summary(lm(Y~0+X))
