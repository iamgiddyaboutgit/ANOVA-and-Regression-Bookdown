sim1 = 10
X_data = rep(seq_len(5), each = sim1)

get_Y_data = function(x){
  return((x - 3)^2 + 2 + rnorm(n = length(x)))
}

Y_data = get_Y_data(X_data)
data = matrix(data = NA_real_, nrow = 5*sim1, ncol = 2, dimnames = list(NULL, c("x", "y")))
data[, "x"] = X_data
data[, "y"] = Y_data

print("If we group Y by X, then we might understand Y's variability better.")
plot(y ~ x, data = data)

# total variance of Y (sample estimation problems aside)
get_pop_var = function(X){
  N = length(X)
  return(1/N*sum(X^2)-(sum(X)/N)^2)
}

V = get_pop_var(Y_data)
print(paste0("The population variance of Y is ", V))

EV = mean(tapply(X = data[, "y"], INDEX = X_data, FUN = get_pop_var))
VE = get_pop_var(tapply(X = data[, "y"], INDEX = X_data, FUN = mean))
print(paste0("The population variance of Y within X is ", EV))
print(paste0("The population variance of Y between X is ", VE))
print(paste0("The variance of Y = the variance of Y within X + the variance of Y between X (", round(V, 3), " = ", round(EV, 3), " + ", round(VE, 3), ")"))

