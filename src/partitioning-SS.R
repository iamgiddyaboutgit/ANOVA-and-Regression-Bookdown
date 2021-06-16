source(file.path("src", "fabricate.R"))

x = list(trt = factor(seq_len(5)))
fab_prep = cbind(expand.grid(x), n = c(10,14,13,9,8))
fabricated_data = fabricate(flr = fab_prep)
plot(response ~ trt, data = fabricated_data)





print("If we group Y by X, then we might understand Y's variability better.")


# total variance of Y (sample estimation problems aside)
get_pop_var = function(X){
  N = length(X)
  return(1/N*sum(X^2)-(sum(X)/N)^2)
}

# V = get_pop_var()
# print(paste0("The population variance of Y is ", V))
#
# EV = mean(tapply(X = data[, "y"], INDEX = X_data, FUN = get_pop_var))
# VE = get_pop_var(tapply(X = data[, "y"], INDEX = X_data, FUN = mean))
# print(paste0("The population variance of Y within X is ", EV))
# print(paste0("The population variance of Y between X is ", VE))
# print(paste0("The variance of Y = the variance of Y within X + the variance of Y between X (", round(V, 3), " = ", round(EV, 3), " + ", round(VE, 3), ")"))
#
# # investigation of mean squares
# data_df = data.frame(x = factor(X_data), y = Y_data)
# summary(aov(y ~ x, data = data_df))
# var(Y_data)
# mean(tapply(X = data[, "y"], INDEX = X_data, FUN = var)) +
#   var(tapply(X = data[, "y"], INDEX = X_data, FUN = mean))
