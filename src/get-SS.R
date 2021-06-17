get_SS_total = function(y) {
  stopifnot(length(y)>=2)
  return((length(y)-1)*var(y))
}

get_SS_within = function(grouping_vector, y) {
  stopifnot(length(y)>=2, identical(length(grouping_vector), length(y)))
  return(sum(tapply(X = y, INDEX = grouping_vector, FUN = get_SS_total)))
}

get_SS_between = function(grouping_vector, y) {
  stopifnot(length(y)>=2, identical(length(grouping_vector), length(y)))
  grand_mean = mean(y)
  group_means = tapply(X = y, INDEX = grouping_vector, FUN = mean)
  n = tapply(X = y, INDEX = grouping_vector, FUN = length)
  return(sum(n*(group_means - grand_mean)^2))
}
