library(dplyr)

# Generate all combinations of the elements of
# x taken m at a time and join them
# together with underscores (_).
tie = function(x, m){
  combn(x, m = m, FUN = paste, collapse = "_")
}

# Given a dataframe and columns to group,
# this function returns the rows for each group.
get_group_rows = function(data, cols_to_group){
  library(dplyr)
  return(data %>%
           group_by(across(all_of(cols_to_group))) %>%
           group_rows())
}

# Return n random normal deviates of which the same
# deviates are each repeated [each] times and
# rounded to [digits].
# [each] must be numeric(1)
get_rounded_rnorm = function(n = 1, mean = 0, sd = 1, digits = 0, each = 1){

  stopifnot(length(each) == 1)
  return(
    rep(
      round(
        rnorm(
          n,
          mean = mean,
          sd = sd
        ),
        digits
      ),
      each = each
    )
  )
}

# Fabricate data for a factorial ANOVA of a CRD with no repeated measurements.

# flr: dataframe. Consists of experimental factors, their levels, and their number of replications. The first several columns contain experimental factors and their levels. The last column contains the number of replications for the combination of factor levels on that row.
### Example: x = list(color = factor(c("red", "blue")), temperature = factor(c("hot", "cold")), area = factor(c("close", "far", "mid")), lot = factor(c("U7", "Y2")))
### flr = cbind(expand.grid(x), n = rep(c(3,4,3,2), 6))

# skewness: character. Optional. One of "none", "low pos", "low neg", "high pos", "high neg".

# excess_kurtosis: character. Optional. One of "none", "low pos", "low neg", "high pos", "high neg".

# discrete: logical. Should the response be continuous or discrete?

# dependent_errors: numeric.
### 0 = no dependent errors;
### 1 = variance of the error increases with run order;
### 2 = variance of the error decreases with run order;
### 3 = mean of the error oscillates with run order;
### 4 = variance of the error increases with response;
### 5 = variance of the error differs among treatment groups

fabricate = function(flr,
                     skewness = "none",
                     excess_kurtosis = "none",
                     discrete = FALSE,
                     dependent_errors = 0
){
  library(dplyr)
  # Error handling.
  stopifnot(skewness %in% c("none", "low pos", "low neg", "high pos", "high neg"))
  stopifnot(excess_kurtosis %in% c("none", "low pos", "low neg", "high pos", "high neg"))
  stopifnot(dependent_errors %in% 0:5)

  response_updated = FALSE

  # Get the total number of factors.
  num_factors = ncol(flr) - 1

  # This will rapidly become too big of
  # a problem if we have too many factors.
  stopifnot(num_factors <= 10)

  # Get the vector of numbers of replicates.
  n = flr[, ncol(flr)]

  # Get the total number of responses.
  N = sum(n)

  # Get the number of treatment groups.
  k = sum(n != 0)

  # Make flr bigger and remove the column
  # containing the number of replicates.
  flr = flr %>%
    slice(rep.int(seq_len(nrow(flr)), times = flr[, ncol(flr)])) %>%
    dplyr::select(!last_col())

  # The overall_mean is randomly selected.
  overall_mean = sample(-100:100, size = 1, prob = c(rep(0.05, 100), rep(0.95, 101)))

  # Make another matrix that will hold the overall_mean
  # and main and interaction effects that we can bind
  # to flr later.
  # The number of subsets of a set of size n
  # is 2^n - 1 if the empty set is not wanted.
  # We want a column in our matrix for each subset of the
  # set of all our experimental factors (except for)
  # the empty set. We also want three additional columns:
  # run_order, response, and overall_mean.
  flr_2 = matrix(nrow = nrow(flr), ncol = 2^num_factors + 2)

  # Combinations of the column names of flr are generated.
  # The tie function uses each element of
  # seq_len(num_factors) for its m argument.
  colnames(flr_2) =
    c("run_order",
      "response",
      "overall_mean",
      unlist(sapply(X = seq_len(num_factors),
                    FUN = tie,
                    x = colnames(flr)[seq_len(num_factors)]))
    )

  # Then only change some of the row names.
  colnames(flr_2)[seq_len(num_factors)+3] = paste(colnames(flr_2)[seq_len(num_factors)+3], "main_effect", sep = "_")

  if(num_factors >= 2){
    colnames(flr_2)[(num_factors+4):ncol(flr_2)] = paste(colnames(flr_2)[(num_factors+4):ncol(flr_2)], "interaction_effect", sep = "_")
  }


  # Fill flr_2.
  # Fill in the overall_mean column separately.
  flr_2[, "overall_mean"] = rep(overall_mean, nrow(flr_2))

  # Generate a list containing which columns
  # we will want to group by.
  cols_to_group = Map(f = combn,
                      x = num_factors,
                      m = seq_len(num_factors))

  # Using the list of cols_to_group, get the
  # list of rows for each group.
  row_list = lapply(X = cols_to_group, FUN = function(X) {apply(X = X, MARGIN = 2, FUN = get_group_rows, data = flr)})

  # Iterate over row_list to
  # generate some random numbers for each grouping.
  # row_list is a nested list:
  # at the 1st level, we have lists,
  # at the 2nd level, we have lists,
  # and at the 3rd level, we have numeric vectors of rows.
  #
  # a is the number of factors that we are taking at a time
  # to choose the number of interactions of.
  # For example, we might have 3 factors and be taking
  # two at a time. Then, a = 2.



  for(a in seq_along(row_list)){
    for(b in seq_along(row_list[[a]])){
      for(c in seq_along(row_list[[a]][[b]])){
        # Assign random numbers to the first column
        # in flr_2 ending in "_effect" that
        # contains at least one NA.

        # For different situations, the sd will need
        # to be calculated differently.
        if(num_factors == 1){
          sd = 5
        }
        else if(num_factors == 2 & a == 1){
          sd = 5
        }
        else if(num_factors == 2 & a == 2){
          sd = 2
        }
        else{
          sd = 5*50^((1-a)/(num_factors - 1))
        }

        flr_2[row_list[[a]][[b]][[c]], 3 + which.max(colSums(is.na(flr_2[, -(1:3), drop = FALSE])) >= 1)] = get_rounded_rnorm(each = length(row_list[[a]][[b]][[c]]), sd = sd)
      }
    }
  }

  # Fill up the responses with some base values to which randomness will be added later.
  # Use the last row from within each grouping
  # for speedier computation.
  flr_2[, "response"] =
    rep(rowSums(flr_2[cumsum(n), -(1:2)]), times = n)

  # Randomly assign the treatments to the
  # experimental units to generate run_order.
  flr_2[, "run_order"] = sample(seq_len(N), size = N)


  # Setup is complete. Here comes the meat of the function. -----------------

  # Fill up responses depending on the conditions.

  # dependent_errors == 0 ---------------------------------------------------

  if(dependent_errors == 0){

    # Add random error vector to responses
    if(skewness == "none" & excess_kurtosis == "none" & discrete == FALSE){
      flr_2[, "response"] = flr_2[, "response"] + rnorm(n = N, sd = 3)
      response_updated = TRUE
    }
    else if(skewness == "none" & excess_kurtosis == "none" & discrete == TRUE){
      flr_2[, "response"] = flr_2[, "response"] + rbinom(n = N, size = 16, prob = 1/2)-8 # This random error vector has a mean of 0 and a sd of 2.
      response_updated = TRUE
    }
    else if(skewness == "low pos" & excess_kurtosis == "low pos" & discrete == FALSE){
      flr_2[, "response"] = flr_2[, "response"] + rchisq(n = N, df = 12) - 12 # The chi-squared distribution has an excess kurtosis of 12/df and a mean = df.
      response_updated = TRUE
    }
    else if(skewness == "low pos" & excess_kurtosis == "low pos" & discrete == TRUE){
      flr_2[, "response"] = flr_2[, "response"] + rbinom(n = N, size = 500, prob = 0.01)-5 # The binomial distribution can be made to be skewed. With these parameters our error has a skewness of 0.440 and an excess kurtosis of 0.190.
      response_updated = TRUE
    }
    else if(skewness == "low neg" & excess_kurtosis == "low pos" & discrete == FALSE){
      flr_2[, "response"] = flr_2[, "response"] - (rchisq(n = N, df = 12) - 12) # The chi-squared distribution has an excess kurtosis of 12/df and a mean = df.
      response_updated = TRUE
    }
    else if(skewness == "low neg" & excess_kurtosis == "low pos" & discrete == TRUE){
      flr_2[, "response"] = flr_2[, "response"] - (rbinom(n = N, size = 500, prob = 0.01)-5)
      response_updated = TRUE
    }
    else if(skewness == "high pos" & excess_kurtosis == "high pos" & discrete == FALSE){
      flr_2[, "response"] = flr_2[, "response"] + rchisq(n = N, df = 2) - 2 # The chi-squared distribution has an excess kurtosis of 12/df and a mean = df and a skewness of sqrt(8/df).
      response_updated = TRUE
    }
    else if(skewness == "high pos" & excess_kurtosis == "high pos" & discrete == TRUE){
      flr_2[, "response"] = flr_2[, "response"] + rbinom(n = N, size = 1000, prob = 0.001)-1 # The binomial distribution has a mean = np.
      # With n = 1000 and p = 0.001, the skewness is 0.998 and the excess kurtosis is 0.995.
      response_updated = TRUE
    }
    else if(skewness == "high neg" & excess_kurtosis == "high pos" & discrete == FALSE){
      flr_2[, "response"] = flr_2[, "response"] - (rchisq(n = N, df = 2) - 2) # The chi-squared distribution has an excess kurtosis of 12/df and a mean = df.
      response_updated = TRUE
    }
    else if(skewness == "high neg" & excess_kurtosis == "high pos" & discrete == TRUE){
      flr_2[, "response"] = flr_2[, "response"] - (rbinom(n = N, size = 1000, prob = 0.001)-1) # With n = 1000 and p = 0.001, the skewness is 0.998 and the excess kurtosis is 0.995.
      response_updated = TRUE
    }
    else if(skewness == "none" & excess_kurtosis == "low pos" & discrete == FALSE){
      # With these arguments, Student's t-Distribution has an excess kurtosis of 0.857.
      flr_2[, "response"] = flr_2[, "response"] + rt(n = N, df = 11)
      response_updated = TRUE
    }
    else if(skewness == "none" & excess_kurtosis == "high pos" & discrete == FALSE){
      # excess kurtosis of 6
      flr_2[, "response"] = flr_2[, "response"] + rt(n = N, df = 5)
      response_updated = TRUE
    }
    else if(skewness == "none" & excess_kurtosis == "low neg" & discrete == TRUE){
      # excess kurtosis of -0.5
      flr_2[, "response"] = flr_2[, "response"] + (rbinom(n = N, size = 4, prob = 0.5) - 2)
      response_updated = TRUE
    }
  }

  # 1 = variance of the error increases with run order

  # dependent_errors == 1 ---------------------------------------------------

  else if(dependent_errors == 1){

    if(skewness == "none" & excess_kurtosis == "none" & discrete == FALSE){
      # Add random error vector to responses. The sd is calculated so that it grows exponentially with the run order from a min of 1 to a max of 5.
      flr_2[, "response"] = flr_2[, "response"] + rnorm(n = N, sd = 5^((flr_2[, "run_order"] - 1)/(N - 1)))
      response_updated = TRUE
    }
    else if(skewness == "none" & excess_kurtosis == "none" & discrete == TRUE){
      # Add random error vector to responses. The sd is calculated so that it grows exponentially with the run order from a min of 1 to a max of 5.
      flr_2[, "response"] = flr_2[, "response"] + get_rounded_rnorm(n = N, sd = 5^((flr_2[, "run_order"] - 1)/(N - 1)))
      response_updated = TRUE
    }
  }

  # 2 = variance of the error decreases with run order

  # dependent_errors == 2 ---------------------------------------------------

  else if(dependent_errors == 2){

    if(skewness == "none" & excess_kurtosis == "none" & discrete == FALSE){
      # Add random error vector to responses. The sd is calculated so that it decreases exponentially with the run order from a max of 5 to a min of 1.
      flr_2[, "response"] = flr_2[, "response"] + rnorm(n = N, sd = 5^((N - flr_2[, "run_order"])/(N - 1)))
      response_updated = TRUE
    }
    else if(skewness == "none" & excess_kurtosis == "none" & discrete == TRUE){
      # Add random error vector to responses. The sd is calculated so that it decreases exponentially with the run order from a max of 5 to a min of 1.
      flr_2[, "response"] = flr_2[, "response"] + get_rounded_rnorm(n = N, sd = 5^((N - flr_2[, "run_order"])/(N - 1)))
      response_updated = TRUE
    }
  }

  ### 3 = mean of the error oscillates with run order

  # dependent_errors == 3 ---------------------------------------------------

  else if(dependent_errors == 3){

    if(skewness == "none" & excess_kurtosis == "none" & discrete == FALSE){
      flr_2[, "response"] = flr_2[, "response"] +
        rnorm(n = N,
              mean = 2*sin(2*pi/N*flr_2[, "run_order"]))
      response_updated = TRUE
    }
    else if(skewness == "none" & excess_kurtosis == "none" & discrete == TRUE){
      flr_2[, "response"] = flr_2[, "response"] +
        get_rounded_rnorm(n = N,
                          mean = 2*sin(2*pi/N*flr_2[, "run_order"]))
      response_updated = TRUE
    }
  }

  # 4 = variance of the error increases with response

  # dependent_errors == 4 ---------------------------------------------------
  else if(dependent_errors == 4){

    if(skewness == "none" & excess_kurtosis == "none" & discrete == FALSE){
      # Add random error vector to responses. The sd is calculated so that it grows exponentially with the response from a min of 1 to a max of 5.
      # The responses should be more variable if
      # they are part of a group with a higher
      # base response.

      # Get ranks of responses.
      x = rank(flr_2[, "response"])

      # Get standard deviations for each response.
      sd = 5^((x-1)/(N-1))

      flr_2[, "response"] = flr_2[, "response"] +
        rnorm(n = N, sd = sd)
      response_updated = TRUE
    }
  }

  # 5 = variance of the error differs among treatment groups

  # dependent_errors == 5 ---------------------------------------------------
  else if(dependent_errors == 5){

    if(skewness == "none" & excess_kurtosis == "none" & discrete == FALSE){
      # Randomly choose which factor will have
      # different variances in the responses
      # for its levels.
      random_factor = sample(1:num_factors, size = 1)

      # Iterate over row_list to
      # generate some random numbers.
      for(c in seq_along(row_list[[1]][[random_factor]])){
        # Assign random numbers to the response.
        flr_2[row_list[[a]][[b]][[c]], "response"] =
          flr_2[row_list[[a]][[b]][[c]], "response"] +
          rnorm(n = length(row_list[[a]][[b]][[c]]),
                sd = sample(1:4, size = 1))
      }

      # Make sure the responses in any
      # treatment group have some kind
      # of randomness.
      flr_2[, "response"] = flr_2[, "response"] +
        rnorm(n = N, sd = 2)

      response_updated = TRUE
    }

  }

  # Check to see if responses were generated.
  if(response_updated == FALSE){
    print("That particular combination of arguments is not currently supported.")
    stop()
  }

  # Round responses to make them more tractable.
  flr_2[, "response"] = round(flr_2[, "response"], 2)

  # Lastly, bind flr_2 to flr.
  flr = bind_cols(flr, as.data.frame(flr_2), .name_repair = "minimal")

  return(flr)
}




