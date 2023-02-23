
calc_rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

