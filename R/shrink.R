
# shrink function

shrink = function(n, y_bar, mu, c) {
  tau = 1
  tau_0 = ((n + c) / n) * tau

  (n * tau / (n * tau + tau_0)) * y_bar + (tau_0 / (n * tau + tau_0)) * mu
}
