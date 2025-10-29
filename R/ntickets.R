#' ntickets
#'
#' @param N The number of seats available on the flight
#' @param gamma The probability that overbooking will occur
#' @param p The likelihood that a passenger will show up for the flight
#'
#' @importFrom stats qbinom pbinom qnorm
#' @importFrom graphics abline barplot points
#'
#' @returns Returns the plots for both discrete and continuous methods, and a list of values for nd, nc, N, p, and gamma
#' @export
#'
#' @examples
#' \dontrun{ntickets(N = 400, gamma = 0.02, p = 0.95)}
ntickets = function(N, gamma, p){

  # Sets up an n range to search for the optimal number of tickets to be sold for each of the two methods
  n_range = seq(N, N + 50, by = 1)

  ## Discrete Objective ##

  # Discrete objective formula: 1 - gamma - P(X <= N), where X~Binomial(n,p)
  obj_discrete = 1 - gamma - pbinom(N, size = n_range, prob = p)

  # Finds the index of minimum objective (closest to zero)
  idx_discrete = which.min(abs(obj_discrete))

  # Finds the minimum n-value for the discrete method using the range of N's
  nd = n_range[idx_discrete]

  # Plots the line of the discrete objective
  plot(n_range, obj_discrete,
       type = "l", lty = "dotdash", col = "black",
       main = paste("Objective Vs n to find optimal tickets sold\n(", n_range[idx_discrete],") gamma=", gamma, ", N=", N, " discrete", sep = ""),
       xlab = "n", ylab = "Objective")

  # Plots points along the line of the discrete objective
  points(n_range, obj_discrete, pch = 21, col = "black", bg = "blue", cex = 0.7)

  # Plot the point to show what is the optimal number of n tickets to be sold
  points(n_range[idx_discrete], obj_discrete[idx_discrete], pch = 21, bg = "yellow", cex = 1)

  # Plots the vertical and horizontal lines that intersect with the optimal point
  abline(h = 0, col = "red", lty = 1, lwd = 2.5)
  abline(v = n_range[idx_discrete], col = "red", lty = 1, lwd = 2.5)


  ## Normal (Continuous) Approximation Objective ##

  # Approximates P(X <= N) by taking the z-value
  z_value = (N + 0.5 - n_range * p) / sqrt(n_range * p * (1 - p))
  p_approx = pnorm(z_value)
  obj_continuous = 1 - gamma - p_approx

  # Finds the index of minimum objective (closest to zero)
  idx_cont = which.min(abs(obj_continuous))

  # Finds the minimum n-value for the continuous method using the range of N's
  nc = n_range[idx_cont]

  # Plots the line of the continuous objective
  plot(n_range, obj_continuous,
       type = "l", col = "black", lwd = 1,
       main = paste("Objective Vs n to find optimal tickets sold\n(", n_range[idx_cont],") gamma=", gamma, ", N=", N, " continuous", sep = ""),
       xlab = "n", ylab = "Objective")

  # Plot the point to show what is the optimal number of n tickets to be sold
  points(n_range[idx_cont], obj_continuous[idx_cont], pch = 21, bg = "yellow", cex = 1)

  # Plots the vertical and horizontal lines that intersect with the optimal point
  abline(h = 0, col = "blue", lty = 1)
  abline(v = n_range[idx_cont], col = "blue", lty = 1)

  # Returns a list that prints the values of nd, nc, N, p, and gamma
  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}
