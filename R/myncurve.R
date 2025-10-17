#' myncurve
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a Upper limit for the shaded area and the cumulative probability
#' @importFrom grDevices rgb
#' @importFrom graphics abline legend polygon
#' @importFrom stats dnorm pnorm
#'
#' @returns a normal distribution curve
#' @export
#'
#' @examples
#' # Standard normal: P(X <= -0.5)
#' \dontrun{myncurve(mu = 0, sigma = 1, a = -0.5)}
#' # Centered at 10 with spread 2: P(X <= 11)
#' \dontrun{myncurve(mu = 10, sigma = 2, a = 11)}
myncurve = function(mu, sigma, a){
  # Validate inputs
  if(!is.numeric(mu) || !is.numeric(sigma) || !is.numeric(a)) stop("mu, sigma, and a must be numeric")
  if(sigma <= 0) stop("sigma must be positive")

  # plotting range (extend a bit to include tails)
  xlim = c(mu - 4*sigma, mu + 4*sigma)

  # x grid for curve
  x = seq(xlim[1], xlim[2], length.out = 1000)
  y = dnorm(x, mean = mu, sd = sigma)

  # draw the density curve
  oldpar = par(no.readonly = TRUE)
  on.exit(par(oldpar))
  plot(x, y, type = "l", lwd = 2, col = "black",
       xlab = "x", ylab = "density",
       main = paste0("Normal density: mu=", mu, ", sigma=", sigma))

  # determine shading region (from -Inf to a)
  # limit shading to plotting range
  x_shade = x[x <= a]
  if(length(x_shade) > 0){
    y_shade = dnorm(x_shade, mean = mu, sd = sigma)
    polygon(c(x_shade, rev(x_shade[1])), c(y_shade, 0),
            col = rgb(0.2, 0.6, 0.9, 0.4), border = NA)
  }

  # vertical line at a
  abline(v = a, col = "red", lty = 2)

  # compute cumulative probability P(X <= a)
  area = pnorm(a, mean = mu, sd = sigma)

  # annotate the plot with the numeric area
  legend("topright", legend = sprintf("P(X <= %.3g) = %.5f", a, area),
         bty = "n")

  # return results as a list
  return(list(mu = mu, sigma = sigma, a = a, area = area))
}
