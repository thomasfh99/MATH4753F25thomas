test_that("myncurve returns the input mu unchanged", {
  res = myncurve(mu = 2.5, sigma = 1.2, a = 0)
  expect_true(is.list(res))
  expect_named(res, c("mu", "sigma", "a", "area"))
  expect_equal(res$mu, 2.5)
})

test_that("myncurve returns the input sigma unchanged", {
  res = myncurve(mu = 0, sigma = 3.7, a = 1)
  expect_equal(res$sigma, 3.7)
})

test_that("myncurve computes correct lower-tail probability (area)", {
  mu = -1
  sigma = 0.8
  a = 0.5
  res = myncurve(mu = mu, sigma = sigma, a = a)
  expected_area = pnorm(a, mean = mu, sd = sigma)
  expect_equal(res$area, expected_area, tolerance = 1e-8)
})
