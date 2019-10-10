context("Testing the `calcCCC` Function -> Concordance Correlation Coefficient")

set.seed(101)
x <- rnorm(100)
y <- rnorm(100)

test_that("calcCCC unit test", {
  ccc <- calcCCC(x, y)
  expect_error(calcCCC(x, c(y, 1)))
  expect_named(ccc, c("rho.c", "ci95", "Z", "p.value"))
  expect_length(ccc$ci95, 2)
  expect_named(ccc$ci95, c("lower", "upper"))
  expect_equal(unname(ccc$ci95), c(-0.08927664, 0.29625201))
  expect_equal(ccc$Z, 0.1079455)
  expect_equal(ccc$p.value, 0.9140389)
})
