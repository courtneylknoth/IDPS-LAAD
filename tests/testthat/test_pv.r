test_that('pv provides proper messages adn warnings',{
  expect_error(pv(FV="1000", 0.09, n=5))
  expect_error(pv(FV=list(1,2,3), 0.09, n=5))
  expect_message(pv(FV=1000, 0.36, n=5))
})

test_that("pv has correct dimensions and output type", {
  expect_is(pv(FV=1000, 0.09, n=5), 'numeric')
  expect_true(is.vector(pv(FV=c(1000,2000), 0.09, n = 5)))
  expect_length(pv(FV=c(1000,2000), 0.09, n=5), 2)
})

test_that("pv computes correctly",{
  expect_equal(pv(FV = 1000, 0.09, n=5), 650)
})