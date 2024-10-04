test_that("GLM.new works", {
  expect_equal(GLM.new(distr = "normal"), NormalGLM$new())
  expect_equal(GLM.new(distr = "exp"), ExpGLM$new())
  expect_equal(GLM.new(distr = "weibull"), WeibullGLM$new())
  expect_equal(GLM.new(distr = "gamma"), GammaGLM$new())
})

test_that("GLM.new throws an error for unknown distr", {
  expect_error(GLM.new(distr = "unknown"))
})
