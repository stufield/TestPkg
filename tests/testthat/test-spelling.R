
test_that("TestPkg has no spelling errors", {
  skip_on_check()
  skip_on_covr()
  spelling_errors <- devtools::spell_check()$word
  expect_length(spelling_errors, 0)
})
