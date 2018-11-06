context("utils")

test_that("unduplicate() works with empty tibbles", {
  expect_identical(unduplicate(data.frame()), data.frame())
})
