context("utils")

test_that("unduplicate() works with empty tibbles", {
  expect_identical(unduplicate(data.frame()), data.frame())
})

test_that("nest_join() joins", {
  out <- nest_join(
    tibble::tibble(x = 1:3, key = c("a", "a", "b")),
    tibble::tibble(y = 3:1, key = c("a", "b", "c")),
    "key",
    name = "foo"
  )

  exp <- tibble::tibble(
    x = 1:3,
    key = c("a", "a", "b"),
    foo = list(
      tibble::tibble(y = 3L),
      tibble::tibble(y = 3L),
      tibble::tibble(y = 2L)
    )
  )

  expect_identical(out, exp)
})

test_that("nest_join() handles missing matches", {
  out <- nest_join(
    tibble::tibble(x = 1:3, key = c("a", "a", "b")),
    tibble::tibble(y = 3:1, key = c("a", "c", "c")),
    by = "key",
    name = "foo"
  )

  exp <- tibble::tibble(
    x = 1:3,
    key = c("a", "a", "b"),
    foo = list(
      tibble::tibble(y = 3L),
      tibble::tibble(y = 3L),
      tibble::tibble(y = int())
    )
  )

  expect_identical(out, exp)
})
