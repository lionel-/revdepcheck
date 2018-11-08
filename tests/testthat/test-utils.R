context("utils")

test_that("unduplicate() works with empty tibbles", {
  expect_identical(unduplicate(data.frame()), data.frame())
})

test_that("nest_join() joins", {
  out <- nest_join(
    "key",
    name = "foo"
    foo = tibble(x = 1:3, key = c("a", "a", "b")),
    bar = tibble(y = 3:1, key = c("a", "b", "c"))
  )

  exp <- tibble(
    x = 1:3,
    key = c("a", "a", "b"),
    foo = list(
      tibble(y = 3L),
      tibble(y = 3L),
      tibble(y = 2L)
    )
  )

  expect_identical(out, exp)
})

test_that("nest_join() handles missing matches", {
  out <- nest_join(
    tibble(x = 1:3, key = c("a", "a", "b")),
    tibble(y = 3:1, key = c("a", "c", "c")),
    by = "key",
    name = "foo"
  )

  exp <- tibble(
    x = 1:3,
    key = c("a", "a", "b"),
    foo = list(
      tibble(y = 3L),
      tibble(y = 3L),
      tibble(y = int())
    )
  )

  expect_identical(out, exp)
})

test_that("bare_join() handles fully unmatched keys", {
  dfs <- list(
    foo = tibble(x = 1:3, key = c("a", "b", "c")),
    bar = tibble(y = 4:6, key = c("a", "c", "d")),
    baz = tibble(z = 7L, key = "d")
  )

  exp <- tibble(
    key = chr(),
    foo = tibble(x = int()),
    bar = tibble(y = int()),
    baz = tibble(z = int())
  )
  expect_identical(bare_join("key", !!!dfs, .unmatched = "drop"), exp)

  expect_error(bare_join("key", !!!dfs, .unmatched = "error"), "can't be unmatched")
})

test_that("bare_join() matches single key", {
  dfs <- list(
    foo = tibble(x = 1:3, key = c("a", "b", "c")),
    bar = tibble(y = 4:6, key = c("a", "b", "d")),
    baz = tibble(z = 7L, key = "b")
  )

  exp <- tibble(
    key = "b",
    foo = tibble(x = 2L),
    bar = tibble(y = 5L),
    baz = tibble(z = 7L)
  )
  expect_identical(bare_join("key", !!!dfs, .unmatched = "drop"), exp)

  expect_error(bare_join("key", !!!dfs, .unmatched = "error"), "can't be unmatched")
})

test_that("bare_join() matches two keys", {
  dfs <- list(
    foo = tibble(x = 1:3, key = c("a", "b", "c")),
    bar = tibble(y = 4:6, key = c("a", "b", "d")),
    baz = tibble(z = 7L, key = c("b", "a"))
  )

  exp <- tibble(
    key = c("a", "b"),
    foo = tibble(x = c(1L, 2L)),
    bar = tibble(y = c(4L, 5L)),
    baz = tibble(z = c(7L, 7L))
  )
  expect_identical(bare_join("key", !!!dfs, .unmatched = "drop"), exp)

  expect_error(bare_join("key", !!!dfs, .unmatched = "error"), "can't be unmatched")
})

test_that("bare_join() matches on all keys", {
  dfs <- list(
    foo = tibble(x = 1:3, key = c("a", "b", "c")),
    bar = tibble(y = 4:6, key = c("c", "b", "a")),
    baz = tibble(z = 7L, key = c("b", "a", "c"))
  )

  exp <- tibble(
    key = c("a", "b", "c"),
    foo = tibble(x = 1:3),
    bar = tibble(y = 6:4),
    baz = tibble(z = c(7L, 7L, 7L))
  )
  expect_identical(bare_join("key", !!!dfs, .unmatched = "drop"), exp)
  expect_identical(bare_join("key", !!!dfs, .unmatched = "error"), exp)
})

test_that("bare_join() fails when keys are duplicated", {
  expect_error(
    bare_join("key",
      foo = tibble(x = 1:3, key = c("a", "a", "b")),
      bar = tibble(y = 3:1, key = c("a", "b", "c"))
    ),
    "can't be duplicated"
  )
})
