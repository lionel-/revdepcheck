context("utils")

test_that("unduplicate() works with empty tibbles", {
  expect_identical(unduplicate(data.frame()), data.frame())
})

test_that("nest_join() handles fully unmatched keys", {
  dfs <- list(
    foo = tibble(x = 1:3, key = c("a", "b", "c")),
    bar = tibble(y = 4:6, key = c("a", "c", "d")),
    baz = tibble(z = 7L, key = "d")
  )

  exp <- tibble(
    key = chr(),
    foo = list(),
    bar = list(),
    baz = list()
  )
  expect_identical(nest_join("key", !!!dfs, .unmatched = "drop"), exp)

  expect_error(bare_join("key", !!!dfs, .unmatched = "error"), "can't be unmatched")
})

test_that("nest_join() matches single key", {
  dfs <- list(
    foo = tibble(x = 1:3, key = c("a", "b", "c")),
    bar = tibble(y = 4:6, key = c("a", "b", "d")),
    baz = tibble(z = 7L, key = "b")
  )

  exp <- tibble(
    key = "b",
    foo = list(tibble(x = 2L)),
    bar = list(tibble(y = 5L)),
    baz = list(tibble(z = 7L))
  )
  expect_identical(nest_join("key", !!!dfs, .unmatched = "drop"), exp)

  expect_error(nest_join("key", !!!dfs, .unmatched = "error"), "can't be unmatched")
})

test_that("nest_join() matches two keys", {
  dfs <- list(
    foo = tibble(x = 1:3, key = c("a", "b", "c")),
    bar = tibble(y = 4:6, key = c("a", "b", "d")),
    baz = tibble(z = 7L, key = c("b", "a"))
  )

  exp <- tibble(
    key = c("a", "b"),
    foo = list(tibble(x = 1L), tibble(x = 2L)),
    bar = list(tibble(y = 4L), tibble(y = 5L)),
    baz = list(tibble(z = 7L), tibble(z = 7L))
  )
  expect_identical(nest_join("key", !!!dfs, .unmatched = "drop"), exp)

  expect_error(nest_join("key", !!!dfs, .unmatched = "error"), "can't be unmatched")
})

test_that("nest_join() matches on all keys", {
  dfs <- list(
    foo = tibble(x = 1:3, key = c("a", "b", "c")),
    bar = tibble(y = 4:6, key = c("c", "b", "a")),
    baz = tibble(z = 7L, key = c("b", "a", "c"))
  )

  exp <- tibble(
    key = c("a", "b", "c"),
    foo = list(tibble(x = 1L), tibble(x = 2L), tibble(x = 3L)),
    bar = list(tibble(y = 6L), tibble(y = 5L), tibble(y = 4L)),
    baz = list(tibble(z = 7L), tibble(z = 7L), tibble(z = 7L))
  )
  expect_identical(nest_join("key", !!!dfs, .unmatched = "drop"), exp)
  expect_identical(nest_join("key", !!!dfs, .unmatched = "error"), exp)
})

test_that("nest_join() fails with duplicated keys", {
  dfs <- list(
    foo = tibble(x = 1:3, key = c("a", "a", "b")),
    bar = tibble(y = 4:6, key = c("a", "b", "c"))
  )
  expect_error(nest_join("key", !!!dfs), "can't be duplicated")
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
