
context("dependencies")

test_that("parse_deps", {
  deps <- c("foobar", "foobar (>= 1.0.0)", "foo, bar", "foo,bar",
            "foo(>= 1.0.0), bar", "foo,\n    bar", "")
  expect_equal(
    parse_deps(deps),
    list("foobar", "foobar", c("foo", "bar"), c("foo", "bar"),
         c("foo", "bar"), c("foo", "bar"), character())
  )
})

test_that("parse_deps extreme cases", {
  deps <- c(NA, "", "  ")
  expect_equal(
    parse_deps(deps),
    list(character(), character(), character())
  )

  expect_equal(parse_deps(character()), list())
})

test_that("pkgs_validate() checks types", {
  expect_error(pkgs_validate(10), "must be")
  expect_error(pkgs_validate(list()), "must be")
  expect_error(pkgs_validate(data.frame(x = "foo")), "must contain")
})

test_that("pkgs_validate() accepts character vectors", {
  out <- pkgs_validate(c("foo", "bar"))
  exp <- tibble::tibble(.package = c("foo", "bar"))
  expect_identical(out, exp)
})

test_that("pkgs_validate() coerces to tibble", {
  expect_is(pkgs_validate(data.frame(.package = "foo")), "tbl_df")
})

test_that("pkgs_validate() removes duplicated rows", {
  df <- data.frame(
    repo = c("a", "b", "b"),
    .package = c("foo", "bar", "foo"),
    stringsAsFactors = FALSE
  )

  out <- pkgs_validate(df)
  exp <- tibble::tibble(repo = c("a", "b"), .package = c("foo", "bar"))
  expect_identical(out, exp)
})

test_that("pkgs_groups() returns groups", {
  expect_length(pkgs_groups(data.frame(.package = "foo")), 0)

  data <- data.frame(repo = c("CRAN", "bioc"), .package = c("foo", "bar"))
  expect_identical(pkgs_groups(data), data.frame(repo = c("CRAN", "bioc")))
})

test_that("pkgs_revdeps() returns tibble", {
  skip_if_offline()
  scoped_options(revdepcheck__limit_revdeps = TRUE)

  pkgs <- pkgs_revdeps("tidyverse")
  expect_identical(dim(pkgs), c(2L, 2L))
  expect_named(pkgs, c("repo", ".package"))
})

