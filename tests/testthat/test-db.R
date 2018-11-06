context("db")


# metadata ----------------------------------------------------------------

test_that("can get and set metadata", {
  db_setup(":memory:")

  db_metadata_set(":memory:", "x", "abc")
  expect_equal(db_metadata_get(":memory:", "x"), "abc")
})

test_that("setting metadata replaces previous value", {
  db_metadata_set(":memory:", "y", "abc")
  db_metadata_set(":memory:", "y", "xyz")

  expect_equal(db_metadata_get(":memory:", "y"), "xyz")
})

test_that("package name is set", {
  pkg <- pkg_check("fixtures/rvdcTestPkg/")
  db_setup(pkg)
  expect_identical(db_metadata_get(pkg, "package"), "rvdcTestPkg")

  db_setup(":memory:")
  expect_identical(db_metadata_get(":memory:", "package"), ":memory:")
})
