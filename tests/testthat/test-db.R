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

test_that("db_insert() removes package from `todo`", {
  db_setup(":memory:")
  db_todo_add(":memory:", c("a", "b"))

  db_insert(":memory:",
    package = "b",
    status = "OK",
    duration = 0,
    starttime = 0,
    result = "",
    summary = ""
  )

  expect_identical(db_todo(":memory:"), "a")
})

test_that("default group table has `package` column", {
  db_setup(":memory:")
  db_todo_add(":memory:", c("a", "b"))
  expect_identical(sort(db_todo(":memory:")), c("a", "b"))
  expect_identical(db_groups(":memory:"), tibble::tibble(package = c("a", "b")))
})

test_that("group metadata is set", {
  db_setup(":memory:")

  groups <- tibble::tibble(group = c("g1", "g2", "g1"), package = c("a", "b", "c"))
  db_todo_add(":memory:", groups)

  expect_identical(sort(db_todo(":memory:")), c("a", "b", "c"))
  expect_identical(db_groups(":memory:"), groups)
})

test_that("existing groups are checked when adding ungrouped packages", {
  db_setup(":memory:")

  groups <- tibble::tibble(group = c("g1", "g2", "g1"), package = c("a", "b", "c"))
  db_todo_add(":memory:", groups)

  db_insert(":memory:",
    package = "b",
    status = "OK",
    duration = 0,
    starttime = 0,
    result = "",
    summary = ""
  )
  expect_identical(sort(db_todo(":memory:")), c("a", "c"))

  db_todo_add(":memory:", "b")
  expect_identical(sort(db_todo(":memory:")), c("a", "b", "c"))

  out <- db_raw_results_by_group(":memory:")
  expect_named(out, "g2")
})
