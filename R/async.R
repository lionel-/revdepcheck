
# remotes::install_github("r-lib/coro")
# remotes::install_github("gaborcsardi/async")
# remotes::install_github("r-lib/rcmdcheck@async")

#' @importFrom coro async await
NULL


#' Compare local checks to CRAN checks
#'
#' @name cran_compare
#' @description
#'
#' Compare local checks against CRAN checks. This can be useful for
#' running checks on a local patched version of R with the CRAN
#' results for that package. For this reason, the default value of
#' `flavour_pattern` is `"devel"`
#'
#' @param dir The directory to perform local checks in.
#' @param pkgs A character vector of package names to check.
#' @param pkg_name A package name.
#' @param flavour_pattern A regexp to match against
#'   [rcmdcheck::cran_check_flavours()]. If multiple matches are
#'   found, the first element is used. This determines which CRAN
#'   check to compare against.
#' @export
revdep_check_against_cran <- function(dir,
                                      pkgs,
                                      num_workers = 2,
                                      flavour_pattern = "devel") {
  async::synchronise(
    async::async_map(
      pkgs,
      function(pkg) async_compare_to_cran(dir, pkg, flavour_pattern),
      .limit = num_workers
    )
  )
}

#' @rdname cran_compare
#' @name async_check_package
#' @usage async_check_package(dir, pkg)
#' @export
on_load(async_compare_to_cran %<~% async(function(dir, pkg_name, flavour_pattern = "devel") {
  results <- await(async::when_all(
    local = async_check_package(dir, pkg_name),
    cran = async_px_cran_results(pkg_name, flavour_pattern = flavour_pattern)
  ))

  rcmdcheck::compare_checks(
    old = results$cran,
    new = results$local
  )
}))

on_load(async_check_package %<~% async(function(dir, pkg_name) {
  fs::dir_create(dir)

  await(async::when_all(
    async_px_install_library(dir, pkg_name),
    async_px_download_package(dir, pkg_name)
  ))

  await(async_px_check(dir, pkg_name))
}))

async_px_install_library <- function(dir, pkg_name, quiet = FALSE, env = character()) {
  libdir <- fs::path(dir, pkg_name, "library")
  fs::dir_create(libdir)

  func <- function(libdir, packages, quiet, repos) {
    ip <- crancache::install_packages
    withr::with_libpaths(
      libdir,
      {
	ip(
	  packages,
	  dependencies = FALSE,
	  lib = libdir[1],
	  quiet = quiet,
	  repos = repos
	)
	stopifnot(all(packages %in% rownames(installed.packages(libdir[1]))))
      }
    )
  }

  args <- c(
    revdepcheck:::deps_opts(pkg_name),

    list(
      libdir = libdir,
      quiet = quiet
    )
  )

  ## CRANCACHE_REPOS makes sure that we only use cached CRAN packages,
  ## but not packages that were installed from elsewhere
  async_r(
    func = func,
    args = args,
    system_profile = FALSE,
    user_profile = FALSE,
    env = c(
      CRANCACHE_REPOS = "cran,bioc",
      CRANCACHE_QUIET = if (quiet) "yes" else "no",
      env
    )
  )
}

async_px_download_package <- function(dir, pkg_name) {
  pkg_dir <- fs::path(dir, pkg_name)
  fs::dir_create(pkg_dir)

  func <- function(pkg_name, dir, repos) {
    dest <- crancache::download_packages(pkg_name, dir, repos = repos)[,2]
    file.copy(dest, dir)
  }

  repos <- revdepcheck:::get_repos(bioc = TRUE)

  async_r(
    func = func,
    args = list(pkg_name = pkg_name, dir = pkg_dir, repos = repos),
    system_profile = FALSE,
    user_profile = FALSE,
    env = c(CRANCACHE_REPOS = "cran,bioc", CRANCACHE_QUIET = "yes")
  )
}

on_load(async_px_check %<~% async(function(dir, pkg_name, env = character()) {
  pkg_dir <- fs::path(dir, pkg_name)
  libdir <- fs::path(pkg_dir, "library")

  check_dir <- fs::path(pkg_dir, "check")
  fs::dir_create(check_dir)

  tarball <- revdepcheck:::latest_file(dir(pkg_dir, pattern = "\\.tar\\.gz$", full.names = TRUE))
  if (length(tarball) == 0) {
    abort(sprintf(
      "Internal error for package %s. No *.tar.gz file found.",
      pkg_name
    ))
  }

  stdout <- tempfile()
  stderr <- tempfile()

  px <- withr::with_envvar(
    c("R_ENVIRON_USER" = tempdir(), "R_LIBS" = "", "NO_COLOR" = "true", env),
    rcmdcheck::rcmdcheck_process$new(
      path = tarball,
      libpath = libdir,
      args = c("--no-manual", "--no-build-vignettes", "-o", check_dir),
      stdout = stdout,
      stderr = stderr
    )
  )
  await(async_px(px))

  px$parse_results()
}))

async_px_cran_results <- function(pkg, flavour_pattern = "devel") {
  flavour <- cran_flavour(pkg, flavour_pattern)
  fn <- function(pkg, flavours) rcmdcheck::cran_check_results(pkg, flavours)[[1]]

  async_r(
    fn,
    args = list(pkg = pkg, flavours = flavour),
    system_profile = FALSE,
    user_profile = FALSE
  )
}

cran_flavour <- function(pkg, flavour_pattern) {
  flavours <- rcmdcheck::cran_check_flavours(pkg)
  flavours <- flavours[grepl(flavour_pattern, flavours)]
  flavours[[1]]
}
