
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
  if (is.numeric(pkgs)) {
    pkgs <- sample(rownames(crancache::available_packages()), size = pkgs)
  }
  assert_that(
    is_character(pkgs),
    is_character(dir)
  )

  cache_dir <- fs::path(dir, "cache")
  fs::dir_create(cache_dir)

  # Don't try again to prepopulate cache with packages that failed to install
  failed_path <- fs::path(dir, "install-failed.rds")
  if (fs::file_exists(failed_path)) {
    failed <- readRDS(failed_path)
  } else {
    failed <- NULL
  }

  cache_results <- tryCatch(
    interrupt = function(...) NULL,
    populate_crancache(cache_dir, pkgs, num_workers = num_workers, exclude = failed)
  )

  # Update failures
  has_failed <- map_lgl(cache_results, inherits, "error")
  if (any(has_failed)) {
    failed <- c(failed, names(cache_results)[has_failed])
    saveRDS(failed, failed_path)
  }


  status("CHECK", paste0(length(pkgs), " packages"))

  pb <- progress::progress_bar$new(
    total = length(pkgs),
    format = "[:current/:total] :elapsedfull | ETA: :eta | :pkg"
  )
  current_pkgs <- character()

  tick <- function(n = 1L) {
    current <- paste0(current_pkgs, collapse = ", ")
    pb$tick(n, token = list(pkg = current))
  }
  async_tick <- async(function() {
    repeat {
      tick(0)
      await(async::delay(1))
    }
  })

  checks_dir <- fs::path(dir, "checks")
  fs::dir_create(checks_dir)

  async::synchronise(
    async::when_any(
      async_tick(),
      async::async_map(pkgs, .limit = num_workers, async(function(pkg) {
        current_pkgs <<- c(current_pkgs, pkg)

        out <- await(async_catch(async_compare_to_cran(dir, pkg, flavour_pattern)))

        current_pkgs <<- current_pkgs[-match(pkg, current_pkgs)]
        tick(1)

        out
      }))
    )
  )
}

populate_crancache <- function(cache_dir, pkgs, num_workers = 2, exclude = NULL) {
  lib_dir <- tempfile("temp_crancache")
  fs::dir_create(lib_dir)
  on.exit(fs::dir_delete(lib_dir))

  deps <- map(pkgs, revdepcheck:::deps_opts)
  deps_pkgs <- unique(do.call("c", map(deps, `[[`, "package")))
  deps_pkgs <- c(pkgs, deps_pkgs)

  deps_repos <- do.call("c", map(deps, `[[`, "repos"))
  deps_repos <- deps_repos[!duplicated(deps_repos)]

  ensure_binary_pkgtype()
  available <- suppressWarnings(withr::with_envvar(
    c(
      CRANCACHE_REPOS = NULL,
      CRANCACHE_DIR = cache_dir
    ),
    crancache::available_packages(type = "binary")
  ))

  has_binary <- grepl("\\.tgz$", available[, "File"])
  available <- available[has_binary, ]
  deps_pkgs <- deps_pkgs[!deps_pkgs %in% rownames(available)]

  deps_pkgs <- deps_pkgs[!deps_pkgs %in% exclude]
  deps_pkgs <- set_names(deps_pkgs)

  if (!length(deps_pkgs)) {
    return()
  }


  status("INIT", "Populating package cache")

  pb <- progress::progress_bar$new(
    total = length(deps_pkgs),
    format = "[:current/:total] :elapsedfull | ETA: :eta | :pkg"
  )
  current_pkgs <- character()

  tick <- function(n = 1L) {
    current <- paste0(current_pkgs, collapse = ", ")
    pb$tick(n, token = list(pkg = current))
  }
  async_tick <- async(function() {
    repeat {
      tick(0)
      await(async::delay(1))
    }
  })

  results <- async::synchronise(
    async::when_any(
      async_tick(),
      async::async_map(deps_pkgs, .limit = num_workers, async(function(pkg) {
        current_pkgs <<- c(current_pkgs, pkg)

        out <- await(async_catch(async_px_install(
          lib_dir,
          pkg,
          repos = deps_repos,
          cache_dir = cache_dir
        )))

        current_pkgs <<- current_pkgs[-match(pkg, current_pkgs)]
        tick(1)

        out
      }))
    )
  )
  pb$terminate()

  invisible(results)
}

# Suboptimal: Overwrite default value which is "source" on r-devel
# compiled fro_scratch
ensure_binary_pkgtype <- function() {
  if (!identical(Sys.info()[["sysname"]], "Darwin")) {
    # TODO: non-Darwin systems
    return()
  }

  .Platform$pkgType <- "mac.binary"
  base <- ns_env("base")

  env_binding_unlock(base, ".Platform")
  on.exit(env_binding_lock(base, ".Platform"))

  env_bind(base, .Platform = .Platform)
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
  lib_dir <- fs::path(dir, "checks", pkg_name, "library")
  fs::dir_create(lib_dir)

  # Create local crancache
  cache_dir <- fs::path(dir, "cache")
  fs::dir_create(cache_dir)

  opts <- revdepcheck:::deps_opts(pkg_name)
  async_px_install(
    lib_dir = lib_dir,
    pkgs = opts$package,
    repos = opts$repos,
    cache_dir = cache_dir,
    quiet = quiet,
    env = env
  )
}

async_px_install <- function(lib_dir,
                             pkgs,
                             repos,
                             cache_dir = NULL,
                             quiet = FALSE,
                             env = character()) {
  func <- function(lib_dir, pkgs, quiet, repos) {
    ensure_binary_pkgtype <- get("ensure_binary_pkgtype", envir = asNamespace("revdepcheck"))
    ensure_binary_pkgtype()

    # Ensure callr is loaded before changing the lib_dir
    requireNamespace("callr")
    ip <- crancache::install_packages

    withr::with_libpaths(lib_dir, {
      ip(
        pkgs,
        dependencies = FALSE,
        lib = lib_dir[1],
        quiet = quiet,
        repos = repos,
        type = "both"  # Needed to build from cached binaries
      )
      stopifnot(all(pkgs %in% rownames(installed.packages(lib_dir[1]))))
    })
  }

  args <- c(
    list(
      pkgs = pkgs,
      repos = repos,
      lib_dir = lib_dir,
      quiet = quiet
    )
  )

  # CRANCACHE_REPOS is unset to make sure we cache pkgs built
  # locally, which is all pkgs when installing on r-devel.  We
  # install in a local cache to avoid polluting the global cache with
  # pkgs built on random (possibly patched) R versions.
  async_r(
    func = func,
    args = args,
    system_profile = FALSE,
    user_profile = FALSE,
    env = c(
      CRANCACHE_REPOS = NULL,
      CRANCACHE_QUIET = if (quiet) "yes" else "no",
      CRANCACHE_DIR = cache_dir,
      env
    )
  )
}

async_px_download_package <- function(dir, pkg_name) {
  pkg_dir <- fs::path(dir, "checks", pkg_name)
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
  pkg_dir <- fs::path(dir, "checks", pkg_name)
  lib_dir <- fs::path(pkg_dir, "library")

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
      libpath = lib_dir,
      args = c("--no-manual", "--no-build-vignettes", "-o", pkg_dir),
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
