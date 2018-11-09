#' Compute package revdeps
#'
#' `pkg_revdeps()` returns a tibble of reverse dependencies that you
#' can pass to `revdep_check()`.
#'
#' @inheritParams revdep_check
#' @param dependencies Which types of revdeps should be checked. For
#'   CRAN release, we recommend using the default.
#' @param bioc Also check revdeps that live in BioConductor?
#'
#' @export
#' @examples
#' if (FALSE) {
#'
#' # Compute revdeps of rlang
#' pkgs_revdeps("rlang")
#'
#' # Don't include Bioconductor revdeps
#' pkgs_revdeps("rlang", bioc = FALSE)
#'
#' # Compute revdeps of rlang, dplyr and purrr
#' pkgs_revdeps(c("rlang", "dplyr", "purrr"))
#'
#' }
pkgs_revdeps <- function(pkg,
                         dependencies = c("Depends", "Imports",
                                          "Suggests", "LinkingTo"),
                         bioc = TRUE) {
  stopifnot(is_character(pkg))
  n <- length(pkg)

  if (n == 0) {
    return(tibble(pkg = chr()))
  }

  repos <- get_repos(bioc = bioc)
  if (n == 1) {
    data <- pkgs_revdeps_data(repos, pkg, dependencies)
    data <- pkgs_revdeps_subset(data)
    return(data)
  }

  pkgs_data <- map(set_names(pkg), pkgs_revdeps_data, repos = repos, dependencies)
  data <- bang(rbind(!!!pkgs_data))

  set <- imap(pkgs_data, function(df, n) rep_len(n, NROW(df)))
  set <- bang(c(!!!unname(set)))

  data <- tibble(set = set, !!!data)
  pkgs_revdeps_subset(data)
}

pkgs_revdeps_subset <- function(pkgs) {
  pkgs <- pkgs[!duplicated(pkgs$package), ]

  # For easier debugging
  if (is_true(peek_option("revdepcheck__limit_revdeps"))) {
    group_nms <- names(pkgs)[-match("package", names(pkgs))]
    subset_groups(pkgs, group_nms, 2)
  } else {
    pkgs
  }
}

pkgs_revdeps_data <- function(repos, package, dependencies) {
  pkgs <- flatten_names(map(repos, get_packages, package, dependencies))
  pkgs <- map(pkgs, tibble::enframe, name = "repo", value = "package")
  pkgs <- bang(rbind(!!!pkgs))
  pkgs
}

#' @importFrom remotes bioc_install_repos
get_repos <- function(bioc) {
  repos <- c(
    getOption("repos"),
    if (bioc) bioc_install_repos()
  )
  if (! "CRAN" %in% names(repos) || repos["CRAN"] == "@CRAN@") {
    repos["CRAN"] <- "https://cloud.r-project.org"
  }

  ## Drop duplicated repos (by name only)
  names <- names(repos)
  repos <- repos[!(nzchar(names) & duplicated(names))]

  repos
}

#' @importFrom crancache available_packages
get_packages <- function(repos, package, dependencies) {
  if (!length(repos)) {
    return(chr())
  }

  allpkgs <- available_packages(repos = repos)
  alldeps <- allpkgs[, dependencies, drop = FALSE]
  alldeps[is.na(alldeps)] <- ""
  deps <- apply(alldeps, 1, paste, collapse = ",")
  rd <- grepl(paste0("\\b", package, "\\b"), deps)

  pkgs <- unname(allpkgs[rd, "Package"])
  pkgs[order(tolower(pkgs))]
}

cran_deps <- function(package, repos) {
  allpkgs <- available_packages(repos = repos)
  current <- deps <- package
  dependencies <- c("Depends", "Imports", "LinkingTo", "Suggests")
  while (TRUE) {
    deprecs <- allpkgs[ allpkgs[, "Package"] %in% deps, dependencies ]
    newdeps <- unlist(parse_deps(deprecs))
    deps <- unique(sort(c(deps, newdeps)))
    if (identical(current, deps)) break
    dependencies <- c("Depends", "Imports", "LinkingTo")
    current <- deps
  }

  setdiff(deps, c(package, base_packages()))
}

parse_deps <- function(deps) {
  deps[is.na(deps)] <- ""
  deps <- gsub("\\s+", "", deps)
  deps <- gsub("\\([^)]+\\)", "", deps)
  notempty <- nzchar(deps)
  res <- replicate(length(deps), character())
  deps <- deps[notempty]
  deps <- strsplit(deps, ",", fixed = TRUE)

  base <- base_packages()
  deps <- map(deps, setdiff, y = c("R", base))

  res[notempty] <- deps
  res
}

pkgs_validate <- function(packages) {
  if (is_character(packages)) {
    data <- tibble(package = packages)
    return(data)
  }

  if (!is_pkgs_revdeps(packages)) {
    abort("`packages` must be a character vector or a data frame with a `package column`")
  }

  packages <- as_tibble(packages)
  unduplicate(packages, "package")
}
is_pkgs_revdeps <- function(x) {
  is.data.frame(x) && has_name(x, "package")
}
