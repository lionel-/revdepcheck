
#' @importFrom remotes bioc_install_repos
#' @importFrom crancache available_packages

pkgs_revdeps <- function(package,
                         dependencies = c("Depends", "Imports",
                                          "Suggests", "LinkingTo"),
                         bioc = TRUE) {
  stopifnot(is_string(package))
  repos <- get_repos(bioc = bioc)

  pkgs <- flatten_names(map(repos, get_packages, package, dependencies))
  pkgs <- map(pkgs, tibble::enframe, name = "repo", value = ".package")

  exec("rbind", !!!pkgs)
}

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
