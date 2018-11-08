#' @import rlang

`%|0|%` <- function(x, y) {
  if (!length(x)) y else x
}

#' @importFrom utils installed.packages
base_packages <- function() {
  rownames(installed.packages(priority="base"))
}

lapply_with_names <- function(X, FUN, ...) {
  n <- if (!is.null(names(X))) names(X) else if (is.character(X)) X
  structure(map(X, FUN, ...), names = n)
}

drop_nulls <- function(x) {
  is_null <- map_lgl(x, is.null)
  x[!is_null]
}

#' @importFrom crayon col_nchar

col_align <- function(text, width = getOption("width"),
                      align = c("left", "center", "right")) {

  align <- match.arg(align)
  nc <- col_nchar(text)

  if (width <= nc) {
    text

  } else if (align == "left") {
    paste0(text, make_space(width - nc))

  } else if (align == "center") {
    paste0(make_space(ceiling((width - nc) / 2)),
           text,
           make_space(floor((width - nc) / 2)))

  } else {
    paste0(make_space(width - nc), text)
  }
}

make_space <- function(num, filling = " ") {
  strrep(filling, num)
}

compact <- function(x) Filter(Negate(is.null), x)

clear_line <- function(width = getOption("width")) {
  spaces <- paste(rep(" ", width), collapse = "")
  cat("\r", spaces, "\r", sep = "")
}

str_trunc <- function(x, n) {
  if (n <= 3) {
    substr("...", 1, n)
  } else if (nchar(x) < n) {
    x
  } else {
    paste0(substr(x, 1, n - 3), "...")
  }
}


#' @importFrom withr with_libpaths with_envvar

execute_r <- function(px_opts, new_session = FALSE) {
  if (new_session) {
    do.call(r, px_opts)
  } else {
    rlang::with_options(
      repos = px_opts$repos,
      with_libpaths(px_opts$libpath,
        with_envvar(px_opts$env,
          do.call(px_opts$func, px_opts$args)
        )
      )
    )
  }
}

str_trim <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}

cut_into_lines <- function(x) {
  x <- do.call(paste0, as.list(x))
  x <- gsub("\r\n", "\n", x, fixed = TRUE)
  x <- strsplit(x, "\n", fixed = TRUE)[[1]]
  if (length(x)) x else ""
}

flatten_names <- function(x) {
  x <- map2(x, names(x), function(v, n) {
    set_names(v, rep_along(v, n))
  })
  unname(x)
}

bang <- function(expr) {
  eval_tidy(enquo(expr), caller_env())
}

full_join <- function(x, y, by) {
  merge_(x, y, by, all.x = TRUE, all.y = TRUE)
}
left_join <- function(x, y, by) {
  merge_(x, y, by, all.x = TRUE, all.y = FALSE)
}
merge_ <- function(x, y, by, ...) {
  out <- merge(x, y, by, ...)

  # Sort columns with original order
  nms <- unique(c(names(x), names(y)))
  out <- out[nms]

  as_tibble(out)
}

join <- function(.by,
                 ...,
                 .keep = FALSE,
                 .unmatched = c("drop", "error")) {
  dfs <- list2(...)
  check_join_inputs(.by, dfs)

  to_nest <- map_lgl(dfs, is_box, "join_nesting_box")
  dfs[to_nest] <- map(dfs[to_nest], unbox)

  dfs <- map(dfs, as_tibble)
  to_splice <- names(dfs) == ""

  strict <- match.arg(.unmatched) == "error"
  inds <- join_indices(.by, dfs, strict)

  by_col <- dfs[[1]][[.by]][inds[[1]]] %||% .by[int()]
  dfs <- map2(dfs, to_splice, unselect_by, .by, .keep)

  dfs[to_nest] <- pmap(
    list(dfs[to_nest], inds[to_nest], to_splice[to_nest]),
    subset_nested,
    keep = .keep,
    by = .by
  )

  dfs[!to_nest] <- pmap(
    list(dfs[!to_nest], inds[!to_nest], to_splice[!to_nest]),
    subset_unnested,
    keep = .keep,
    by = .by
  )

  dfs <- flatten_if(dfs, is_spliced)
  tibble(!!.by := by_col, !!!dfs)
}

nesting <- function(x) {
  new_box(x, "join_nesting_box")
}
is_nesting_box <- function(x) {
  is_box(x, "join_nesting_box")
}

join_indices <- function(by, dfs, strict = FALSE) {
  keys <- map(dfs, `[[`, by)
  matched <- matched_keys(keys, strict)

  # Pair each key with a row index
  keys <- map(keys, tibble::enframe, name = "index")

  # Drop unmatched
  keys <- map2(keys, matched, function(key, idx) key[idx, ])

  # Take first key vector ordering
  model <- keys[[1]]$value

  # Match keys to model and reorder the key index
  map(keys, function(key) {
    key <- key[match(model, key$value), ]
    key$index
  })
}

matched_keys <- function(keys, strict) {
  matched <- new_list(length(keys))

  for (i in seq_along(keys)) {
    key <- keys[[i]]
    keep <- reduce(keys[-i], .init = TRUE, function(keep, other) {
      keep & key %in% other
    })

    if (strict && !all(keep)) {
      abort("Join keys can't be unmatched")
    }

    matched[[i]] <- which(keep)
  }

  matched
}

unselect_by <- function(df, splice, by, keep) {
  if (!keep || splice) {
    df <- df[-match(by, names(df))]
  }
  df
}

subset_nested <- function(df, idx, splice, keep, by) {
  n <- length(idx)
  ptype <- df[int(), ]

  list_col <- rep_len(list(ptype), n)
  for (i in seq_len(n)) {
    list_col[[i]] <- df[idx[[i]], ]
  }

  if (splice) {
    splice(transpose(list_col))
  } else {
    list_col
  }
}
subset_unnested <- function(df, idx, splice, keep, by) {
  df <- df[idx, ]

  if (splice) {
    splice(df)
  } else {
    df
  }
}

check_join_inputs <- function(by, dfs) {
  stopifnot(
    # Multiple keys are unimplemented
    is_string(by),

    is_list(dfs) && length(dfs) >= 1L,

    every(dfs, is_join_df, by = by)
  )

  spliced <- dfs[names(dfs) == ""]
  spliced <- map_if(spliced, is_nesting_box, unbox)

  spliced <- map(spliced, function(df) df[-match(by, names(df))])
  all_nms <- unlist(map(spliced, names))
  if (anyDuplicated(all_nms)) {
    abort("Can't join tibbles with homonym columns")
  }
}

is_join_df <- function(df, by) {
  if (is_nesting_box(df)) {
    df <- unbox(df)
  }

  if (!is.data.frame(df)) {
    abort("Can't join with objects that are not data frames")
  }

  if (!has_name(df, by)) {
    abort("Can't join data frame that has no key column")
  }

  if (anyDuplicated(df[[by]])) {
    abort("Join keys can't be duplicated")
  }

  TRUE
}

groups_join <- function(packages, db) {
  x <- unduplicate(packages)
  y <- db_groups(db)

  empty_x <- !NROW(x)
  empty_y <- !NROW(y)

  if (empty_x && empty_y) {
    tibble()
  } else if (empty_x) {
    y
  } else if (empty_y) {
    x
  } else {
    unduplicate(full_join(x, y, "package"))
  }
}

unduplicate <- function(x, ...) {
  empty_dims <- n_dim(x) - 1L
  empty_args <- rep_len(list(expr()), empty_dims)

  subset <- x[...]
  dups <- duplicated(subset)

  bang(x[which(!dups), !!!empty_args])
}
n_dim <- function(x) {
  dim <- dim(x) %|0|% 1L
  length(dim)
}
