#' Select data for a ggplot facet
#'
#' Add `facet_select()` to a ggplot to retain rows matching named facet
#' selections. Values supplied for one variable are alternatives; selections
#' across variables are combined. Data frames without selected variables are
#' left unchanged, so shared layers such as basemaps remain visible in each
#' facet.
#'
#' @param ... Named facet variables and the level or levels to retain. For
#'   example, `facet_select(region = "north", year = 2025)` keeps rows matching
#'   both selections. A vector value retains any of its levels.
#'
#' @details
#' Works with `facet_wrap()`, `ggh4x::facet_wrap2()`, `ggh4x::facet_grid2()`,
#' `ggh4x::facet_nested()`, `ggh4x::facet_nested_wrap()`, and
#' `ggh4x::facet_manual()` when facets are simple variable names. Argument
#' names must match facet variable names and each selected variable must occur
#' in at least one plot or layer data frame.
#' Selections across arguments are combined with AND; levels within one
#' argument are combined with OR. Unspecified facet variables are unrestricted.
#' A data frame is filtered by selected variables it contains; data frames
#' containing none of them are left unchanged, which is useful for shared layers
#' such as basemaps.
#'
#' @return A `facet_select` object that can be added to a ggplot.
#'
#' @examples
#' plot_data <- data.frame(
#'   continent = rep(c("Americas", "Europe"), each = 4),
#'   region = rep(c("North", "South", "East", "West"), each = 2),
#'   year = rep(c(2024, 2025), 4),
#'   x = 1:8,
#'   y = c(2, 3, 1, 4, 3, 5, 2, 6)
#' )
#'
#' # Standard ggplot2 facet_wrap()
#' ggplot2::ggplot(plot_data, ggplot2::aes(x, y)) +
#'   ggplot2::geom_point() +
#'   ggplot2::facet_wrap(~continent + region + year) +
#'   facet_select(continent = "Americas", region = "North", year = 2025)
#'
#' # ggh4x facet_grid2() and facet_nested()
#' if (requireNamespace("ggh4x", quietly = TRUE)) {
#'   ggplot2::ggplot(plot_data, ggplot2::aes(x, y)) +
#'     ggplot2::geom_point() +
#'     ggh4x::facet_grid2(
#'       rows = ggplot2::vars(year),
#'       cols = ggplot2::vars(continent, region)
#'     ) +
#'     facet_select(continent = "Americas", year = 2025)
#'
#'   ggplot2::ggplot(plot_data, ggplot2::aes(x, y)) +
#'     ggplot2::geom_point() +
#'     ggh4x::facet_nested(
#'       rows = ggplot2::vars(year),
#'       cols = ggplot2::vars(continent, region)
#'     ) +
#'     facet_select(continent = "Americas", region = "North", year = 2025)
#' }
#'
#' @export
facet_select <- function(...) {
  selections <- list(...)
  selection_names <- names(selections)

  if (length(selections) == 0L || is.null(selection_names) ||
      anyNA(selection_names) || any(!nzchar(selection_names))) {
    stop("Supply one or more named facet selections.", call. = FALSE)
  }
  if (anyDuplicated(selection_names)) {
    stop("Each facet variable can be selected only once.", call. = FALSE)
  }

  structure(list(selections = selections), class = "facet_select")
}

facet_select_data <- function(gg, selections) {
  if (!inherits(gg, "ggplot")) {
    stop("`gg` must be a ggplot object.", call. = FALSE)
  }

  facet_params <- gg$facet$params
  if (!is.null(facet_params$facets)) {
    facet_specs <- facet_params$facets
  } else if (!is.null(facet_params$rows) || !is.null(facet_params$cols)) {
    facet_specs <- c(facet_params$rows, facet_params$cols)
  } else {
    stop("`facet_select()` could not identify variables in this facet specification.", call. = FALSE)
  }

  facet_vars <- tryCatch(
    vapply(facet_specs, rlang::as_name, character(1)),
    error = function(e) {
      stop("`facet_select()` requires facets to be simple variable names.", call. = FALSE)
    }
  )

  unknown_vars <- setdiff(names(selections), facet_vars)
  if (length(unknown_vars) > 0L) {
    stop("Not a faceting variable: ", paste(unknown_vars, collapse = ", "), call. = FALSE)
  }

  contains_selection <- function(data, selection_var) {
    is.data.frame(data) && selection_var %in% names(data)
  }

  data_sources <- c(list(gg$data), lapply(gg$layers, `[[`, "data"))
  absent_vars <- names(selections)[!vapply(names(selections), function(variable) {
    any(vapply(data_sources, contains_selection, logical(1), selection_var = variable))
  }, logical(1))]
  if (length(absent_vars) > 0L) {
    stop(
      "No plot or layer data contains selected facet variable(s): ",
      paste(absent_vars, collapse = ", "),
      call. = FALSE
    )
  }

  filter_data <- function(data) {
    if (!is.data.frame(data)) {
      return(data)
    }

    applicable <- intersect(names(selections), names(data))
    if (length(applicable) == 0L) {
      return(data)
    }

    keep <- Reduce(`&`, lapply(applicable, function(variable) {
      data[[variable]] %in% selections[[variable]]
    }))
    data[keep, , drop = FALSE]
  }

  gg$data <- filter_data(gg$data)

  gg$layers <- lapply(gg$layers, function(layer) {
    # ggplot layers are ggproto objects; clone them to avoid mutating layers
    # shared with the original plot or other derived plots.
    layer_copy <- rlang::env_clone(layer)
    class(layer_copy) <- class(layer)
    layer <- layer_copy
    layer$data <- filter_data(layer$data)
    layer
  })

  gg
}

#' @export
ggplot_add.facet_select <- function(object, plot, ...) {
  facet_select_data(plot, object$selections)
}

#' @export
update_ggplot.facet_select <- function(object, plot, ...) {
  facet_select_data(plot, object$selections)
}
