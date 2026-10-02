#' Select data for a ggplot facet
#'
#' Add `facet_select()` to a ggplot to retain only rows whose single faceting
#' variable matches `select`. Data frames without the faceting variable are
#' left unchanged, so shared layers such as basemaps remain visible in each
#' facet.
#'
#' @param select Values to retain for the plot's faceting variable.
#'
#' @return A `facet_select` object that can be added to a ggplot.
#' @export
facet_select <- function(select) {
  structure(list(select = select), class = "facet_select")
}

facet_select_data <- function(gg, select) {
  if (!inherits(gg, "ggplot")) {
    stop("`gg` must be a ggplot object.", call. = FALSE)
  }

  facets <- gg$facet$params$facets
  if (length(facets) != 1L) {
    stop("`facet_select()` currently supports exactly one faceting variable.", call. = FALSE)
  }

  facet_var <- rlang::as_name(facets[[1]])

  contains_facet_var <- function(data) {
    is.data.frame(data) && facet_var %in% names(data)
  }

  layer_has_facet_var <- vapply(gg$layers, function(layer) {
    contains_facet_var(layer$data)
  }, logical(1))

  if (!contains_facet_var(gg$data) && !any(layer_has_facet_var)) {
    stop("The plot data and its layers must contain the faceting variable `", facet_var, "` in at least one data frame.", call. = FALSE)
  }

  if (contains_facet_var(gg$data)) {
    gg$data <- gg$data[gg$data[[facet_var]] %in% select, , drop = FALSE]
  }

  gg$layers <- lapply(gg$layers, function(layer) {
    # ggplot layers are ggproto objects; clone them to avoid mutating layers
    # shared with the original plot or other derived plots.
    layer_copy <- rlang::env_clone(layer)
    class(layer_copy) <- class(layer)
    layer <- layer_copy
    layer_data <- layer$data

    if (contains_facet_var(layer_data)) {
      layer$data <- layer_data[layer_data[[facet_var]] %in% select, , drop = FALSE]
    }

    layer
  })

  gg
}

#' @export
ggplot_add.facet_select <- function(object, plot, object_name) {
  facet_select_data(plot, object$select)
}

#' @export
update_ggplot.facet_select <- function(object, plot, ...) {
  facet_select_data(plot, object$select)
}
