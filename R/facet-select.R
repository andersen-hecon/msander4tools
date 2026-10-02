#' Select data for a ggplot facet
#'
#' Add `facet_select()` to a ggplot to retain only rows whose single faceting
#' variable matches `select`.
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
  if (!is.data.frame(gg$data) || !facet_var %in% names(gg$data)) {
    stop("The plot's main data must contain the faceting variable `", facet_var, "`.", call. = FALSE)
  }

  gg$data <- gg$data[gg$data[[facet_var]] %in% select, , drop = FALSE]

  gg$layers <- lapply(gg$layers, function(layer) {
    layer_data <- layer$data

    if (is.data.frame(layer_data) && facet_var %in% names(layer_data)) {
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
