test_that("facet_select filters plot and layer data", {
  plot_data <- data.frame(group = c("a", "b", "a"), value = 1:3)
  layer_data <- data.frame(group = c("a", "b"), value = 4:5)
  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(group, value)) +
    ggplot2::geom_point() +
    ggplot2::geom_point(data = layer_data) +
    ggplot2::facet_wrap(~group) +
    facet_select("a")

  expect_equal(plot$data$group, "a")
  expect_equal(plot$layers[[1]]$data, ggplot2::waiver())
  expect_equal(plot$layers[[2]]$data$group, "a")
})

test_that("facet_select requires one facet and matching plot data", {
  plot_data <- data.frame(group = c("a", "b"), value = 1:2)
  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(group, value))

  expect_error(plot + facet_select("a"), "exactly one faceting variable")
  expect_error(
    plot + ggplot2::facet_wrap(~missing) + facet_select("a"),
    "main data must contain the faceting variable"
  )
})

test_that("facet_select rejects non-ggplot objects", {
  expect_error(
    msander4tools:::facet_select_data(list(), "a"),
    "`gg` must be a ggplot object"
  )
})
