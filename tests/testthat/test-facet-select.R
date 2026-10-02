test_that("facet_select filters plot and layer data", {
  plot_data <- data.frame(group = c("a", "b", "a"), value = 1:3)
  layer_data <- data.frame(group = c("a", "b"), value = 4:5)
  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(group, value)) +
    ggplot2::geom_point() +
    ggplot2::geom_point(data = layer_data) +
    ggplot2::facet_wrap(~group) +
    facet_select("a")

  expect_equal(plot$data$group, c("a", "a"))
  expect_equal(plot$layers[[1]]$data, ggplot2::waiver())
  expect_equal(plot$layers[[2]]$data$group, "a")
})

test_that("facet_select requires one facet and matching data", {
  plot_data <- data.frame(group = c("a", "b"), value = 1:2)
  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(group, value))

  expect_error(plot + facet_select("a"), "exactly one faceting variable")
  expect_error(
    plot + ggplot2::facet_wrap(~missing) + facet_select("a"),
    "must contain the faceting variable.*in at least one data frame"
  )
})

test_that("facet_select filters facet-bearing layers and preserves shared data", {
  basemap <- data.frame(x = c(0, 1), y = c(0, 1))
  points <- data.frame(
    x = c(0.2, 0.4, 0.6),
    y = c(0.3, 0.5, 0.7),
    region = c("north", "south", "north")
  )

  plot <- ggplot2::ggplot(basemap, ggplot2::aes(x, y)) +
    ggplot2::geom_path() +
    ggplot2::geom_point(data = points) +
    ggplot2::facet_wrap(~region) +
    facet_select("north")

  expect_equal(plot$data, basemap)
  expect_equal(plot$layers[[2]]$data$region, c("north", "north"))
})

test_that("selections from the same plot do not mutate each other", {
  basemap <- data.frame(x = c(0, 1), y = c(0, 1))
  points <- data.frame(
    x = c(0.2, 0.4, 0.6),
    y = c(0.3, 0.5, 0.7),
    region = c("one", "two", "one")
  )
  g <- ggplot2::ggplot(basemap, ggplot2::aes(x, y)) +
    ggplot2::geom_path() +
    ggplot2::geom_point(data = points) +
    ggplot2::facet_wrap(~region)

  g_one <- g + facet_select("one")
  g_two <- g + facet_select("two")

  expect_equal(g$layers[[2]]$data$region, c("one", "two", "one"))
  expect_equal(g_one$layers[[2]]$data$region, c("one", "one"))
  expect_equal(g_two$layers[[2]]$data$region, "two")
  expect_equal(as.character(ggplot2::ggplot_build(g_two)$layout$layout$region), "two")
})

test_that("facet_select rejects non-ggplot objects", {
  expect_error(
    msander4tools:::facet_select_data(list(), "a"),
    "`gg` must be a ggplot object"
  )
})
