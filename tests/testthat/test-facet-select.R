test_that("facet_select filters plot and layer data", {
  plot_data <- data.frame(group = c("a", "b", "a"), value = 1:3)
  layer_data <- data.frame(group = c("a", "b"), value = 4:5)
  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(group, value)) +
    ggplot2::geom_point() +
    ggplot2::geom_point(data = layer_data) +
    ggplot2::facet_wrap(~group) +
    facet_select(group = "a")

  expect_equal(plot$data$group, c("a", "a"))
  expect_equal(plot$layers[[1]]$data, ggplot2::waiver())
  expect_equal(plot$layers[[2]]$data$group, "a")
})

test_that("facet_select validates facet and data variable names", {
  plot_data <- data.frame(group = c("a", "b"), value = 1:2)
  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(group, value))

  expect_error(plot + facet_select(group = "a"), "could not identify variables")
  expect_error(
    plot + ggplot2::facet_wrap(~missing) + facet_select(missing = "a"),
    "No plot or layer data contains selected facet variable"
  )
  expect_error(facet_select("a"), "named facet selections")
  expect_error(facet_select(group = "a", group = "b"), "only once")
  expect_error(
    plot + ggplot2::facet_wrap(~group) + facet_select(not_a_facet = "a"),
    "Not a faceting variable"
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
    facet_select(region = "north")

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

  g_one <- g + facet_select(region = "one")
  g_two <- g + facet_select(region = "two")

  expect_equal(g$layers[[2]]$data$region, c("one", "two", "one"))
  expect_equal(g_one$layers[[2]]$data$region, c("one", "one"))
  expect_equal(g_two$layers[[2]]$data$region, "two")
  expect_equal(as.character(ggplot2::ggplot_build(g_two)$layout$layout$region), "two")
})

test_that("facet_select combines named criteria for facet_grid", {
  plot_data <- data.frame(
    region = c("north", "north", "south", "south"),
    year = c(2023, 2024, 2024, 2024),
    x = 1:4,
    y = 1:4
  )
  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(rows = ggplot2::vars(year), cols = ggplot2::vars(region)) +
    facet_select(region = "north", year = 2024)

  expect_equal(plot$data$region, "north")
  expect_equal(plot$data$year, 2024)
  layout <- ggplot2::ggplot_build(plot)$layout$layout
  expect_equal(layout$region, "north")
  expect_equal(layout$year, 2024)

  region_only <- ggplot2::ggplot(plot_data, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(rows = ggplot2::vars(year), cols = ggplot2::vars(region)) +
    facet_select(region = "north")
  expect_equal(region_only$data$year, c(2023, 2024))
})

test_that("a facet selection can retain multiple levels", {
  plot_data <- data.frame(group = c("a", "b", "c"), value = 1:3)
  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(group, value)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~group) +
    facet_select(group = c("a", "c"))

  expect_equal(plot$data$group, c("a", "c"))
})

test_that("facet_select works with ggh4x facet variants", {
  skip_if_not_installed("ggh4x", minimum_version = "0.3.1")

  basemap <- data.frame(x = c(0, 1), y = c(0, 1))
  overlay <- data.frame(
    region = rep(c("north", "south"), each = 2),
    year = rep(c(2023, 2024), 2),
    x = 1:4,
    y = 1:4
  )
  base_plot <- ggplot2::ggplot(basemap, ggplot2::aes(x, y)) +
    ggplot2::geom_path() +
    ggplot2::geom_point(data = overlay)

  facets <- list(
    facet_wrap2 = ggh4x::facet_wrap2(ggplot2::vars(region, year)),
    facet_grid2 = ggh4x::facet_grid2(
      rows = ggplot2::vars(year), cols = ggplot2::vars(region)
    ),
    facet_nested = ggh4x::facet_nested(
      rows = ggplot2::vars(year), cols = ggplot2::vars(region)
    ),
    facet_nested_wrap = ggh4x::facet_nested_wrap(ggplot2::vars(year, region)),
    facet_manual = ggh4x::facet_manual(
      ggplot2::vars(region, year), design = "AB\nCD"
    )
  )

  for (facet_name in names(facets)) {
    selected <- base_plot + facets[[facet_name]] +
      facet_select(region = "north", year = 2024)
    built_layout <- ggplot2::ggplot_build(selected)$layout$layout

    expect_equal(selected$data, basemap, info = facet_name)
    expect_equal(selected$layers[[2]]$data$region, "north", info = facet_name)
    expect_equal(selected$layers[[2]]$data$year, 2024, info = facet_name)
    expect_equal(nrow(built_layout), 1L, info = facet_name)
    expect_equal(built_layout$region, "north", info = facet_name)
    expect_equal(built_layout$year, 2024, info = facet_name)
  }

  expect_equal(base_plot$layers[[2]]$data, overlay)
})

test_that("facet_select rejects non-ggplot objects", {
  expect_error(
    msander4tools:::facet_select_data(list(), list(group = "a")),
    "`gg` must be a ggplot object"
  )
})
