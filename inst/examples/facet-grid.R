# Explore named facet_select() arguments with several facet implementations.
# Run after installing msander4tools or loading it with devtools::load_all().

library(ggplot2)
library(msander4tools)

plot_data <- data.frame(
  continent = rep(c("Americas", "Europe"), each = 4),
  region = rep(c("North", "South", "East", "West"), each = 2),
  year = rep(c(2024, 2025), 4),
  x = 1:8,
  y = c(2, 3, 1, 4, 3, 5, 2, 6)
)

# Standard ggplot2 facet_wrap()
g_wrap <- ggplot(plot_data, aes(x, y)) +
  geom_point() +
  facet_wrap(~continent + region + year) +
  facet_select(continent = "Americas", region = "North", year = 2025)
g_wrap

# ggh4x facet_grid2() and facet_nested()
if (requireNamespace("ggh4x", quietly = TRUE)) {
  g_grid2 <- ggplot(plot_data, aes(x, y)) +
    geom_point() +
    ggh4x::facet_grid2(
      rows = vars(year),
      cols = vars(continent, region)
    ) +
    facet_select(continent = "Americas", year = 2025)
  g_grid2

  g_nested <- ggplot(plot_data, aes(x, y)) +
    geom_point() +
    ggh4x::facet_nested(
      rows = vars(year),
      cols = vars(continent, region)
    ) +
    facet_select(continent = "Americas", region = "North", year = 2025)
  g_nested
}
