\name{facet_select}
\alias{facet_select}
\title{Select data for a ggplot facet}
\usage{
facet_select(...)
}
\arguments{
\item{...}{Named facet variables and the level or levels to retain. For
example, \code{facet_select(region = "north", year = 2025)} keeps rows matching
both selections. A vector value retains any of its levels.}
}
\details{
Works with \code{facet_wrap()}, \code{ggh4x::facet_wrap2()},
\code{ggh4x::facet_grid2()}, \code{ggh4x::facet_nested()},
\code{ggh4x::facet_nested_wrap()}, and \code{ggh4x::facet_manual()} when
facets are simple variable names. Argument names must match facet variable
names and each selected variable must occur in at least one plot or layer data
frame.
Selections across arguments are combined with AND; levels within one
argument are combined with OR. Unspecified facet variables are unrestricted.
A data frame is filtered by selected variables it contains; data frames
containing none of them are left unchanged, which is useful for shared layers
such as basemaps.
}
\value{
A \code{facet_select} object that can be added to a ggplot.
}
\description{
Add \code{facet_select()} to a ggplot to retain rows matching named facet
selections. Values supplied for one variable are alternatives; selections
across variables are combined. Data frames without selected variables are
left unchanged, so shared layers such as basemaps remain visible in each
facet.
}
\examples{
plot_data <- data.frame(
  continent = rep(c("Americas", "Europe"), each = 4),
  region = rep(c("North", "South", "East", "West"), each = 2),
  year = rep(c(2024, 2025), 4),
  x = 1:8,
  y = c(2, 3, 1, 4, 3, 5, 2, 6)
)
# Standard ggplot2 facet_wrap()
ggplot2::ggplot(plot_data, ggplot2::aes(x, y)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~continent + region + year) +
  facet_select(continent = "Americas", region = "North", year = 2025)

# ggh4x facet_grid2() and facet_nested()
if (requireNamespace("ggh4x", quietly = TRUE)) {
  ggplot2::ggplot(plot_data, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggh4x::facet_grid2(
      rows = ggplot2::vars(year),
      cols = ggplot2::vars(continent, region)
    ) +
    facet_select(continent = "Americas", year = 2025)

  ggplot2::ggplot(plot_data, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggh4x::facet_nested(
      rows = ggplot2::vars(year),
      cols = ggplot2::vars(continent, region)
    ) +
    facet_select(continent = "Americas", region = "North", year = 2025)
}
}
