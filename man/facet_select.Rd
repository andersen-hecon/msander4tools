\name{facet_select}
\alias{facet_select}
\title{Select data for a ggplot facet}
\usage{
facet_select(select)
}
\arguments{
\item{select}{Values to retain for the plot's faceting variable.}
}
\value{
A \code{facet_select} object that can be added to a ggplot.
}
\description{
Add \code{facet_select()} to a ggplot to retain only rows whose single
faceting variable matches \code{select}.
}
