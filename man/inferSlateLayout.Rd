% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blueprint.R
\name{inferSlateLayout}
\alias{inferSlateLayout}
\title{Infer the Parent of Layout Elements By Their Order}
\usage{
inferSlateLayout(layout)
}
\arguments{
\item{layout}{an ordered list of layout elements (pages, groups and inputs).}
}
\value{
The same structure passed in \code{layout} with the appropriate modifications
to \code{parent} values
}
\description{
Infer the Parent of Layout Elements By Their Order
}
\details{
This function takes in a list of pages, groups and inputs and
replaces the \code{parent} of elements with \code{parent = "auto"} with the name of the
previous group or page.
}
