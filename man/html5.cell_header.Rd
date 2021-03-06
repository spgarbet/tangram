% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render-html5.R
\name{html5.cell_header}
\alias{html5.cell_header}
\title{Convert an abstract cell_header object into an HTML5 string}
\usage{
\method{html5}{cell_header}(object, id, ..., class = NULL)
}
\arguments{
\item{object}{The cell subheader to render to HTML5}

\item{id}{A unique identifier for traceability}

\item{...}{additional arguments to renderer. Unused}

\item{class}{additional class attributes for CSS rendering}
}
\value{
A text string rendering of the given subheader as a <td> with several <span>'s.
}
\description{
Given a cell_header class create an HTML5 representation.
}
