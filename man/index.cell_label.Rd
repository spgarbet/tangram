% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render-index.R
\name{index.cell_label}
\alias{index.cell_label}
\title{Generate an index from a label object}
\usage{
\method{index}{cell_label}(object, id = "tangram", key.len = 4, ...)
}
\arguments{
\item{object}{cell; The cell for indexing}

\item{id}{character; an additional specifier for the object key}

\item{key.len}{numeric; length of key to generate}

\item{...}{additional arguments to renderer. Unused}
}
\value{
A list of strings containing key, source and value
}
\description{
Overrides to generate no indexing on labels
}
