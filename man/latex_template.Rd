% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render-latex.R
\name{latex_template}
\alias{latex_template}
\title{Return a LaTeX template that works with tangram}
\usage{
latex_template()
}
\value{
filename of LaTeX template
}
\description{
Pandoc in the current version of RStudio does not allow for setting
package options to xcolor and this has made it incompatible with
the LaTeX generated by this package. This provides a known
working template.
}
\details{
An example header would look like the following:

---
title: "A Document Full of Beautiful Tables"
  output:
    pdf_document:
    "`r tangram::latex_template()`"
---
}
\examples{
 latex_template()

}
