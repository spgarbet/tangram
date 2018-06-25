# tangram a general purpose table toolkit for R
# Copyright (C) 2017 Shawn Garbett
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


# Turn a passed pformat into a function (or leave alone)
hmisc_p <- function(pformat)
{
  if(class(pformat) == "function") return(pformat)

  if(is.null(pformat)) pformat <- "%1.3f"

  function(p)
  {
    if(is.na(p) || is.nan(p) || p <0 || p>1) return("NA")

    y <- render_f(p, pformat)

    # Check for all zeros once formated
    test <- grep("[^0\\.]+", y)
    if(length(test) > 0) paste0("P=",y) else paste0("P<", substr(y, 1, nchar(y)-1), "1")
  }
}

#' Create a interquartile range cell object of the given data
#'
#' Construct a cell which has the 3 interquartile ranges specified.
#'
#' @param x numeric vector whose sample quantiles are wanted. NA and NaN values are not allowed in numeric vectors unless na.rm is TRUE.
#' @param format numeric or character; Significant digits or fmt to pass to sprintf
#' @param na.rm logical; if true, any NA and NaN's are removed from x before the quantiles are computed.
#' @param names logical; if true, the result has a names attribute. Set to FALSE for speedup with many probs.
#' @param type integer; specify algorithm to use in constructing quantile. See quantile for more information.
#' @param msd logical; compute an msd attribute containing mean and standard deviation
#' @param quant numeric; The quantiles to display. Should be an odd length vector, since the center value is highlighted.
#' @param ... additional arguments to constructing cell
#'
#' @return A cell_quantile object.
#' @export
#' @importFrom stats quantile
#' @importFrom stats sd
#' @examples
#' require(stats)
#' hmisc_iqr(rnorm(100), '3')
hmisc_iqr <- function(x,
                      format = NA,
                      na.rm  = TRUE,
                      names  = FALSE,
                      type   = 8,
                      msd    = FALSE,
                      quant  = c(0.25, 0.50, 0.75),
                      ...
                          )
{
  if(length(quant) %% 2 == 0) stop("hmisc_iqr quant argument must be an odd length")

  m <- median(1:length(quant))

  y <- quantile(x, quant, na.rm, names, type)

  if(is.na(format)) format <- format_guess(y)
  ql <- sapply(y, function(x) render_f(x, format))
  ql[m] <- paste0("**", ql[m], "**")
  ql <- paste0(ql, collapse=' ')

  if(msd) ql <- paste0(ql, " ",
    render_f(mean(x, na.rm=TRUE), format),
    "\u00b1",
    render_f(sd(x, na.rm=TRUE), format)
    )

  cell(ql, ...)
}

#' Create an cell_fraction (S3) object of the given statistic
#'
#' A hmisc_fraction object contains a statistical result of a fraction/percentage.
#'
#' @param numerator numeric; The value of the numerator
#' @param denominator numeric; The value of the denominator
#' @param format numeric or character; a string formatting directive
#' @param class character; An optional field for additional S3 classes (e.g. could be used in html rendering for CSS)
#' @param ... optional extra information to attach
#' @return A cell_fraction object.
#' @export
#' @examples
#' hmisc_fraction(1, 4, 3)
hmisc_fraction <- function(numerator, denominator, format=3, ...)
{
  ratio      <- render_f(numerator / denominator, format)

  cell(paste0(ratio, " \\frac{",numerator,"}{",denominator,"}"), ...)
}

#' Create an hmisc_fstat (S3) object of the given statistic
#'
#' A hmisc_fstat object contains a statistical result of an F-test.
#'
#' @param f The value of the f-statistic
#' @param df1 1st dimension degrees of freedom
#' @param df2 2nd dimension degrees of freedom
#' @param p The p-value of the resulting test
#' @param class character; An optional field for additional S3 classes (e.g. could be used in html rendering for CSS)
#' @param ... optional extra information to attach
#' @return A cell_fstat object.
#' @export
#' @examples
#' hmisc_fstat(4.0, 10, 20, 0.004039541)
hmisc_fstat <- function(f, df1, df2, p, class=NULL, ...)
{
  cell(paste0("F~", df1, ",", df2, "~=", f, ", ", p, "^1^"), ..., class=c(class, "statistics"))
}

#' Create an hmisc_chi2 (S3) object of the given statistic
#'
#' A hmisc_chi2 object contains a statistical result of an X^2-test.
#'
#' @param chi2 The value of the X^2 statistic
#' @param df degrees of freedom
#' @param p p-value of resulting test
#' @param class character; An optional field for additional S3 classes (e.g. could be used in html rendering for CSS)
#' @param ... optional extra information to attach
#' @return A cell_chi2 object.
#' @export
#' @examples
#' hmisc_chi2(5.33, 6, 0.2)
hmisc_chi2 <- function(chi2, df, p, class=NULL, ...)
{
  cell(paste0("\u03c7^2^~", df, "~=", chi2,", ", p, "^2^"),
       class="statistics",
       ...)
}

#' Create an hmisc_spearman (S3) object of the given statistic
#'
#' A hmisc_spearman object contains a statistical result of an spearman-test.
#'
#' @param S The value of the spearman statistic
#' @param rho The rho value of the test
#' @param p p-value of resulting test
#' @param class character; An optional field for additional S3 classes (e.g. could be used in html rendering for CSS)
#' @param ... optional extra information to attach
#' @return A cell_spearman object.
#' @export
#' @examples
#' hmisc_spearman(20, 0.2, 0.05)
hmisc_spearman <- function(S, rho, p, class=NULL, ...)
{
  cell(paste0(p, "^3^"), class=c(class, "statistics"), ...)
}

#' Create an hmisc_wilcox (S3) object of the given statistic
#'
#' A hmisc_wilcox object contains a statistical result of an Wilcoxon-test.
#'
#' @param S The value of the spearman statistic
#' @param p p-value of resulting test
#' @param class character; An optional field for additional S3 classes (e.g. could be used in html rendering for CSS)
#' @param ... optional extra information to attach
#' @return A cell_spearman object.
#' @export
#' @examples
#' hmisc_wilcox(20, 0.2)
hmisc_wilcox <- function(V, p, class=NULL, ...)
{
  cell(paste0(p, "^3^"), class=c(class, "statistics"), ...)
}

#' Cell Generation functions for hmisc default
#'
#' Each function here is called when a cell is generated. Overriding these in a formula call will allows
#' one to customize exactly how each cell's contents are generated.
#'
#' While this serves as the base template for transforms, it is by no means required if one develops their
#' own bundle of data transforms. One can create ay number of cell level styling choices.
#'
#' @include compile-cell.R
#' @keywords data
#' @export
hmisc_cell <- list(
  n        = cell_n,
  iqr      = hmisc_iqr,
  fraction = hmisc_fraction,
  fstat    = hmisc_fstat,
  chi2     = hmisc_chi2,
  spearman = hmisc_spearman,
  wilcox   = hmisc_wilcox,
  p        = hmisc_p
)

