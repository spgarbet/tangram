# tangram a general purpose table toolkit for R
# Copyright (C) 2017-2018 Shawn Garbett
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

#' Cell Generation functions for hmisc default
#'
#' Each function here is called when a cell is generated. Overriding these in a formula call will allows
#' one to customize exactly how each cell's contents are generated.
#' While this serves as the base template for transforms, it is by no means required if one develops their
#' own bundle of data transforms. One can create ay number of cell level styling choices.
#'
#' @param p numeric; p-value to format
#' @param pformat numeric or character; Significant digits or fmt to pass to sprintf
#' @param include_p logical; include the leading P on the output string
#' @param x numeric; whose sample quantiles are wanted. NA and NaN values are not allowed in numeric vectors unless na.rm is TRUE.
#' @param format numeric or character; Significant digits or fmt to pass to sprintf
#' @param na.rm logical; if true, any NA and NaN's are removed from x before the quantiles are computed.
#' @param names logical; if true, the result has a names attribute. Set to FALSE for speedup with many probs.
#' @param type integer; specify algorithm to use in constructing quantile. See quantile for more information.
#' @param msd logical; compute an msd attribute containing mean and standard deviation
#' @param quant numeric; The quantiles to display. Should be an odd length vector, since the center value is highlighted.
#' @param numerator numeric; The value of the numerator
#' @param denominator numeric; The value of the denominator
#' @param f The value of the f-statistic
#' @param df1 1st dimension degrees of freedom
#' @param df2 2nd dimension degrees of freedom
#' @param class character; An optional field for additional S3 classes (e.g. could be used in html rendering for CSS)
#' @param chi2 The value of the X^2 statistic
#' @param df degrees of freedom
#' @param S The value of the spearman statistic
#' @param rho The rho value of the test
#' @param V The value of the Wilcoxon statistic
#' @param ... additional arguments passed
#' @return A formatted string or cell as appropriate
#' @rdname hmisc_cell
#'
#' @seealso \code{\link{hmisc}}
#'
#' @section \code{hmisc_p}:
#' Given a style in number of digits or a sprintf style specifier it renders
#' the p-value and checks to see if it's all zeros, then switches the
#' output to a less than.
#'
#' @export
#' @examples
#' hmisc_p(1e-6)
#' hmisc_p(0.234)
#' hmisc_p(1.234e-6, 5)
#' hmisc_p(1.234e-6, 6)
hmisc_p <- function(p, pformat="%1.3f", include_p=TRUE)
{
  if(inherits(pformat,"function")) pformat(p)

  if(is.na(p) || is.nan(p) || p <0 || p>1) return("\u2014")

  y <- render_f(p, pformat)

  # Check for all zeros once formated
  test <- grep("[^0\\.]+", y)
  if(length(test) > 0)
  {
    if(include_p) paste0("P=",y) else y
  } else {
    if(include_p) paste0("P<", substr(y, 1, nchar(y)-1), "1") else paste0("<", substr(y, 1, nchar(y)-1), "1")
  }
}

#' @section \code{hmisc_iqr}:
#' Construct a cell which has the interquartile ranges specified.
#'
#' @rdname hmisc_cell
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

  if(msd) ql <- paste0(ql, "\u00A0\u00A0",
    render_f(mean(x, na.rm=TRUE), format),
    "\u00b1",
    render_f(sd(x, na.rm=TRUE), format)
    )

  cell(ql, ...)
}

#' @section \code{hmisc_fraction}:
#' Construct a cell which has the fraction specified in an hmisc format
#'
#' @rdname hmisc_cell
#'
#' @export
#' @examples
#' hmisc_fraction(1, 4, 3)
hmisc_fraction <- function(numerator, denominator, format=3, ...)
{
  ratio      <- numerator / denominator
  if(is.na(format) || is.null(format)) format <- format_guess(ratio)
  ratio      <- render_f(ratio, format)

  cell(paste0(ratio, " \\frac{",str_pad(numerator, nchar(as.character(denominator))),"}{",denominator,"}"), ...)
}


#' @section \code{hmisc_fstat}:
#' Construct a cell which has the fstat specified in an hmisc format.
#'
#' @rdname hmisc_cell
#' @export
#' @examples
#' hmisc_fstat(4.0, 10, 20, 0.004039541)
hmisc_fstat <- function(f, df1, df2, p, class=NULL, ...)
{
  if(is.na(f)) return("")
  ref <- if(df1 == 1) "^3^" else "^1^"
  cell(paste0("F~", df1, ",", df2, "~=", f, ", ", p, ref), ..., class=c(class, "statistics"))
}


#' @section \code{hmisc_chi2}:
#' Construct a cell which has the chi^2 specified in an hmisc format
#'
#' @rdname hmisc_cell
#' @export
#' @examples
#' hmisc_chi2(5.33, 6, 0.2)
hmisc_chi2 <- function(chi2, df, p, class=NULL, ...)
{
  if(is.na(chi2)) return("\u2014")
  cell(paste0("\u03a7^2^~", df, "~=", chi2,", ", p, "^2^"),
       class="statistics",
       ...)
}

#' @section \code{hmisc_spearman}:
#' Construct a cell which has the spearman specified in an hmisc format
#'
#' @rdname hmisc_cell
#' @export
#' @examples
#' hmisc_spearman(20, 0.2, 0.05)
hmisc_spearman <- function(S, rho, p, class=NULL, ...)
{
  if(is.na(S)) return("")
  cell(paste0(p, "^3^"), class=c(class, "statistics"), ...)
}


#' @section \code{hmisc_wilcox}:
#' Construct a cell which has the Wilcoxon specified in an hmisc format
#'
#' @rdname hmisc_cell
#' @export
#' @examples
#' hmisc_wilcox(20, 0.2)
hmisc_wilcox <- function(V, p, class=NULL, ...)
{
  if(is.na(V)) return("")
  cell(paste0(p, "^3^"), class=c(class, "statistics"), ...)
}

#' @section \code{hmisc_cell}:
#' List of data transforms for a cell of a table.
#'
#' \preformatted{
#' hmisc_cell <- list(
#'   n        = cell_n,
#'   iqr      = hmisc_iqr,
#'   fraction = hmisc_fraction,
#'   fstat    = hmisc_fstat,
#'   chi2     = hmisc_chi2,
#'   spearman = hmisc_spearman,
#'   wilcox   = hmisc_wilcox,
#'   p        = hmisc_p
#' )}
#'
#' @include compile-cell.R
#' @rdname hmisc_cell
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

