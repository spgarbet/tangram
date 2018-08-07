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


center_decimal <- function(x) gsub("\\.", "\u00b7", x)

#' Create a mean/sd cell object of the given data in Lancet style
#'
#' Create a mean/sd cell object of the given data in Lancet style.
#'
#' @param x numeric vector whose sample quantiles are wanted. NA and NaN values are not allowed in numeric vectors unless na.rm is TRUE.
#' @param format numeric or character; Significant digits or fmt to pass to sprintf
#' @param na.rm logical; if true, any NA and NaN's are removed from x before the quantiles are computed.
#' @param names logical; ignored. For compatibility with hmisc_iqr
#' @param type integer; ignored. For compatibility with hmisc_iqr
#' @param msd logical; ignored. For compatibility with hmisc_iqr
#' @param quant numeric; ignored. For compatibility with hmisc_iqr
#' @param ... additional arguments to constructing cell
#'
#' @return A cell object.
#' @export
#' @importFrom stats sd
#' @examples
#' require(stats)
#' lancet_mean_sd(rnorm(100), '3')
lancet_mean_sd <- function(x,
                          format = NA,
                          na.rm  = TRUE,
                          names  = FALSE,
                          type   = 8,
                          msd    = FALSE,
                          quant  = c(0.25, 0.50, 0.75),
                          ...
                          )
{
  if(is.na(format) || is.null(format)) format <- format_guess(x)

  cell(center_decimal(paste0(
    render_f(mean(x, na.rm=na.rm), format),
    " (",
    render_f(sd(x, na.rm=na.rm), format),
    ")"
  )), ...)
}

#' Create an cell_fraction (S3) in NEJM style of the given data
#'
#' A cell object contains a statistical result of a fraction/percentage in nejm style
#'
#' @param numerator numeric; The value of the numerator
#' @param denominator numeric; The value of the denominator
#' @param format numeric or character; a string formatting directive
#' @param ... optional extra information to attach
#' @return A cell_fraction object.
#' @export
#' @examples
#' lancet_fraction(1, 4, 3)
lancet_fraction <- function(numerator, denominator, format=NULL, ...)
{
  percent      <- 100*numerator / denominator
  if(is.na(format) || is.null(format)) format <- format_guess(percent)
  percent      <- render_f(percent, format)

  num <- gsub("(?!^)(?=(?:\\d{3})+(?:\\.|$))",
              ' ', perl=TRUE,
              str_pad(numerator, nchar(as.character(denominator))))

  cell(paste0(num, " (", center_decimal(percent), "%)"), ...)
}

#' Cell Generation functions for Lancet styling
#'
#' Each function here is called when a cell is generated. Overriding these in a formula call will allows
#' one to customize exactly how each cell's contents are generated.
#' @include compile-cell.R
#' @keywords data
#' @export
lancet_cell <- list(
  n        = cell_n,
  iqr      = lancet_mean_sd,                                 # Reuse Hmisc transform, but instead of IQR, do mean(sd)
  fraction = lancet_fraction,
  fstat    = function(...) center_decimal(hmisc_fstat(...)), # Great example of additional styling
  chi2     = function(...) center_decimal(hmisc_chi2(...)),
  spearman = function(...) center_decimal(hmisc_spearman(...)),
  wilcox   = function(...) center_decimal(hmisc_wilcox(...)),
  p        = hmisc_p
)

