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


#' Render to LaTeX methods for tangram cell objects
#'
#' Each of these methods will render the cell object as a LaTeX fragment
#'
#' @param object object; the item to render to latex
#' @param ... additional arguments
#' @return the LaTeX rendering
#'
#' @include dplr-latexify.R
#' @include compile-cell.R
#'
#' @examples
#' latex(cell_label("123"))
#' latex(cell_iqr(rnorm(20)))
#' latex(cell_estimate(2.1,0.8, 3.3))
#' latex(cell_fraction(45, 137))
#' latex(table_builder()   %>%
#'         row_header("row") %>%
#'         col_header(1,2,3) %>%
#'         add_col("A","B","C"))
#' latex(tangram(drug~bili, pbc))
#' @rdname latex
#' @export
latex.default <- function(object, ...)
{
  warning(paste("summary unhandled class : ", paste(base::class(object), collapse=', ')))

  latexify(paste0(as.character(object), collapse=" "))
}

