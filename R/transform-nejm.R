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


#' Create a summarization for a categorical set of column versus a numerical row in NEJM style
#'
#' Given a row and column object from the parser apply a Kruskal test and output
#' the results horizontally. 5 X (n + no. categories + test statistic)
#'
#' @param table The table object to modify
#' @param row The row variable object to use (numerical)
#' @param column The column variable to use (categorical)
#' @param cell_style list; cell styling functions
#' @param pformat numeric, character or function; A formatting directive to be applied to p-values
#' @param msd logical; Include mean and standard deviation with quantile statistics
#' @param quant numeric; Vector of quantiles to include. Should be an odd number since the middle value is highlighted on display.
#' @param overall logical; Include overall summary statistics for a categorical column
#' @param test logical; include statistical test results
#' @param ... absorbs additional arugments. Unused at present.
#' @return The modified table object
#' @export
#' @importFrom magrittr "%>%"
#' @include hmisc-biVar.R
#' @include compile.R
#' @include compile-cell.R
#' @include compile-typing.R
#' @include helper-format.R
#' @include transform-hmisc.R
#' @importFrom stats chisq.test
#' @importFrom stats cor
#' @importFrom stats cor.test
#' @importFrom stats na.omit
#' @importFrom stats wilcox.test
summarize_nejm_horz <-    function(table,
                                   row,
                                   column,
                                   cell_style,
                                   pformat=NULL,
                                   msd=FALSE,
                                   quant=c(0.25, 0.5, 0.75),
                                   overall=NULL,
                                   test=FALSE,
                                   ...)
{
  # Treat overall as a label if it's character
  overall_label <- if(is.character(overall)) overall else "Overall"
  overall       <- column$value != "1" && (isTRUE(overall) || is.character(overall))
  datar         <- row$data
  datac         <- as.categorical(column$data)
  categories    <- if(overall) c(levels(datac), overall_label) else levels(datac)
  categories    <- if(length(categories) == 1) overall_label else categories
  format        <- ifelse(is.na(row$format), format_guess(datar), row$format)

  # Compute N values for each category
  subN <- lapply(levels(datac), FUN=function(cat){
    cell_style[['n']](length(datac[datac == cat & !is.na(datac)]), subcol=cat, hdr=TRUE)
  })

  if(overall) subN[[length(subN)+1]] <- cell_style[['n']]( sum(!is.na(column$data)), hdr=TRUE, subcol="Overall")

  # Test Versus Zero, Wilcox
  stat <- if(length(categories) == 1)
  {
    #tst <- suppressWarnings(wilcox.test(datar))
    #cell_style[['wilcox']](tst$statistic, cell_style[['p']](tst$p.value, pformat))
    ""
  }
  else   # Kruskal-Wallis via F-distribution
  {
    tst  <- suppressWarnings(spearman2(datac, datar, na.action=na.retain))
    cell_style[['fstat']](
      f         = render_f(tst['F'], "%.2f"),
      df1       = tst['df1'],
      df2       = tst['df2'],
      p         = cell_style[['p']](tst['P'], pformat)
    )
  }

  tbl <- table %>%
         row_header(derive_label(row)) %>%
         row_header("   Median (interquartile range)") %>%
         row_header("   Range")

  tbl <- if(test) {
    col_header(tbl, "N", categories, "Test Statistic")  %>% col_header("", subN, "")
  } else {
    col_header(tbl, "N", categories)  %>% col_header("", subN)
  }
  tbl <- add_col(tbl, cell_style[['n']](sum(!is.na(datar)),name=NULL))
  tbl <- table_builder_apply(tbl, categories, function(tbl, category) {
    x   <- if(category == overall_label) datar else datar[datac == category]
    tbl               %>%
    add_row(cell("")) %>%
    add_row(cell_style[['iqr']](x, row$format, subcol=category))   %>%
    add_row(cell_style[['range']](x, row$format, subcol=category)) %>%
    new_col()
  })
  tbl <- home(tbl) %>% cursor_right(length(categories)+1)
  if(test) tbl <- add_col(tbl, stat)

  tbl
}


#' Style Bundle for Closer to NEJM style
#'
#' List of lists, should contain a "Type" entry with a function to determine type of vector passed in.
#' Next entries are keyed off returned types from function, and represent the type of a row.
#' The returned list should contain the same list of types, and represents the type of a column. Thus it now returns
#' a function to process the intersection of those two types.
#'
#' @keywords data
#' @export
#'
nejm <- list(
  Type        = hmisc_data_type,
  Numerical   = list(
                  Numerical   = summarize_spearman,
                  Categorical = summarize_nejm_horz
            ),
  Categorical = list(
                  Numerical   = function(...){stop("Cat X Numerical not implemented in nejm style")},
                  Categorical = summarize_chisq
            ),
  Cell        = nejm_cell,
  Footnote    = "N is the number of non-missing value. ^1^Kruskal-Wallis. ^2^Pearson. ^3^Wilcoxon."
)
