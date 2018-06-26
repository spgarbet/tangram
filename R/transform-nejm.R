
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
                                   test=TRUE,
                                   ...)
{
  pformat <- cell_style[['p']](pformat)

  # Treat overall as a label if it's character
  overall_label <- if(is.null(overall)) "" else { if(is.character(overall)) overall else "Overall" }
  overall       <- !is.null(overall)

  datar      <- row$data
  datac      <- as.categorical(column$data)
  categories <- if(overall) c(levels(datac), overall_label) else levels(datac)

  format <- ifelse(is.na(row$format), format_guess(datar), row$format)

  # Compute N values for each category
  subN <- lapply(levels(datac), FUN=function(cat){
    cell_style[['n']](length(datac[datac == cat & !is.na(datac)]), subcol=cat)
  })

  if(overall) subN[[length(subN)+1]] <- cell_style[['n']]( sum(!is.na(column$data)), subcol="Overall")

  # Kruskal-Wallis via F-distribution
  stat <- if(length(categories) == 1)
  {
    tst <- suppressWarnings(wilcox.test(datar))
    cell_style[['wilcox']](tst$statistic, pformat(tst$p.value))
  }
  else
  {
    tst  <- suppressWarnings(spearman2(datac, datar, na.action=na.retain))
    cell_style[['fstat']](
      f         = render_f(tst['F'], "%.2f"),
      df1       = tst['df1'],
      df2       = tst['df2'],
      p         = pformat(tst['P'])
    )
  }

  tbl <- table %>%
         row_header(derive_label(row)) %>%
         row_header("   Mean") %>%
         row_header("   Median") %>%
         row_header("   Minimum") %>%
         row_header("   Maximum")

  tbl <- if(test) {
    col_header(tbl, "N", categories, "Test Statistic")  %>% col_header("", subN, "")
  } else {
    col_header(tbl, "N", categories)  %>% col_header("", subN)
  }
  tbl <- add_col(tbl, cell_style[['n']](sum(!is.na(datar)),name=NULL))
  tbl <- table_builder_apply(tbl, categories, function(tbl, category) {
     x  <- if(category == overall_label) datar else datar[datac == category]
     tbl <- add_row(tbl, cell(""))
     sapply(c(mean, median, min, max), function (f) {
       tbl <<- add_row(tbl, cell(render_f(f(x, na.rm=TRUE), row$format), subcol=category))
     })
     tbl <- new_col(tbl)
     tbl
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
  Cell        = hmisc_cell,
  Footnote    = "N is the number of non-missing value. ^1^Kruskal-Wallis. ^2^Pearson. ^3^Wilcoxon."
)
