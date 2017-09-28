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

#' Create a summarization for a categorical set of column versus a numerical row
#'
#' Given a row and column object from the parser apply a Kruskal test and output
#' the results horizontally. 1 X (n + no. categories + test statistic)
#'
#' @param table The table object to modify
#' @param row The row variable object to use (numerical)
#' @param column The column variable to use (categorical)
#' @param pformat numeric or character; A formatting directive to be applied to p-values
#' @param msd logical; Include mean and standard deviation with quantile statistics
#' @param quant numeric; Vector of quantiles to include. Should be an odd number since the middle value is highlighted on display.
#' @param overall logical; Include overall summary statistics for a categorical column
#' @param ... absorbs additional arugments. Unused at present.
#' @return The modified table object
#' @export
#' @importFrom magrittr "%>%"
#' @include hmisc-biVar.R
#' @include compile.R
#' @include compile-cell.R
#' @include compile-typing.R
#' @include helper-format.R
#' @importFrom stats chisq.test
#' @importFrom stats cor
#' @importFrom stats cor.test
#' @importFrom stats na.omit
#' @importFrom stats wilcox.test
summarize_kruskal_horz <- function(table,
                                   row,
                                   column,
                                   pformat=NULL,
                                   msd=FALSE,
                                   quant=c(0.25, 0.5, 0.75),
                                   overall=NULL,
                                   ...)
{
  if(is.null(pformat)) pformat <- "%1.3f"

  # Treat overall as a label if it's character
  overall_label <- if(is.null(overall)) "" else { if(is.character(overall)) overall else "Overall" }
  overall       <- !is.null(overall)

  datar      <- row$data
  datac      <- as.categorical(column$data)
  categories <- if(overall) c(levels(datac), overall_label) else levels(datac)

  format <- ifelse(is.na(row$format), format_guess(datar), row$format)

  # Compute N values for each category
  subN <- lapply(levels(datac), FUN=function(cat){
    cell_n(length(datac[datac == cat & !is.na(datac)]), subcol=cat)
  })

  if(overall) subN[[length(subN)+1]] <- cell_n( sum(!is.na(column$data)), subcol="Overall")

  # Kruskal-Wallis via F-distribution
  stat <- if(length(categories) == 1)
  {
    cell(suppressWarnings(wilcox.test(datar)), pformat=pformat, reference="3")
  }
  else
  {
    test  <- suppressWarnings(spearman2(datac, datar, na.action=na.retain))
    cell_fstat(f         = render_f(test['F'], "%.2f"),
               df1       = test['df1'],
               df2       = test['df2'],
               p         = render_f(test['P'], pformat),
               reference = "1")
  }

  tbl <- table                                   %>%
  row_header(derive_label(row))                  %>%
  col_header("N", categories, "Test Statistic")  %>%
  col_header("",  subN,       ""              )  %>%
  add_col(cell_n(sum(!is.na(datar)),name=NULL))            %>%
  table_builder_apply(categories, function(tbl, category) {
     x  <- if(category == overall_label) datar else datar[datac == category]

     add_col(tbl, cell_iqr(x, format, na.rm=TRUE, subcol=category, msd=msd, quant=quant))
  })

  tbl <- add_col(tbl, stat)

  tbl
}

#' Create a summarization for a categorical row versus a numerical column
#'
#' Given a row and column object from the parser apply a Kruskal test and output
#' the results vertically (#Categories+1) X (N, Summary, Statistic)
#'
#' @param table The table object to modify
#' @param row The row variable object to use (categorical)
#' @param column The column variable to use (numerical)
#' @param pformat numeric or character; A formatting directive to be applied to p-values
#' @param ... absorbs additional arugments. Unused at present.
#' @return The modified table object
#' @export
summarize_kruskal_vert <- function(table, row, column, pformat=NULL, ...)
{
  if(is.null(pformat)) pformat <- "%1.3f"

  datar      <- as.categorical(row$data)
  datac      <- column$data
  categories <- levels(datar)

  # Kruskal-Wallis via F-distribution
  test  <- suppressWarnings(spearman2(datar, datac, na.action=na.retain))
  fstat <- cell_fstat(f   = render_f(test['F'], "%.2f"),
                      df1 = test['df1'],
                      df2 = test['df2'],
                      p   = render_f(test['P'], pformat),
                      reference = "1")

  table                                                             %>%
  col_header("N", derive_label(column), "Test Statistic")           %>%
  row_header(derive_label(row))                                     %>%
# FIXME Need to handle single case consistent with chisq
  new_line()                                                        %>%
  table_builder_apply(categories, FUN=function(tbl, category) {
    x <- datac[datar == categories[category]]
    tbl                                                  %>%
    row_header(category)                                 %>%
    add_col(cell(length(x), subcol=category))            %>%
    add_col(cell_iqr(x, column$format, na.rm=TRUE, subrow=category)) %>%
    new_line()
  })                                                                %>%
  cursor_pos(1, 3)                                                  %>%
  add_col(fstat)
}

#' Create a summarization for a categorical row versus a categorical column
#'
#' Given a row and column object from the parser apply a chi^2 test and output
#' the results
#'
#' @param table The table object to modify
#' @param row The row variable object to use (categorical)
#' @param column The column variable to use (categorical)
#' @param pformat numeric or character; A formatting directive to be applied to p-values
#' @param collapse_single logical; default TRUE. Categorical variables with a two values collapse to single row.
#' @param overall logical; Include the overall summary column
#' @param ... absorbs extra parameters. Currently unused.
#' @return The modified table object
#' @export
summarize_chisq <- function(table,
                            row,
                            column,
                            pformat=NULL,
                            collapse_single=TRUE,
                            overall=NULL,
                            ...)
{
  if(is.null(pformat)) pformat <- "%1.3f"

  grid          <- table(as.categorical(row$data), as.categorical(column$data), useNA="no")
  validcol      <- which(!apply(grid,2,FUN = function(x){all(x == 0)}))
  validrow      <- which(!apply(grid,1,FUN = function(x){all(x == 0)}))
  test          <- if(length(grid[validrow,validcol]) < 2) NA else suppressWarnings(chisq.test(grid[validrow,validcol], correct=FALSE))
  ncol          <- dim(grid)[2]
  nrow          <- dim(grid)[1]

  denominators  <- matrix(rep(colSums(grid), nrow), ncol=ncol, byrow=TRUE)
  rowlabels     <- rownames(grid)

  # Compute overall N values for each category
  # length(datac[datac == cat & !is.na(datac)])
  subN <- lapply(colnames(grid), FUN=function(cat)
    cell_n( sum(column$data == cat, na.rm=TRUE), subcol=cat)
  )

  if(!is.null(overall))
  {
    denominators <- cbind(denominators, rep(sum(grid), nrow))
    grid         <- cbind(grid,         rowSums(grid))
    colnames(grid)[ncol+1] <- if(is.character(overall)) overall else "Overall"
    subN[[ncol+1]] <- cell_n( sum(!is.na(column$data)), subcol="Overall")
    ncol         <- ncol + 1
  }

  # Collapse to a single line when requested for 2 binomial factors
  if(collapse_single && dim(grid)[1]==2)
  {
    # Why is this so difficult?

    # More complex name derivation
    name <- row$name()
    try({
          l2 <- attr(row$data, "label")
          if(!is.null(l2)) {name<-l2}
    })

    # Select part of grid table, then do all the munging to get it back in form
    x <- matrix(grid[2,], nrow=1)
    colnames(x) <- colnames(grid)
    rownames(x) <- paste(name,":", rownames(grid)[2])
    grid <- x
    denominators <- matrix(denominators[2,], nrow=1)
    nrow <- 1
  }
  else # Give a good indent otherwise
  {
    rownames(grid)   <- lapply(rownames(grid), FUN=function(x) paste("  ", x))
  }

  # Column Headers
  table <- col_header(table, "N", colnames(grid), "Test Statistic")
  table <- col_header(table, "", subN, "")

  # Row Headers
  if(nrow > 1) table <- row_header(table, derive_label(row)) # Deal with single
  for(nm in rownames(grid)) table <- row_header(table, nm)

  # The N value
  table <- add_col(table, sum(!is.na(row$data)))

  # Now loop the grid into the table as a fraction
  for(j in 1:ncol)
  {
    if(nrow > 1) table <- add_row(table, "")
    for(i in 1:nrow)
    {
      table <-
        if(denominators[i,j] == 0)
          add_row(table, "")
        else
          add_row(table,
                  cell_fraction(grid[i,j], denominators[i,j],
                                format=row$format,
                                subcol=colnames(grid)[i], subrow=rownames(grid)[j]))
    }
    table <- new_col(table)
  }

  # Finally add the stats
  table <- add_row(table, cell(test, reference="2", pformat=pformat))

  # Fill in blank cells in stats column
  if(nrow > 1) table <- add_row(table, rep("", nrow))

  table
}

#' Create a summarization for a numerical row versus a numerical column
#'
#' Given a row and column object from the parser apply a Spearman test and output
#' the results in a 1X3 format.
#'
#' @param table The table object to modify
#' @param row The row variable object to use (numerical)
#' @param column The column variable to use (numerical)
#' @param pformat numeric or character; A formatting directive to be applied to p-values
#' @param ... absorbs additional arguments. Unused at present.
#' @return The modified table object
#' @export
summarize_spearman <- function(table, row, column, pformat=NULL, ...)
{
  if(is.null(pformat)) pformat <- "%1.3f"

  datar <- row$data
  datac <- column$data

  test  <- suppressWarnings(cor.test(datar, datac, alternate="two.sided", method="spearman", na.action=na.omit, exact=FALSE))

  table %>%
  row_header(derive_label(row)) %>%
  col_header("N", derive_label(column), "Test Statistic") %>%
  col_header("", "", "") %>%
  add_col(sum(!is.na(datar) & !is.na(datac))) %>%
  add_col(test$estimate) %>%
  add_col(cell(test, pformat=pformat))
}

apply_factors <- function(row, column)
{
  stop("Not Implemented")
}

#' Determine data type of a vector loosely consistent with Hmisc.
#'
#' @param x Vector to determine type of
#' @param category_threshold The upper threshold of unique values for which a vector is considered categorical.
#'
#' @return One of the following strings: Binomial, Categorical, or Numerical.
#' @export
#'
#' @examples
#'
#' hmisc_data_type(c(1,2,3))
#' hmisc_data_type(factor(c("A","B","C")))
#' hmisc_data_type(factor(c("A","B","B","A")))
#' hmisc_data_type(factor(c(TRUE, FALSE, TRUE, FALSE)))
#'
hmisc_data_type <- function(x, category_threshold=NA)
{
  #if(inherits(x,"data.frame")) x <- x[,1] # FIXME: Need to deal with factors
  if(is.categorical(x,category_threshold))  "Categorical" else
  if(is.numeric(x))                         "Numerical"   else
  stop(paste("Unsupported class/type - ",class(x), typeof(x)))
}

#' Style Bundle for Hmisc defaults.
#'
#' List of lists, should contain a "Type" entry with a function to determine type of vector passed in.
#' Next entries are keyed off returned types from function, and represent the type of a row.
#' The returned list should contain the same list of types, and represents the type of a column. Thus it now returns
#' a function to process the intersection of those two types.
#'
#' @keywords data
#' @export
#'
hmisc_style <- list(
  Type        = hmisc_data_type,
  Numerical   = list(
                  Numerical   = summarize_spearman,
                  Categorical = summarize_kruskal_horz
            ),
  Categorical = list(
                  Numerical   = summarize_kruskal_vert,
                  Categorical = summarize_chisq
            ),
  Footnote    = "N is the number of non-missing value. ^1^Kruskal-Wallis test. ^2^Pearson test"
)

#' Mayo Clinic Primary Biliary Cirrhosis Data
#'
#' D This data is from the Mayo Clinic trial in primary biliary cirrhosis (PBC) of the liver conducted between 1974 and 1984. A total of 424 PBC patients, referred to Mayo Clinic during that ten-year interval, met eligibility criteria for the randomized placebo controlled trial of the drug D-penicillamine. The first 312 cases in the data set participated in the randomized trial and contain largely complete data. The additional 112 cases did not participate in the clinical trial, but consented to have basic measurements recorded and to be followed for survival. Six of those cases were lost to follow-up shortly after diagnosis, so the data here are on an additional 106 cases as well as the 312 randomized participants.
#'
#' A nearly identical data set found in appendix D of Fleming and Harrington; this version has fewer missing values.
#'
#' Included for use in example from Hmisc.
#'
#' @name pbc
#' @docType data
#' @keywords data
"pbc"
