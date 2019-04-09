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


#' Create an SMD mean and standard deviation cell
#'
#' Create an SMD mean and standard deviation cell. In this case it prints the
#' mean with the standard deviation in parenthesis
#'
#' @param x vector; variable to evaluate with smd
#' @param format formatting to apply to result
#' @param ... additional arguments to pass to cell generation
#' @return a tangram cell
#' @export
#' @importFrom stringr str_sub
smd_meansd <- function(x, format, ...)
{
  if(sum(!is.na(x))==0) return(cell("", ...))
  cell(paste0(
         render_f(mean(x, na.rm=TRUE), format),
         " (", render_f(sd(x, na.rm=TRUE), format), ")"
             ),
       ...)
}

#' Create an SMD distance cell
#'
#' Create an SMD distance cell. It calls the smd function then formats the result.
#' If the result rounds to all zeros then it appends a less than sign and
#' bumps the least significant digit to one.
#'
#' @param x vector; variable to evaluate with smd
#' @param group factor; A grouping to apply. Must have 2 levels.
#' @param format formatting to apply to result
#' @param weight numeric; Weighting to apply to computation. Defaults to NULL.
#' @param ... additional arguments to pass to cell generation
#' @return a tangram cell
#' @export
#' @importFrom stringr str_sub
smd_dist <- function(x, group, format, weight=NULL, ...)
{
  if(sum(!is.na(x))==0 || sum(!is.na(x))==0) return(cell("NA", ...))
  if(is.null(format)) format <- 3

  d <- render_f(standard_difference(x, group, weight), format)

  # Check for all zeros once formated
  test <- grep("[^0\\.]+", d)
  if(length(test) > 0) return(cell(d, ...)) # It's good!

  # Otherwise, append less than
  cell(paste0("<", substr(d, 1, nchar(d)-1), "1"), ...)
}

#' Create a fraction cell in the smd transform
#'
#' Create a fraction cell in the smd transform. In this instance it
#' print the numerator followed by percentage in parenthesis.
#'
#' @param num numerator of fraction
#' @param den denominator of fraction
#' @param format formatting to apply to result
#' @param ... additional arguments to pass to cell generation
#' @return a tangram cell
#' @export
#' @importFrom stringr str_sub
smd_fraction <- function(num, den, format, ...)
{
  if(den == 0) return(cell("", ...))

  percent <- render_f(100*num/den, format)

  x <- if(str_sub(percent, 2, 2) == ".")
          paste0(num, " ( ", percent, ")") else
          paste0(num, " (",  percent, ")")

  cell(x, ...)
}

#' Create a SMD for a categorical set of column versus a numerical row
#'
#' Given a row and column object from the parser apply a Kruskal test and output
#' the results horizontally. 1 X (n + no. categories + test statistic)
#'
#' @param table The table object to modify
#' @param row The row variable object to use (numerical)
#' @param column The column variable to use (categorical)
#' @param cell_style list; cell styling functions
#' @param style character; chosen styling to final table
#' @param smdformat numeric, character or function; A formatting directive to be applied to smd
#' @param pformat numeric, character or function; A formatting directive to be applied to p-values
#' @param weight numeric; Vector of weights to apply to data when computing SMD
#' @param test logical; include statistical test results
#' @param ... absorbs additional arugments. Unused at present.
#' @return The modified table object
#' @export
#' @importFrom magrittr "%>%"
#' @include compile.R
#' @include compile-cell.R
#' @include compile-typing.R
#' @include helper-format.R
smd_compare <- function(table,
                        row,
                        column,
                        cell_style,
                        style,
                        smdformat=NULL,
                        pformat=NULL,
                        weight=NULL,
                        test=FALSE,
                        ...)
{
  datar      <- as.numeric(row$data)
  datac      <- as.categorical(column$data)
  categories <- levels(datac)
  format     <- ifelse(is.na(row$format), 2, row$format)

  if(length(categories) != 2) stop("SMD Comparison must be between exactly 2 groups")

  # Wilcox rank sum test versus zero
  tst  <- suppressWarnings(spearman2(c(datac), c(datar), na.action=na.retain))
  stat <- cell_style[['fstat']](
      f         = render_f(tst['F'], "%.2f"),
      df1       = tst['df1'],
      df2       = tst['df2'],
      p         = cell_style[['p']](tst['P'], pformat))

  cat1   <- categories[1]
  cat2   <- categories[2]
  datar1 <- datar[datac == cat1]
  datar2 <- datar[datac == cat2]

  # Compute N values for each category
  subN <- c(sum(!is.na(datar1)), sum(!is.na(datar2)))

  table <- if(test) col_header(table, "N", categories, "SMD", "Test Statistic") else
                    col_header(table, "N", categories, "SMD")
  table <- if(test)
    col_header(table, "", cell_style[['n']](subN[1], subcol=cat, hdr=TRUE),
               cell_style[['n']](subN[2], subcol=cat, hdr=TRUE), "", "")  else
    col_header(table, "", cell_style[['n']](subN[1], subcol=cat, hdr=TRUE),
               cell_style[['n']](subN[2], subcol=cat, hdr=TRUE), "")

  table <- table %>%
  row_header(derive_label(row)) %>%
  add_col(cell_style[['n']](sum(!is.na(datar)))) %>%
  add_col(cell_style[['meansd']](datar1, format, subcol=cat1)) %>%
  add_col(cell_style[['meansd']](datar2, format, subcol=cat2)) %>%
  add_col(cell_style[['smd']](datar, datac, smdformat, weight))

  if(test)
  {
    table <- add_col(table, stat)
  }

  table
}

#' Create a contingency table with SMD given a row column of a formula
#'
#' Create a contingency table with SMD given a row column of a formula
#'
#' @param table The tablebuilder object
#' @param row The row node from the parser of the formula
#' @param column The column node provided by the parser of the formula
#' @param cell_style A list of all individual cell stylings to apply
#' @param style The global style to apply.
#' @param smdformat The format command to apply to smd
#' @param pformat numeric, character or function; A formatting directive to be applied to p-values
#' @param collapse_single Should single factor variables be collapsed
#' @param weight Any weighting to apply to data for computation of SMD
#' @param test logical; include statistical test results
#' @param ... Additional arguments to provide cell generation functions
#' @return The resulting sub table constructed
#' @export
#' @importFrom stringr str_sub
smd_contingency <- function(table,
                            row,
                            column,
                            cell_style,
                            style,
                            smdformat = NULL,
                            collapse_single = TRUE,
                            weight=NULL,
                            test=FALSE,
                            pformat=NULL,
                            ...)
{
  grid          <- table(as.categorical(row$data), as.categorical(column$data), useNA="no")
  ncol          <- dim(grid)[2]
  nrow          <- dim(grid)[1]
  denominators  <- matrix(rep(colSums(grid), nrow), ncol=ncol, byrow=TRUE)
  rowlabels     <- rownames(grid)

  validcol      <- which(!apply(grid,2,FUN = function(x){all(x == 0)}))
  validrow      <- which(!apply(grid,1,FUN = function(x){all(x == 0)}))
  stat          <- if(length(validrow) < 2 || length(validcol) < 2) NA else suppressWarnings(chisq.test(grid[validrow,validcol], correct=FALSE))


  # Compute overall N values for each category
  # length(datac[datac == cat & !is.na(datac)])
  subN <- lapply(colnames(grid), FUN=function(cat)
    cell_style[['n']](sum(column$data == cat, na.rm=TRUE), subcol=cat, hdr=TRUE)
  )

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
  table <- if(test) col_header(table, "N", colnames(grid), "SMD", "Test Statistic") else
                    col_header(table, "N", colnames(grid), "SMD")
  table <- if(test) col_header(table, "", subN, "", "") else
                    col_header(table, "", subN, "")
  # Row Headers
  if(nrow > 1) table <- row_header(table, derive_label(row)) # Deal with single
  for(nm in rownames(grid)) table <- row_header(table, nm)

  # The N value
  table <- add_col(table, sum(!is.na(row$data)))

  # Now loop the grid into the table as a fraction
  for(j in 1:ncol)
  {
    if(nrow > 1) table <- add_row(table, "")
    format <- if(is.na(row$format) || is.null(row$format)) 2 else row$format
    for(i in 1:nrow)
    {
      table <- add_row(table,
                       cell_style[['fraction']](
                          grid[i,j], denominators[i,j],
                          format=format,
                          subcol=colnames(grid)[i], subrow=rownames(grid)[j]))
    }
    table <- new_col(table)
  }

  table <- add_row(table, cell_style[['smd']](
             as.categorical(row$data),
             as.categorical(column$data),
             smdformat,
             weight,
             ...))
  if(nrow > 1) table <- add_row(table, rep("", nrow))

  # Finally add the stats
  if(test)
  {
    test_result <- if(any(is.na(stat))) cell(NA) else
      cell_style[['chi2']](
        render_f(stat$statistic, 2),
        stat$parameter,
        cell_style[['p']](stat$p.value, pformat)
      )
    table <- home(table) %>% new_col() %>% add_row(test_result)
    if(nrow > 1) table <- add_row(table, rep("", nrow))
  }

  table
}

#' Cell Generation functions for SMD comparisons of categorical to numerical
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
smd_cell <- list(
  n          = cell_n,
  meansd     = smd_meansd,
  smd        = smd_dist,
  fraction   = smd_fraction,

  fstat      = hmisc_fstat,
  chi2       = hmisc_chi2,
  p          = hmisc_p
)

#'
#' List of lists, should contain a "Type" entry with a function to determine type of vector passed in.
#' Next entries are keyed off returned types from function, and represent the type of a row.
#' The returned list should contain the same list of types, and represents the type of a column. Thus it now returns
#' a function to process the intersection of those two types.
#'
#' @keywords data
#' @include transform-hmisc.R
#' @export
#'
smd <- list(
  Type        = hmisc_data_type,
  Numerical   = list(
                  Numerical   = function() stop("unsupported type comparison"),
                  Categorical = smd_compare
            ),
  Categorical = list(
                  Numerical   = function() stop("unsupported type comparison"),
                  Categorical = smd_contingency
            ),
  Cell        = smd_cell,
  Footnote    = "Numerical summary is mean (sd). Categorical is N(%). ^1^Kruskal-Wallis. ^2^Pearson."
)


